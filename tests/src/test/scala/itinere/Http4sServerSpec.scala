package itinere

import cats.Show
import cats.effect.{IO, Sync}
import cats.implicits._
import eu.timepit.refined._
import io.circe.literal._
import io.circe.Error
import itinere.circe.CirceJsonLike
import itinere.http4s_server.{HeaderDecodeFailure, Http4sServer, Http4sServerJson, UriDecodeFailure}
import org.http4s._
import org.http4s.circe._
import org.http4s.implicits._
import org.specs2.matcher.{IOMatchers, Matcher, Matchers}
import shapeless._

class Http4sServerSpec extends org.specs2.mutable.Specification with IOMatchers with Matchers {

  "Server" >> {
    "register" >> {
      "return http 200 ok" >> {
        val resp = serve(post(Uri.uri("/users/register"), json"""{"name": "Mark", "age": 12 }"""))

        resp must returnStatus(Status.Ok)
        resp.as[io.circe.Json] must returnValue(json""""Mark"""")
      }

      "return http 400 bad_request when .age < 0" >> {
        val resp = serve(post(Uri.uri("/users/register"), json"""{"name": "Mark", "age": -1 }"""))

        resp must returnStatus(Status.BadRequest)
        resp.as[String] must returnValue("DecodingFailure at .age: Left predicate of ((-1 > 0) && (-1 < 150)) failed: Predicate failed: (-1 > 0).")
      }
    }

    "list" >> {
      "return http 200 ok" >> {
        val resp = serve(get(Uri.uri("/users?ageGreater=1")))

        resp must returnStatus(Status.Ok)
        resp.as[io.circe.Json] must returnValue(json"""[]""")
      }

      "return http 400 bad_request when ageGreater is -1" >> {
        val resp = serve(get(Uri.uri("/users?ageGreater=-1")))

        resp must returnStatus(Status.BadRequest)
        resp.as[String] must returnValue("Failed to decode query string ageGreater : Predicate failed: (-1 > 0).")
      }
    }

    "get" >> {
      "return http 200 ok" >> {
        val resp = serve(get(Uri.uri("/users/1"), Header("X-Token", "23") :: Header("X-ValidTill", "1234") :: Nil))

        resp must returnStatus(Status.Ok)
        resp.as[io.circe.Json] must returnValue(json"""{"value": {"id": 1, "name": "Klaas", "age": 3}}""")
      }

      "return http 404 not_found when non existing id is given" >> {
        val resp = serve(get(Uri.uri("/users/2"), Header("X-Token", "23") :: Header("X-ValidTill", "1234") :: Nil))

        resp must returnStatus(Status.NotFound)
        resp.as[io.circe.Json] must returnValue(json"""{"error": "User was not found"}""")
      }

      "return http 400 bad_request when segment userId is -1" >> {
        val resp = serve(get(Uri.uri("/users/-1"), Header("X-Token", "23") :: Header("X-ValidTill", "1234") :: Nil))

        resp must returnStatus(Status.BadRequest)
        resp.as[String] must returnValue("Failed to decode segment userId : Predicate failed: (-1 > 0).")
      }

      "return http 400 bad_request when X-Token is not a int" >> {
        val resp = serve(get(Uri.uri("/users/2"), Header("X-Token", "this-is-astring") :: Nil))

        resp must returnStatus(Status.BadRequest)
        resp.as[String] must returnValue("Failed to decode header 'X-Token' : For input string: \"this-is-astring\"")
      }

      "return http 400 bad_request when no header is given" >> {
        val resp = serve(get(Uri.uri("/users/2")))

        resp must returnStatus(Status.BadRequest)
        resp.as[String] must returnValue("Required header 'X-Token' not present.")
      }
    }

    "delete" >> {
      "return http 200 ok" >> {
        val resp = serve(delete(Uri.uri("/users/1/red")))

        resp must returnStatus(Status.Ok)
        resp.as[String] must returnValue(""""Deleted user 1"""")
      }

      "return http 400 bad_request when invalid is given" >> {
        val resp = serve(delete(Uri.uri("/users/1/purple")))

        resp must returnStatus(Status.BadRequest)
        resp.as[String] must returnValue("Failed to decode segment color")
      }
    }

    "composing multiple HttpRoutes via Router should work" >> {
      val router = AppService.routes <+> Server.routes

      val serverResponse = serve(get(Uri.uri("/users/1"), Header("X-Token", "23") :: Header("X-ValidTill", "1234") :: Nil), router)
      serverResponse must returnStatus(Status.Ok)
      serverResponse.as[io.circe.Json] must returnValue(json"""{"value": {"id": 1, "name": "Klaas", "age": 3}}""")

      val statusResponse = serve(get(Uri.uri("/status")), router)
      statusResponse must returnStatus(Status.Ok)
      statusResponse.as[String] must returnValue("Alive!")
    }
  }

  def returnStatus(status: Status): Matcher[Response[IO]] = { s: Response[IO] =>
    s.status must beEqualTo(status)
  }

  private def serve(request: IO[Request[IO]], routes: HttpRoutes[IO] = Server.routes): Response[IO] =
    request.flatMap(routes.orNotFound.handleError(errorHandler).run).unsafeRunSync()

  private def post[A](uri: Uri, body: A)(implicit E: EntityEncoder[IO, A]): IO[Request[IO]] =
    IO.pure(Request(Method.POST, uri, body = E.toEntity(body).body))

  private def get[A](uri: Uri, headers: List[Header] = Nil): IO[Request[IO]] =
    IO.pure(Request(Method.GET, uri, headers = Headers(headers)))

  private def delete[A](uri: Uri, headers: List[Header] = Nil): IO[Request[IO]] =
    IO.pure(Request(Method.DELETE, uri, headers = Headers(headers)))

  private def errorHandler(error: Throwable): Response[IO] = error match {
    case u: UriDecodeFailure =>
      Response(Status.BadRequest).withEntity(u.message)
    case u: HeaderDecodeFailure =>
      Response(Status.BadRequest).withEntity(u.message)
    case MalformedMessageBodyFailure(_, Some(err: Error)) =>
      Response(Status.BadRequest).withEntity(Show[Error].show(err))
    case _ =>
      Response(Status.InternalServerError)
  }
}

object AppService {
  import org.http4s.dsl.io._

  val routes: HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case GET -> Root / "status" =>
        Ok("Alive!")
    }
  }
}

object Server extends Http4sServer with Endpoints with Http4sServerJson with CirceJsonLike {
  override type F[A] = IO[A]
  override def F: Sync[IO] = Sync[IO]

  val routes =
  userRegister.implementedBy(r => IO.pure(r.name.value)) <+>
  userList.implementedBy(_ => IO.pure(List.empty)) <+>
  userDelete.implementedBy { case uid :: _ :: _ => IO.pure(s"Deleted user ${uid.value}") } <+>
  userGet.implementedBy { case uid :: _ :: _    => IO.pure(if (uid.value == 1l) DomainResponse.Success(User(uid, "Klaas", refineMV(3))) else DomainResponse.NotFound("User was not found")) }

}

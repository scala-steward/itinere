package itinere

import cats.Show
import cats.effect.concurrent.Ref
import cats.effect.{IO, Sync}
import cats.implicits._
import cats.kernel.{Monoid}
import eu.timepit.refined._
import io.circe.literal._
import io.circe.Error
import itinere.circe.CirceJsonLike
import itinere.http4s_server._
import org.http4s._
import org.http4s.circe._
import org.http4s.implicits._
import org.specs2.matcher.{IOMatchers, Matcher, Matchers}
import shapeless._

import scala.concurrent.duration.FiniteDuration

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

      "return http 500 internal_server_error when invalid color is given" >> {
        val resp = serve(delete(Uri.uri("/users/1/green")))

        resp must returnStatus(Status.InternalServerError)
        resp.as[String] must returnValue("Internal error occurred")

        ref.get.unsafeRunSync().get("users/:userId/:color") must beSome(EndpointStats(Map(200 -> 1, 500 -> 1)))
      }
    }

    "composing multiple HttpRoutes via Router should work" >> {
      val router = AppService.routes <+> server.routes

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

  private def serve(request: IO[Request[IO]], routes: HttpRoutes[IO] = server.routes): Response[IO] =
    request.flatMap(routes.orNotFound.run(_).handleErrorWith(server.errorHandler)).unsafeRunSync()

  private def post[A](uri: Uri, body: A)(implicit E: EntityEncoder[IO, A]): IO[Request[IO]] =
    IO.pure(Request(Method.POST, uri, body = E.toEntity(body).body))

  private def get[A](uri: Uri, headers: List[Header] = Nil): IO[Request[IO]] =
    IO.pure(Request(Method.GET, uri, headers = Headers(headers)))

  private def delete[A](uri: Uri, headers: List[Header] = Nil): IO[Request[IO]] =
    IO.pure(Request(Method.DELETE, uri, headers = Headers(headers)))

  lazy val ref = Ref.of[IO, Map[String, EndpointStats]](Map.empty).unsafeRunSync()
  lazy val server = new TestServer(ref)
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

case class EndpointStats(hits: Map[Int, Int])

object EndpointStats {

  implicit val monoid: Monoid[EndpointStats] = new Monoid[EndpointStats] {
    override def empty: EndpointStats = EndpointStats(Map.empty)
    override def combine(x: EndpointStats, y: EndpointStats): EndpointStats =
      EndpointStats(Monoid[Map[Int, Int]].combine(x.hits, y.hits))
  }
}

class TestServer(measurements: Ref[IO, Map[String, EndpointStats]]) extends Http4sServer with Endpoints with Http4sServerJson with CirceJsonLike {
  override type F[A] = IO[A]
  override def F: Sync[IO] = Sync[IO]

  override def measurementHandler(requestLatency: RequestMessage[FiniteDuration], responseStatusCode: Int): IO[Unit] =
    measurements.update(_ |+| Map(requestLatency.uri -> EndpointStats(Map(responseStatusCode -> 1))))

  override def errorHandler(error: Throwable): IO[Response[IO]] = error match {
    case u: UriDecodeFailure =>
      IO.pure(Response(Status.BadRequest).withEntity(u.message))
    case u: HeaderDecodeFailure =>
      IO.pure(Response(Status.BadRequest).withEntity(u.message))
    case MalformedMessageBodyFailure(_, Some(err: Error)) =>
      IO.pure(Response(Status.BadRequest).withEntity(Show[Error].show(err)))
    case err =>
      super.errorHandler(err)
  }

  //these endpoint return gibberish output, but it's sufficient for testing
  val routes =
  userRegister.implementedBy(r => IO.pure(r.name.value)) <+>
  userList.implementedBy(_ => IO.pure(List.empty)) <+>
  userDelete.implementedBy { case uid :: c :: _ => if (c == Color.Red) IO.pure(s"Deleted user ${uid.value}") else IO.raiseError(new Throwable("Only the color red can be deleted!")) } <+>
  userGet.implementedBy { case uid :: _ :: _    => IO.pure(if (uid.value == 1l) DomainResponse.Success(User(uid, "Klaas", refineMV(3))) else DomainResponse.NotFound("User was not found")) }

}

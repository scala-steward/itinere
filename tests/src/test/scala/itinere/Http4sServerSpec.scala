package itinere

import cats.Show
import cats.effect.{IO, Sync}
import cats.implicits._
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.{PosInt, PosLong}
import io.circe.Error
import io.circe.literal._
import itinere.circe.CirceJsonLike
import itinere.http4s_server.{Http4sServer, Http4sServerJson}
import itinere.refined._
import org.http4s._
import org.http4s.circe._
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

      "return http 400 bad_request" >> {
        val resp = serve(post(Uri.uri("/users/register"), json"""{"name": "Mark", "age": -1 }"""))

        resp must returnStatus(Status.BadRequest)
        resp.as[String] must returnValue("DecodingFailure at .age: Predicate failed: (-1 > 0).")
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
        val resp = serve(delete(Uri.uri("/users/1")))

        resp must returnStatus(Status.Ok)
        resp.as[String] must returnValue("")
      }
    }
  }

  def returnStatus(status: Status): Matcher[Response[IO]] = { s: Response[IO] =>
    s.status must beEqualTo(status)
  }

  private def serve(request: IO[Request[IO]]): Response[IO] =
    request.flatMap(Server.router.run).unsafeRunSync()

  private def post[A](uri: Uri, body: A)(implicit E: EntityEncoder[IO, A]): IO[Request[IO]] =
    IO.pure(Request(Method.POST, uri, body = E.toEntity(body).body))

  private def get[A](uri: Uri, headers: List[Header] = Nil): IO[Request[IO]] =
    IO.pure(Request(Method.GET, uri, headers = Headers(headers)))

  private def delete[A](uri: Uri, headers: List[Header] = Nil): IO[Request[IO]] =
    IO.pure(Request(Method.DELETE, uri, headers = Headers(headers)))
}

trait Endpoints extends HttpEndpointAlgebra with HttpJsonAlgebra with RefinedPrimitives {

  val userId: Path[Refined[Long, Positive]] = segment("userId", _.long.refined[Positive])
  val authInfo: HttpRequestHeaders[AuthInfo] = (requestHeader("X-Token", _.int.refined[Positive]) ~ requestHeader("X-ValidTill", _.long.refined[Positive])).as[AuthInfo]

  def domainResponse[A](value: Json[A]): HttpResponse[DomainResponse[A]] =
    coproductResponseBuilder
      .add(response(HttpStatus.Ok, entity = jsonResponse(DomainResponse.success(value))))
      .add(response(HttpStatus.NotFound, entity = jsonResponse(DomainResponse.notFound)))
      .add(response(HttpStatus.BadRequest, entity = jsonResponse(DomainResponse.badRequest)))
      .as[DomainResponse[A]]

  val userRegister =
  POST(path / "users" / "register", entity = jsonRequest(RegisterUser.json)) ~>
  response(HttpStatus.Ok, entity = jsonResponse(Json.string))

  val userList =
  GET(path / "users" /? (qs("ageGreater", _.int.refined[Positive]) & qs[String]("nameStartsWith", _.string)).as[ListFilter]) ~>
  response(HttpStatus.Ok, entity = jsonResponse(Json.list(User.json)))

  val userGet =
  GET(path / "users" / userId, authInfo) ~>
  domainResponse(User.json)

  val userDelete =
  DELETE(path / "users" / userId) ~> response(HttpStatus.Ok)

}

final case class AuthInfo(
  token: PosInt,
  validTill: PosLong
)

final case class ListFilter(
  ageGreater: Option[PosInt],
  nameStartsWith: Option[String]
)

final case class RegisterUser(
  name: String,
  age: PosInt
)

final case class User(
  id: PosLong,
  name: String,
  age: PosInt
)
object User {
  val json: Json[User] = Json.object3("User")(User.apply)(
    "id"   -> member(Json.long.positive, _.id),
    "name" -> member(Json.string, _.name),
    "age"  -> member(Json.int.positive, _.age)
  )
}

object RegisterUser {
  val json: Json[RegisterUser] = Json.object2("RegisterUser")(RegisterUser.apply)(
    "name" -> member(Json.string, _.name),
    "age"  -> member(Json.int.positive, _.age)
  )
}

sealed trait DomainResponse[+A]
object DomainResponse {
  final case class Success[A](value: A) extends DomainResponse[A]
  final case class BadRequest(error: String) extends DomainResponse[Nothing]
  final case class NotFound(error: String) extends DomainResponse[Nothing]

  def success[A](value: Json[A]): Json[Success[A]] = Json.object1("Success")(Success[A](_))("value" -> member(value, _.value))
  val badRequest: Json[BadRequest] = Json.object1("BadRequest")(BadRequest.apply)("error"           -> member(Json.string, _.error))
  val notFound: Json[NotFound] = Json.object1("NotFound")(NotFound.apply)("error"                   -> member(Json.string, _.error))
}

object Server extends Http4sServer with Endpoints with Http4sServerJson with CirceJsonLike {
  override type F[A] = IO[A]
  override def F: Sync[IO] = Sync[IO]

  override def errorHandler(error: Throwable): Response[IO] = error match {
    case MalformedMessageBodyFailure(_, Some(err: Error)) =>
      Response(Status.BadRequest).withEntity(Show[Error].show(err))
    case err =>
      super.errorHandler(err)
  }

  val handlers =
  userRegister.implementedBy(r => IO.pure(r.name)) <+>
  userList.implementedBy(_ => IO.pure(List.empty)) <+>
  userDelete.implementedBy(_ => IO.pure(HNil)) <+>
  userGet.implementedBy { case uid :: _ :: _ => IO.pure(if (uid.value == 1l) DomainResponse.Success(User(uid, "Klaas", refineMV(3))) else DomainResponse.NotFound("User was not found")) }

}

package itinere

import cats.implicits._
import cats.Show
import cats.effect.{IO, Sync}
import eu.timepit.refined._
import eu.timepit.refined.types.numeric.{PosInt, PosLong}

import io.circe.Error
import io.circe.literal._
import itinere.circe.CirceJsonLike
import itinere.http4s_server.{Http4sServer, Http4sServerJson, UriDecodeException}
import itinere.refined._
import org.http4s._
import org.http4s.circe._
import org.specs2.matcher.{IOMatchers, Matcher, Matchers}
import shapeless._

class Http4sServerSpec extends org.specs2.mutable.Specification with IOMatchers with Matchers {


  "Server" >> {
    "register" >> {
      "return http 200 ok" >> {
        val resp = serve(
          post(
            Uri.uri("/users/register"),
            json"""{"name": "Mark", "age": 12 }"""
          )
        )

        resp must returnStatus(Status.Ok)
        resp.as[io.circe.Json] must returnValue(json""""Mark"""")
      }

      "return http 400 bad_request" >> {
        val resp = serve(
          post(
            Uri.uri("/users/register"),
            json"""{"name": "Mark", "age": -1 }"""
          )
        )

        resp must returnStatus(Status.BadRequest)
        resp.as[String] must returnValue("DecodingFailure at .age: Predicate failed: (-1 > 0).")
      }
    }

    "list" >> {
      "return http 200 ok" >> {
        val resp = serve(
          get(Uri.uri("/users?ageGreater=1"))
        )

        resp must returnStatus(Status.Ok)
        resp.as[io.circe.Json] must returnValue(json"""[]""")
      }

      "return http 400 bad_request when ageGreater is -1" >> {
        val resp = serve(
          get(Uri.uri("/users?ageGreater=-1"))
        )

        resp must returnStatus(Status.BadRequest)
        resp.as[String] must returnValue("Failed to decode query string ageGreater (Predicate failed: (-1 > 0).)")
      }
    }

    "get" >> {
      "return http 200 ok" >> {
        val resp = serve(
          get(Uri.uri("/users/1"))
        )

        resp must returnStatus(Status.Ok)
        resp.as[io.circe.Json] must returnValue(json"""{"value": {"id": 1, "name": "Klaas", "age": 3}}""")
      }

      "return http 400 bad_request when segment userId is -1" >> {
        val resp = serve(
          get(Uri.uri("/users/-1"))
        )

        resp must returnStatus(Status.BadRequest)
        resp.as[String] must returnValue("Failed to decode segment userId (Predicate failed: (-1 > 0).)")
      }

      "return http 404 not_found" >> {
        val resp = serve(
          get(Uri.uri("/users/2"))
        )

        resp must returnStatus(Status.NotFound)
        resp.as[io.circe.Json] must returnValue(json"""{"error": "User was not found"}""")
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

  private def get[A](uri: Uri): IO[Request[IO]] =
    IO.pure(Request(Method.GET, uri))
}



trait Endpoints extends HttpEndpointAlgebra with HttpJsonAlgebra with RefinedPrimitives {

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
    GET(path / "users" /? (qs[PosInt]("ageGreater") & qs[String]("nameStartsWith")).as[ListFilter]) ~>
      response(HttpStatus.Ok, entity = jsonResponse(Json.list(User.json)))

  val userGet =
    GET(path / "users" / segment[PosLong]("userId") /? (qs[PosInt]("ageGreater") & qs[String]("nameStartsWith")).as[ListFilter]) ~>
      domainResponse(User.json)

}

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
    "id" -> member(Json.long.positive, _.id),
    "name" -> member(Json.string, _.name),
    "age" -> member(Json.int.positive, _.age)
  )
}

object RegisterUser {
  val json: Json[RegisterUser] = Json.object2("RegisterUser")(RegisterUser.apply)(
    "name" -> member(Json.string, _.name),
    "age" -> member(Json.int.positive, _.age)
  )
}

sealed trait DomainResponse[+A]
object DomainResponse {
  final case class Success[A](value: A) extends DomainResponse[A]
  final case class BadRequest(error: String) extends DomainResponse[Nothing]
  final case class NotFound(error: String) extends DomainResponse[Nothing]

  def success[A](value: Json[A]): Json[Success[A]] = Json.object1("Success")(Success[A](_))("value" -> member(value, _.value))
  val badRequest: Json[BadRequest] = Json.object1("BadRequest")(BadRequest.apply)("error" -> member(Json.string, _.error))
  val notFound: Json[NotFound] = Json.object1("NotFound")(NotFound.apply)("error" -> member(Json.string, _.error))
}

object Server extends Http4sServer with Endpoints with Http4sServerJson with CirceJsonLike {
  override type F[A] = IO[A]
  override def F: Sync[IO] = Sync[IO]

  override def errorHandler(error: Throwable): Response[IO] = error match {
    case u : UriDecodeException =>
      Response(Status.BadRequest).withEntity(u.message)
    case MalformedMessageBodyFailure(_, Some(err: Error)) =>
      Response(Status.BadRequest).withEntity(Show[Error].show(err))
    case InvalidMessageBodyFailure(_, Some(err: Error)) =>
      Response(Status.BadRequest).withEntity(Show[Error].show(err))
    case _ =>
      Response(Status.InternalServerError).withEntity("Internal server error")
  }


  val handlers =
    userRegister.implementedBy(r => IO.pure(r.name)) <+>
    userList.implementedBy(_ => IO.pure(List.empty)) <+>
    userGet.implementedBy { case userId :: _ :: _ => IO.pure(if(userId.value == 1l) DomainResponse.Success(User(userId, "Klaas", refineMV(3))) else DomainResponse.NotFound("User was not found")) }

}
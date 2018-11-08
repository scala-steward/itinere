package itinere
import cats.Show
import cats.effect.{IO, Sync}
import cats.implicits._
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import itinere.circe.CirceJsonLike
import itinere.http4s_server.{Http4sServer, Http4sServerJson, UriDecodeException}
import itinere.refined._
import org.http4s._
import org.http4s.circe._
import org.specs2.matcher.{IOMatchers, Matcher, Matchers}
import shapeless._
import io.circe.literal._
import io.circe.Error

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
        resp.as[io.circe.Json] must returnValue(json"""{"message":"Mark"}""")
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



trait Endpoints extends HttpEndpointAlgebra with HttpJsonAlgebra {

  def domainResponse[A](value: Json[A]): HttpResponse[DomainResponse[A]] =
    coproductResponseBuilder
      .add(response(200, entity = jsonResponse(DomainResponse.success(value))))
      .add(response(404, entity = jsonResponse(DomainResponse.notFound)))
      .add(response(400, entity = jsonResponse(DomainResponse.badRequest)))
      .as[DomainResponse[A]]

  val userRegister =
    endpoint(
      request(POST, path / "users" / "register", entity = jsonRequest(RegisterUser.json)),
      response(200, entity = jsonResponse(Result.json))
    )


  val userList =
    endpoint(
      request(GET, path / "users" /? (qs("ageGreater", Read.int.refined[Positive]) & qs("nameStartsWith", Read.string)).as[ListFilter]),
      response(200, entity = jsonResponse(list(User.json)))
    )

  val userGet =
    endpoint(
      request(GET, path / "users" / segment("userId", Read.long.refined[Positive]) /? (qs("ageGreater", Read.int.refined[Positive]) & qs("nameStartsWith", Read.string)).as[ListFilter]),
      domainResponse(User.json)
    )

}

final case class ListFilter(
  ageGreater: Option[Int Refined Positive],
  nameStartsWith: Option[String]
)

final case class Result(message: String)
object Result {
  val json: Json[Result] = object1("Result")(Result.apply)(
    "message" -> member(string, _.message)
  )
}

final case class RegisterUser(
   name: String,
   age: Int Refined Positive
)

final case class User(
  id: Long Refined Positive,
  name: String,
  age: Int Refined Positive
)
object User {
  val json: Json[User] = object3("User")(User.apply)(
    "id" -> member(long.positive, _.id),
    "name" -> member(string, _.name),
    "age" -> member(int.positive, _.age)
  )
}

object RegisterUser {
  val json: Json[RegisterUser] = object2("RegisterUser")(RegisterUser.apply)(
    "name" -> member(string, _.name),
    "age" -> member(int.positive, _.age)
  )
}

sealed trait DomainResponse[+A]
object DomainResponse {
  final case class Success[A](value: A) extends DomainResponse[A]
  final case class BadRequest(error: String) extends DomainResponse[Nothing]
  final case class NotFound(error: String) extends DomainResponse[Nothing]

  def success[A](value: Json[A]): Json[Success[A]] = object1("Success")(Success[A](_))("value" -> member(value, _.value))
  val badRequest: Json[BadRequest] = object1("BadRequest")(BadRequest.apply)("error" -> member(string, _.error))
  val notFound: Json[NotFound] = object1("NotFound")(NotFound.apply)("error" -> member(string, _.error))
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
    userRegister.implementedBy(r => IO.pure(Result(r.name))) <+>
    userList.implementedBy(_ => IO.pure(List.empty)) <+>
    userGet.implementedBy { case userId :: _ :: _ => IO.pure(if(userId.value == 1l) DomainResponse.Success(User(userId, "Klaas", refineMV(3))) else DomainResponse.NotFound("User was not found")) }

}
package itinere
import cats.effect.{IO, Sync}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import itinere.circe.CirceJsonLike
import itinere.http4s_server.{Http4sServer, Http4sServerJson}
import itinere.refined._
import org.http4s._
import org.http4s.circe._
import org.http4s.implicits._
import org.specs2.matcher.{IOMatchers, Matcher, Matchers}
import shapeless._
import io.circe.literal._

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
    }

    "list" >> {
      "return http 200 ok" >> {
        val resp = serve(
          get(Uri.uri("/users?ageGreater=1"))
        )

        resp must returnStatus(Status.Ok)
        resp.as[io.circe.Json] must returnValue(json"""[]""")
      }
    }
  }


  def returnStatus(status: Status): Matcher[Response[IO]] = { s: Response[IO] =>
    s.status must beEqualTo(status)
  }

  private def serve(request: IO[Request[IO]]): Response[IO] =
    request.flatMap(HttpRoutes.of(Server.handlers).orNotFound.run).unsafeRunSync()

  private def post[A](uri: Uri, body: A)(implicit E: EntityEncoder[IO, A]): IO[Request[IO]] =
    IO.pure(Request(Method.POST, uri, body = E.toEntity(body).body))

  private def get[A](uri: Uri): IO[Request[IO]] =
    IO.pure(Request(Method.GET, uri))
}



trait Endpoints extends HttpEndpointAlgebra with HttpJsonAlgebra {



  val userRegister =
    endpoint(
      request(POST, path / "users" / "register", entity = jsonRequest(RegisterUser.json)),
      response(200, entity = jsonResponse(Result.json))
    )


  val userList =
    endpoint(
      request(GET, path / "users" /? (optQs("ageGreater", Read.int.refined[Positive]) & optQs("nameStartsWith", Read.string)).as[ListFilter]),
      response(200, entity = jsonResponse(list(User.json)))
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
  id: Long,
  name: String,
  age: Int Refined Positive
)
object User {
  val json: Json[User] = object3("User")(User.apply)(
    "id" -> member(long, _.id),
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

object Server extends Http4sServer with Endpoints with Http4sServerJson with CirceJsonLike {
  override type F[A] = IO[A]

  override def F: Sync[IO] = Sync[IO]

  val handlers =
    userRegister.implementedBy(r => IO.pure(Result(r.name))) orElse
    userList.implementedBy(filter => IO.pure(List.empty))

}
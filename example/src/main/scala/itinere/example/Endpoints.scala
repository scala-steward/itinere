package itinere.example

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import itinere._
import itinere.circe.CirceJsonLike
import itinere.http4s_server.{Http4sServer, Http4sServerJson}
import itinere.refined._
import org.http4s.HttpRoutes
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import shapeless._

trait Endpoints extends HttpEndpointAlgebra with HttpJsonAlgebra {



  val register =
    endpoint(request(POST, path / "users" / "register", entity = jsonRequest(RegisterUser.json)), response(200, entity = jsonResponse(Result.json)))

}

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


object RegisterUser {
  val json: Json[RegisterUser] = object2("RegisterUser")(RegisterUser.apply)(
    "name" -> member(string, _.name),
    "age" -> member(int.positive, _.age)
  )
}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(HttpRoutes.of(Server.handlers).orNotFound)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}

object Server extends Http4sServer with Endpoints with Http4sServerJson with CirceJsonLike {
  override type F[A] = IO[A]

  override def F: Sync[IO] = Sync[IO]

  val handlers = register.implementedBy(r => IO.pure(Result(r.head.name) :: HNil))

}
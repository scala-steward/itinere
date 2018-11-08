package itinere.http4s_server


import cats.effect.Sync
import cats.implicits._
import itinere.HttpEndpointAlgebra
import org.http4s.{Request => Req, Response => Resp}

abstract class Http4sServer
  extends HttpEndpointAlgebra
    with Http4sServerResponse
    with Http4sServerRequest
{

  type F[A]

  implicit def F: Sync[F]

  case class Endpoint[A, B](request: Request[A], response: Response[B]) {
    def implementedBy(implementation: A => F[B]): PartialFunction[Req[F], F[Resp[F]]] =
      request.andThen(_.flatMap(implementation).map(response.apply))
  }

  override def endpoint[A, B](request: PartialFunction[Req[F], F[A]], response: B => Resp[F], description: Option[String]): Endpoint[A, B] = {
    Endpoint(request, response)
  }

  val handlers: PartialFunction[Req[F], F[Resp[F]]]

}

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

  case class HttpEndpoint[A, B](request: HttpRequest[A], response: HttpResponse[B]) {
    def implementedBy(implementation: A => F[B]): PartialFunction[Req[F], F[Resp[F]]] =
      request.andThen(_.flatMap(implementation).map(response.apply))
  }

  override def endpoint[A, B](request: PartialFunction[Req[F], F[A]], response: B => Resp[F], description: Option[String]): HttpEndpoint[A, B] = {
    HttpEndpoint(request, response)
  }

  val handlers: PartialFunction[Req[F], F[Resp[F]]]

}

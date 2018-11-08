package itinere.http4s_server


import cats.data.{Kleisli, OptionT}
import cats.effect.Sync
import cats.implicits._
import itinere.HttpEndpointAlgebra
import org.http4s.implicits._
import org.http4s.{HttpRoutes, MalformedMessageBodyFailure, Status, Request => Req, Response => Resp}

abstract class Http4sServer
  extends HttpEndpointAlgebra
    with Http4sServerResponse
    with Http4sServerRequest
{

  type F[A]

  implicit def F: Sync[F]

  case class HttpEndpoint[A, B](request: HttpRequest[A], response: HttpResponse[B]) {
    def implementedBy(implementation: A => F[B]): HttpRoutes[F] =
      Kleisli(req => request(req).flatMap(a => OptionT.liftF(implementation(a))).map(response.apply))
  }

  override def endpoint[A, B](request: Req[F] => OptionT[F, A], response: B => Resp[F], description: Option[String]): HttpEndpoint[A, B] = {
    HttpEndpoint(request, response)
  }

  def errorHandler(error: Throwable): Resp[F] = error match {
    case MalformedMessageBodyFailure(err, _) => Resp(Status.BadRequest).withEntity(err)
    case err =>
      Resp(Status.InternalServerError).withEntity("Internal server error")
  }

  val handlers: HttpRoutes[F]

  def router = handlers.orNotFound.handleError(errorHandler)

//  private implicit class KleisliResponseOps[F[_]: Functor, A](self: Kleisli[OptionT[F, ?], A, Resp[F]]) {
//    def handleError: Kleisli[F, A, Resp[F]] =
//      Kleisli(a => F.handleError(self.run(a))(errorHandler))
//  }


}

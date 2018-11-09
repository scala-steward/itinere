package itinere.http4s_server


import java.util.concurrent.TimeUnit

import cats.data.{Kleisli, OptionT}
import cats.effect.Sync
import cats.implicits._
import itinere.{HttpEndpointAlgebra, ReadPrimitives}
import org.http4s.implicits._
import org.http4s.{HttpRoutes, MalformedMessageBodyFailure, Status, Request => Req, Response => Resp}

import scala.concurrent.duration.FiniteDuration

abstract class Http4sServer
  extends HttpEndpointAlgebra
    with Http4sServerResponse
    with Http4sServerRequest
    with ReadPrimitives
{

  type F[A]

  implicit def F: Sync[F]

  final case class RequestMessage[A](method: HttpMethod, uri: String, value: A)

  final case class HttpEndpoint[A, B](request: HttpRequest[A], response: HttpResponse[B]) {
    def implementedBy(implementation: A => F[B]): HttpRoutes[F] =
      Kleisli(req => request(req).flatMap(a => OptionT.liftF(measureLatency(a, implementation))).map(response.apply))
  }

  private val currentTimeMillis = F.delay(System.currentTimeMillis())

  private def measureLatency[A, B](message: RequestMessage[A], handler: A => F[B]): F[B] =
    for {
      start <- currentTimeMillis
      res <- handler(message.value)
      end <- currentTimeMillis
      _ <- measurementHandler(
        RequestMessage(message.method, message.uri, FiniteDuration(end - start, TimeUnit.MILLISECONDS)))
    } yield res

  final def endpoint[A, B](request: Req[F] => OptionT[F, RequestMessage[A]], response: B => Resp[F], description: Option[String]): HttpEndpoint[A, B] =
    HttpEndpoint(request, response)

  def measurementHandler(message: RequestMessage[FiniteDuration]): F[Unit] = F.unit

  def errorHandler(error: Throwable): Resp[F] = error match {
    case MalformedMessageBodyFailure(err, _) => Resp(Status.BadRequest).withEntity(err)
    case err =>
      Resp(Status.InternalServerError).withEntity("Internal server error")
  }

  val handlers: HttpRoutes[F]

  final def router = handlers.orNotFound.handleError(errorHandler)

}

package itinere.http4s_server

import java.util.concurrent.TimeUnit

import cats.Show
import cats.data.{Kleisli, OptionT}
import cats.effect.Sync
import cats.implicits._
import com.github.ghik.silencer.silent
import itinere.HttpEndpointAlgebra
import org.http4s.{HttpRoutes, Status, Response => Resp}

import scala.concurrent.duration.FiniteDuration

abstract class Http4sServer extends HttpEndpointAlgebra with Http4sServerResponse with Http4sServerRequest {

  type F[A]

  implicit def F: Sync[F]

  @silent
  final case class HttpEndpoint[A, B](request: HttpRequest[A], response: HttpResponse[B]) {
    def implementedBy(implementation: A => F[B]): HttpRoutes[F] =
      Kleisli(req => request(req).flatMap(a => OptionT.liftF(measureLatency(a, implementation, response))))
  }

  private val currentTimeMillis = F.delay(System.currentTimeMillis())

  private def measureLatency[A, B](message: RequestMessage[A], handler: A => F[B], responder: HttpResponse[B]): F[Resp[F]] =
    for {
      start <- currentTimeMillis
      resp  <- F.handleErrorWith(handler(message.value).map(responder))(errorHandler)
      end   <- currentTimeMillis
      _     <- measurementHandler(RequestMessage(message.method, message.uri, FiniteDuration(end - start, TimeUnit.MILLISECONDS)), resp.status.code)
    } yield resp

  final def endpoint[A, B, C: Show](request: HttpRequest[A], response: HttpResponse[B], tag: C, summary: String): HttpEndpoint[A, B] =
    HttpEndpoint(request, response)

  @silent
  def measurementHandler(requestLatency: RequestMessage[FiniteDuration], responseStatusCode: Int): F[Unit] = F.unit

  @silent
  def errorHandler(error: Throwable): F[Resp[F]] = F.pure(Resp(Status.InternalServerError).withEntity("Internal error occurred"))

  val routes: HttpRoutes[F]

}

package itinere.http4s_server

import java.util.concurrent.TimeUnit

import cats.data.{Kleisli, OptionT}
import cats.effect.Sync
import cats.implicits._
import com.github.ghik.silencer.silent
import itinere.HttpEndpointAlgebra
import org.http4s.{HttpRoutes, Response => Resp}

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
      res   <- handler(message.value)
      end   <- currentTimeMillis
      resp = responder(res)
      _ <- measurementHandler(RequestMessage(message.method, message.uri, FiniteDuration(end - start, TimeUnit.MILLISECONDS)), resp.status.code)
    } yield resp

  final def endpoint[A, B](request: HttpRequest[A], response: HttpResponse[B], description: Option[String]): HttpEndpoint[A, B] =
    HttpEndpoint(request, response)

  @silent
  def measurementHandler(requestLatency: RequestMessage[FiniteDuration], responseStatusCode: Int): F[Unit] = F.unit

  val routes: HttpRoutes[F]

}

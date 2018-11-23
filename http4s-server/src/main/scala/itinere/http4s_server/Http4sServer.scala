package itinere.http4s_server

import java.util.concurrent.TimeUnit

import cats.Show
import cats.data.{Kleisli, OptionT}
import cats.effect.Effect
import cats.implicits._
import com.github.ghik.silencer.silent
import itinere.HttpEndpointAlgebra
import org.http4s.{HttpRoutes, Status, Response => Resp}

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

abstract class Http4sServer extends HttpEndpointAlgebra with Http4sServerResponse with Http4sServerRequest {

  type F[A]

  implicit def F: Effect[F]

  implicit val ErrorHandler: Http4sError[F] = Http4sError.default

  private val currentTimeMillis = F.delay(System.currentTimeMillis())

  @silent
  final case class HttpEndpoint[A, B](request: HttpRequest[A], response: HttpResponse[B]) {

    private def measureLatency(message: RequestMessage[Either[Resp[F], A]], handler: A => F[B], responder: HttpResponse[B]): F[Resp[F]] =
      message match {
        case RequestMessage(endpoint, Left(resp)) =>
          measurementHandler(endpoint, resp.status.code, None) *> F.pure(resp)
        case RequestMessage(endpoint, Right(a)) =>
          for {
            start <- currentTimeMillis
            resp  <- handler(a).map(responder).handleErrorWith(ErrorHandler.other)
            end   <- currentTimeMillis
            _     <- measurementHandler(endpoint, resp.status.code, Some(FiniteDuration(end - start, TimeUnit.MILLISECONDS)))
          } yield resp
      }

    def implementedBy(implementation: A => F[B]): HttpRoutes[F] =
      Kleisli(req => OptionT(F.handleErrorWith(request(req).flatMap(a => OptionT.liftF(measureLatency(a, implementation, response))).value)(err => ErrorHandler.other(err).map(Option.apply))))
  }

  final def endpoint[A, B, C: Show](request: HttpRequest[A], response: HttpResponse[B], tag: C, summary: String): HttpEndpoint[A, B] =
    HttpEndpoint(request, response)

  /**
   * Template method for measuring
   *
   * @param endpoint The endpoint being hit
   * @param responseStatusCode The status code which is returned
   * @param latency The measured latency (this starts when we got the complete headers + body), before that we don't measure anything yet
   * @return Nothing, but you can track it
   */
  @silent
  def measurementHandler(endpoint: Endpoint, responseStatusCode: Int, latency: Option[FiniteDuration]): F[Unit] = F.unit

  val routes: HttpRoutes[F]

}

trait Http4sError[F[_]] {
  def headerMissing(endpoint: Endpoint, expectedHeader: String): F[Resp[F]]
  def headerDecodeFailure(endpoint: Endpoint, headerName: String, cause: Option[Throwable]): F[Resp[F]]
  def bodyFailure(endpoint: Endpoint, error: String, cause: Option[Throwable]): F[Resp[F]]
  def other(throwable: Throwable): F[Resp[F]]
}
object Http4sError {
  def default[F[_]](implicit F: Effect[F]): Http4sError[F] = new Http4sError[F] {
    override def headerMissing(endpoint: Endpoint, expectedHeader: String): F[Resp[F]] =
      F.pure(Resp(Status.BadRequest).withEntity(s"Missing header `$expectedHeader`"))
    override def headerDecodeFailure(endpoint: Endpoint, headerName: String, cause: Option[Throwable]): F[Resp[F]] =
      F.pure(Resp(Status.BadRequest).withEntity(s"Decode failure for header `$headerName`"))
    override def bodyFailure(endpoint: Endpoint, error: String, cause: Option[Throwable]): F[Resp[F]] =
      F.pure(Resp(Status.BadRequest).withEntity(error))
    override def other(throwable: Throwable): F[Resp[F]] =
      throwable match {
        case UriDecodeFailure(error, _) => F.pure(Resp(Status.BadRequest).withEntity(error))
        case NonFatal(_)                => F.pure(Resp(Status.InternalServerError))
      }
  }
}

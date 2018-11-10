package itinere.http4s_server
import cats.Applicative
import org.http4s.{DecodeFailure, HttpVersion, Response, Status}

final case class RequestMessage[A](method: String, uri: String, value: A)

final case class UriDecodeFailure(error: String, cause: Option[Throwable]) extends DecodeFailure {
  def message: String = cause.fold(error)(exception => s"$error: ${exception.getMessage}")

  override def toHttpResponse[F[_]](httpVersion: HttpVersion)(implicit F: Applicative[F]): F[Response[F]] =
    F.pure(Response(Status.BadRequest).withEntity(message))
}

final case class HeaderDecodeFailure(error: String, cause: Option[Throwable]) extends DecodeFailure {
  def message: String = cause.fold(error)(exception => s"$error: ${exception.getMessage}")

  override def toHttpResponse[F[_]](httpVersion: HttpVersion)(implicit F: Applicative[F]): F[Response[F]] =
    F.pure(Response(Status.BadRequest).withEntity(error))
}



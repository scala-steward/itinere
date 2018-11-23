package itinere.http4s_server
import cats.Monad
import cats.data.OptionT
import cats.effect.Sync
import org.http4s.{Method, Uri}

final case class Endpoint(method: String, uri: String)

final case class RequestMessage[A](endpoint: Endpoint, value: A) {
  def withValue[B](value: B): RequestMessage[B] = copy(value = value)
}

final case class UriDecodeFailure(error: String, cause: Option[Throwable]) extends Throwable(error) {
  def message: String = cause.fold(error)(exception => s"$error : ${exception.getMessage}")
}

sealed trait UriDecodeResult[+A] { self =>

  def toOptionT[F[_], B >: A](method: Method)(implicit F: Sync[F]): OptionT[F, RequestMessage[B]] = self match {
    case UriDecodeResult.Matched(result, uri, segments) => if (uri.path == "") OptionT.pure[F](RequestMessage(Endpoint(method.name, segments.mkString("/")), result)) else OptionT.none[F, RequestMessage[B]]
    case UriDecodeResult.Fatal(err, cause)              => OptionT.liftF[F, RequestMessage[B]](F.raiseError(UriDecodeFailure(err, cause)))
    case UriDecodeResult.NoMatch                        => OptionT.none[F, RequestMessage[B]]
  }

  def map[B](f: A => B): UriDecodeResult[B] = self match {
    case UriDecodeResult.NoMatch                     => UriDecodeResult.NoMatch
    case UriDecodeResult.Matched(v, remainder, segs) => UriDecodeResult.Matched(f(v), remainder, segs)
    case UriDecodeResult.Fatal(err, cause)           => UriDecodeResult.Fatal(err, cause)
  }

  def prependSegments(segments: List[String]): UriDecodeResult[A] = self match {
    case UriDecodeResult.Matched(result, remainder, existing) => UriDecodeResult.Matched(result, remainder, segments ::: existing)
    case UriDecodeResult.NoMatch                              => UriDecodeResult.NoMatch
    case UriDecodeResult.Fatal(error, cause)                  => UriDecodeResult.Fatal(error, cause)
  }
}

object UriDecodeResult {
  case class Matched[A](result: A, remainder: Uri, segments: List[String]) extends UriDecodeResult[A]
  case class Fatal(error: String, cause: Option[Throwable] = None) extends UriDecodeResult[Nothing]
  case object NoMatch extends UriDecodeResult[Nothing]
}

trait UriDecoder[A] {
  def decode(uri: Uri): UriDecodeResult[A]
}

object UriDecoder {

  implicit val monad: Monad[UriDecoder] = new Monad[UriDecoder] {
    override def pure[A](x: A): UriDecoder[A] = new UriDecoder[A] {
      override def decode(uri: Uri): UriDecodeResult[A] = UriDecodeResult.Matched(x, uri, Nil)
    }

    override def flatMap[A, B](fa: UriDecoder[A])(f: (A) => UriDecoder[B]): UriDecoder[B] = new UriDecoder[B] {
      override def decode(uri: Uri): UriDecodeResult[B] = fa.decode(uri) match {
        case UriDecodeResult.Matched(v, remainder, segments) => f(v).decode(remainder).prependSegments(segments)
        case UriDecodeResult.NoMatch                         => UriDecodeResult.NoMatch
        case UriDecodeResult.Fatal(err, cause)               => UriDecodeResult.Fatal(err, cause)
      }
    }

    override def tailRecM[A, B](a: A)(f: (A) => UriDecoder[Either[A, B]]): UriDecoder[B] = flatMap(f(a)) {
      case Left(v)  => tailRecM(v)(f)
      case Right(v) => pure(v)
    }
  }
}

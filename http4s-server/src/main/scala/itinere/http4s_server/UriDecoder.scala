package itinere.http4s_server
import cats.{Eq, Monad}
import cats.data.OptionT
import cats.effect.Sync
import org.http4s.Uri

sealed trait UriDecodeResult[+A] { self =>

  def toOptionT[F[_], B >: A](implicit F: Sync[F]): OptionT[F, B] = self match {
    case UriDecodeResult.Matched(result, uri, _)    => if(uri.path == "") OptionT.pure[F](result) else OptionT.none[F, B]
    case UriDecodeResult.Fatal(err, cause)          => OptionT.liftF[F, B](F.raiseError(UriDecodeFailure(err, cause)))
    case UriDecodeResult.NoMatch                    => OptionT.none[F, B]
  }

  def map[B](f: A => B): UriDecodeResult[B] = self match {
    case UriDecodeResult.NoMatch => UriDecodeResult.NoMatch
    case UriDecodeResult.Matched(v, remainder, segs) => UriDecodeResult.Matched(f(v), remainder, segs)
    case UriDecodeResult.Fatal(err, cause) => UriDecodeResult.Fatal(err, cause)
  }

  def prependSegments(segments: List[String]): UriDecodeResult[A] = self match {
    case UriDecodeResult.Matched(result, remainder, existing) => UriDecodeResult.Matched(result, remainder, segments ::: existing)
    case UriDecodeResult.NoMatch                              => UriDecodeResult.NoMatch
    case UriDecodeResult.Fatal(error, cause)                  => UriDecodeResult.Fatal(error, cause)
  }

  def matchedSegments: List[String] = self match {
    case UriDecodeResult.Matched(_, _, segments) => segments
    case UriDecodeResult.NoMatch                 => Nil
    case UriDecodeResult.Fatal(_, _)             => Nil
  }
}

object UriDecodeResult {
  case class Matched[A](result: A, remainder: Uri, segments: List[String]) extends UriDecodeResult[A]
  case object NoMatch extends UriDecodeResult[Nothing]
  case class Fatal(error: String, cause: Option[Throwable] = None) extends UriDecodeResult[Nothing]
}

trait UriDecoder[A] {
  def decode(uri: Uri): UriDecodeResult[A]
}

object UriDecoder {

  implicit def eq[A]: Eq[UriDecoder[A]] = Eq.allEqual

  implicit val monad: Monad[UriDecoder] = new Monad[UriDecoder] {
    override def pure[A](x: A): UriDecoder[A] = new UriDecoder[A] {
      override def decode(uri: Uri): UriDecodeResult[A] = UriDecodeResult.Matched(x, uri, Nil)
    }

    override def flatMap[A, B](fa: UriDecoder[A])(f: (A) => UriDecoder[B]): UriDecoder[B] = new UriDecoder[B] {
      override def decode(uri: Uri): UriDecodeResult[B] = fa.decode(uri) match {
        case UriDecodeResult.Matched(v, remainder, segments) => f(v).decode(remainder).prependSegments(segments)
        case UriDecodeResult.NoMatch => UriDecodeResult.NoMatch
        case UriDecodeResult.Fatal(err, cause) => UriDecodeResult.Fatal(err, cause)
      }
    }

    override def tailRecM[A, B](a: A)(f: (A) => UriDecoder[Either[A, B]]): UriDecoder[B] = flatMap(f(a)) {
      case Left(v) => tailRecM(v)(f)
      case Right(v) => pure(v)
    }
  }
}

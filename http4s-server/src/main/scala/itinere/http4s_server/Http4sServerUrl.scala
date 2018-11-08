package itinere.http4s_server


import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import cats.{Invariant, Monad}
import itinere.{Attempt, Read, Tupler, UrlAlgebra}
import org.http4s._
import shapeless.HNil

final case class UriDecodeException(error: String, cause: Option[Throwable]) extends Throwable(error) {
  def message: String = cause.fold(error)(exception => s"$error (${exception.getMessage})")
}

trait Http4sServerUrl extends UrlAlgebra {

  sealed trait UriDecodeResult[+A] { self =>

    def toOptionT[F[_], B >: A](implicit F: Sync[F]): OptionT[F, B] = self match {
      //TODO: how can we be sure the url is fully decoded?
      case UriDecodeResult.Matched(result, uri)       => if(uri.path == "") OptionT.pure[F](result) else OptionT.none[F, B]
      case UriDecodeResult.Fatal(err, cause)          => OptionT.liftF[F, B](F.raiseError(UriDecodeException(err, cause)))
      case UriDecodeResult.NotMatched(error)          => OptionT.none[F, B]
    }

    def map[B](f: A => B): UriDecodeResult[B] = self match {
      case UriDecodeResult.NotMatched(err) => UriDecodeResult.NotMatched(err)
      case UriDecodeResult.Matched(v, remainder) => UriDecodeResult.Matched(f(v), remainder)
      case UriDecodeResult.Fatal(err, cause) => UriDecodeResult.Fatal(err, cause)
    }
  }

  object UriDecodeResult {
    case class Matched[A](result: A, remainder: Uri) extends UriDecodeResult[A]
    case class NotMatched(error: String) extends UriDecodeResult[Nothing]
    case class Fatal(error: String, cause: Option[Throwable] = None) extends UriDecodeResult[Nothing]
  }

  trait UriDecoder[A] {
    def decode(uri: Uri): UriDecodeResult[A]
  }

  object UriDecoder {
    implicit val monad: Monad[UriDecoder] = new Monad[UriDecoder] {
      override def pure[A](x: A): UriDecoder[A] = new UriDecoder[A] {
        override def decode(uri: Uri): UriDecodeResult[A] = UriDecodeResult.Matched(x, uri)
      }

      override def flatMap[A, B](fa: UriDecoder[A])(f: (A) => UriDecoder[B]): UriDecoder[B] = new UriDecoder[B] {
        override def decode(uri: Uri): UriDecodeResult[B] = fa.decode(uri) match {
          case UriDecodeResult.Matched(v, remainder) => f(v).decode(remainder)
          case UriDecodeResult.NotMatched(err) => UriDecodeResult.NotMatched(err)
          case UriDecodeResult.Fatal(err, cause) => UriDecodeResult.Fatal(err, cause)
        }
      }

      override def tailRecM[A, B](a: A)(f: (A) => UriDecoder[Either[A, B]]): UriDecoder[B] = flatMap(f(a)) {
        case Left(v) => tailRecM(v)(f)
        case Right(v) => pure(v)
      }
    }
  }

  type Path[A] = UriDecoder[A]
  type Url[A] = UriDecoder[A]
  type QueryString[A] = UriDecoder[A]

  override def combineQueryStrings[A, B](first: QueryString[A], second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out] =
    first.flatMap(a => second.map(b => tupler(a, b)))

  override def qs[A](name: String, value: Read[A], description: Option[String]): QueryString[Option[A]] = new QueryString[Option[A]] {
    override def decode(uri: Uri): UriDecodeResult[Option[A]] =
      uri.query.params.get(name) match {
        case None    => UriDecodeResult.Matched(None, uri)
        case Some(v) => value.fromString(v) match {
          case Attempt.Success(vv)     => UriDecodeResult.Matched(Some(vv), uri)
          case Attempt.Exception(err)  => UriDecodeResult.Fatal(s"Failed to decode query string $name", Some(err))
          case Attempt.Error(err)      => UriDecodeResult.Fatal(s"Failed to decode query string $name : $err", None)
        }
      }
  }

  override def staticPathSegment(segment: String): Path[HNil] = new Path[HNil] {
    override def decode(uri: Uri): UriDecodeResult[HNil] = {
      val path = uri.path.split('/')

      path.headOption.fold[UriDecodeResult[HNil]](UriDecodeResult.NotMatched("Path is empty!"))(s =>
        if(segment == s) UriDecodeResult.Matched(HNil, uri.copy(path = path.tail.mkString("/")))
        else UriDecodeResult.NotMatched(s"Segment `$s` didn't equal to `$segment`")
      )
    }

  }

  override def segment[A](name: String, value: Read[A], description: Option[String]): Path[A] = new Path[A] {
    override def decode(uri: Uri): UriDecodeResult[A] = {
      val path = uri.path.split('/')

      path.headOption.fold[UriDecodeResult[A]](UriDecodeResult.NotMatched("Path is empty!")) { s =>
        value.fromString(s) match {
          case Attempt.Success(vv)     => UriDecodeResult.Matched(vv, uri.copy(path = path.tail.mkString("/")))
          case Attempt.Exception(err)  => UriDecodeResult.Fatal(s"Failed to decode segment $name", Some(err))
          case Attempt.Error(err)      => UriDecodeResult.Fatal(s"Failed to decode segment $name : $err", None)
        }
      }
    }
  }

  override def chainPaths[A, B](first: Path[A], second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out] =
    first.flatMap(a => second.map(b => tupler(a, b)))

  override def urlWithQueryString[A, B](path: Path[A], qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out] =
    path.flatMap(a => qs.map(b => tupler(a, b)))

  override implicit val queryStringInvariantFunctor: Invariant[QueryString] = new Invariant[QueryString] {
    override def imap[A, B](fa: QueryString[A])(f: (A) => B)(g: (B) => A): QueryString[B] = new QueryString[B] {
      override def decode(uri: Uri): UriDecodeResult[B] = fa.decode(uri).map(f)
    }
  }
  override implicit val pathInvariantFunctor: Invariant[Path] = new Invariant[Path] {
    override def imap[A, B](fa: Path[A])(f: (A) => B)(g: (B) => A): Path[B] = new Path[B] {
      override def decode(uri: Uri): UriDecodeResult[B] = fa.decode(uri).map(f)
    }
  }
  override implicit val urlInvariantFunctor: Invariant[Url] = new Invariant[Url] {
    override def imap[A, B](fa: Url[A])(f: (A) => B)(g: (B) => A): Url[B] = new Url[B] {
      override def decode(uri: Uri): UriDecodeResult[B] = fa.decode(uri).map(f)
    }
  }
}

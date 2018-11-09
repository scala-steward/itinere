package itinere.http4s_server


import cats.data.{NonEmptyList, OptionT}
import cats.effect.Sync
import cats.implicits._
import cats.{Invariant, Monad}
import itinere.{Attempt, Read, ReadPrimitives, Tupler, UrlAlgebra}
import org.http4s._
import shapeless.HNil

final case class UriDecodeException(error: String, cause: Option[Throwable]) extends Throwable(error) {
  def message: String = cause.fold(error)(exception => s"$error (${exception.getMessage})")
}

trait Http4sServerUrl extends UrlAlgebra { self: ReadPrimitives =>

  sealed trait UriDecodeResult[+A] { self =>

    def toOptionT[F[_], B >: A](implicit F: Sync[F]): OptionT[F, B] = self match {
      case UriDecodeResult.Matched(result, uri, _)    => if(uri.path == "") OptionT.pure[F](result) else OptionT.none[F, B]
      case UriDecodeResult.Fatal(err, cause)          => OptionT.liftF[F, B](F.raiseError(UriDecodeException(err, cause)))
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

  type Path[A] = UriDecoder[A]
  type Url[A] = UriDecoder[A]
  type QueryString[A] = UriDecoder[A]

  override def combineQueryStrings[A, B](first: QueryString[A], second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out] =
    first.flatMap(a => second.map(b => tupler(a, b)))

  override def qs[A](name: String, description: Option[String])(implicit QSV: Primitive[A]): QueryString[Option[A]] = new QueryString[Option[A]] {
    override def decode(uri: Uri): UriDecodeResult[Option[A]] =
      uri.query.params.get(name) match {
        case None    => UriDecodeResult.Matched(None, uri, List.empty)
        case Some(v) => QSV.fromString(v) match {
          case Attempt.Success(vv)     => UriDecodeResult.Matched(Some(vv), uri, List.empty)
          case Attempt.Exception(err)  => UriDecodeResult.Fatal(s"Failed to decode query string $name", Some(err))
          case Attempt.Error(err)      => UriDecodeResult.Fatal(s"Failed to decode query string $name : $err", None)
        }
      }
  }

  override def staticPathSegment(segment: String): Path[HNil] = new Path[HNil] {
    override def decode(uri: Uri): UriDecodeResult[HNil] = {
      val path = uri.path.split('/')

      path.headOption.fold[UriDecodeResult[HNil]](UriDecodeResult.NoMatch)(s =>
        if(segment == s) UriDecodeResult.Matched(HNil, uri.copy(path = path.tail.mkString("/")), if(segment.isEmpty) Nil else segment :: Nil)
        else UriDecodeResult.NoMatch
      )
    }

  }

  override def segment[A](name: String, description: Option[String])(implicit S: Primitive[A]): Path[A] = new Path[A] {
    override def decode(uri: Uri): UriDecodeResult[A] = {
      val path = uri.path.split('/')

      path.headOption.fold[UriDecodeResult[A]](UriDecodeResult.NoMatch) { s =>
        S.fromString(s) match {
          case Attempt.Success(vv)     => UriDecodeResult.Matched(vv, uri.copy(path = path.tail.mkString("/")), s":$name" :: Nil)
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

package itinere.http4s_server


import cats.implicits._
import cats.{Invariant, Monad}
import itinere.{Read, Tupler, UrlAlgebra}
import org.http4s._
import shapeless.HNil

trait Http4sServerUrl extends UrlAlgebra {

  sealed trait UriDecodeResult[+A] { self =>

    def fold[B](ifFailure: String => B, ifSuccess: (A, Uri) => B): B = self match {
      case UriDecodeResult.Success(v, uri) => ifSuccess(v, uri)
      case UriDecodeResult.Failure(err) => ifFailure(err)
    }

    def toOption: Option[A] =
      fold(_ => None, (v, uri) => if(uri.path == "") Some(v) else None)

    def toEither: Either[String, A] =
      fold(Left.apply, (v, uri) => if(uri.path == "") Right(v) else Left(s"Uri.path should be empty, but it's not: ${uri.path}"))

    def map[B](f: A => B): UriDecodeResult[B] = self match {
      case UriDecodeResult.Failure(err) => UriDecodeResult.Failure(err)
      case UriDecodeResult.Success(v, remainder) => UriDecodeResult.Success(f(v), remainder)
    }
  }

  object UriDecodeResult {
    case class Success[A](result: A, remainder: Uri) extends UriDecodeResult[A]
    case class Failure(error: String) extends UriDecodeResult[Nothing]
  }

  trait UriDecoder[A] {
    def decode(uri: Uri): UriDecodeResult[A]
  }

  object UriDecoder {
    implicit val monad: Monad[UriDecoder] = new Monad[UriDecoder] {
      override def pure[A](x: A): UriDecoder[A] = new UriDecoder[A] {
        override def decode(uri: Uri): UriDecodeResult[A] = UriDecodeResult.Success(x, uri)
      }

      override def flatMap[A, B](fa: UriDecoder[A])(f: (A) => UriDecoder[B]): UriDecoder[B] = new UriDecoder[B] {
        override def decode(uri: Uri): UriDecodeResult[B] = fa.decode(uri) match {
          case UriDecodeResult.Success(v, remainder) => f(v).decode(remainder)
          case UriDecodeResult.Failure(err) => UriDecodeResult.Failure(err)
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

  override def qs[A](name: String, value: Read[A], description: Option[String]): QueryString[A] = new UriDecoder[A] {
    override def decode(uri: Uri): UriDecodeResult[A] =
      uri.query
        .params
        .get(name)
        .fold[Either[String, String]](Left("No such param"))(Right.apply)
        .flatMap(str => value.fromString(str).toEither)
        .fold[UriDecodeResult[A]](err => UriDecodeResult.Failure(err), v => UriDecodeResult.Success(v, uri))
  }

  override def optQs[A](name: String, value: Read[A], description: Option[String]): QueryString[Option[A]] = new QueryString[Option[A]] {
    override def decode(uri: Uri): UriDecodeResult[Option[A]] =
      uri.query.params.get(name) match {
        case None => UriDecodeResult.Success(None, uri)
        case Some(v) => value.fromString(v).toEither match {
          case Left(err) => UriDecodeResult.Failure(err)
          case Right(vv) => UriDecodeResult.Success(Some(vv), uri)
        }
      }
  }

  override def staticPathSegment(segment: String): Path[HNil] = new Path[HNil] {
    override def decode(uri: Uri): UriDecodeResult[HNil] = {
      val path = uri.path.split('/')

      path.headOption.fold[UriDecodeResult[HNil]](UriDecodeResult.Failure("Path is empty!"))(s =>
        if(segment == s) UriDecodeResult.Success(HNil, uri.copy(path = path.tail.mkString("/")))
        else UriDecodeResult.Failure(s"Segment `$s` didn't equal to `$segment`")
      )
    }

  }

  override def segment[A](name: String, value: Read[A], description: Option[String]): Path[A] = new Path[A] {
    override def decode(uri: Uri): UriDecodeResult[A] = {
      val path = uri.path.split('/')

      path.headOption.fold[UriDecodeResult[A]](UriDecodeResult.Failure("Path is empty!")) { s =>
        value.fromString(s).toEither match {
          case Left(err) => UriDecodeResult.Failure(err)
          case Right(v) => UriDecodeResult.Success(v, uri.copy(path = path.tail.mkString("/")))
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

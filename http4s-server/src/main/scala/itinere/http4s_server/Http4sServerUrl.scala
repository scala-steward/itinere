package itinere.http4s_server


import cats.data.{NonEmptyList, OptionT}
import cats.effect.Sync
import cats.implicits._
import cats.{Invariant, Monad}
import itinere.{Attempt, Partial, Primitives, Read, ReadPrimitives, Tupler, UrlAlgebra}
import org.http4s._
import shapeless.HNil


trait Http4sServerUrl extends UrlAlgebra {

  type Path[A] = UriDecoder[A]
  type Url[A] = UriDecoder[A]
  type QueryString[A] = UriDecoder[A]
  type QueryStringValue[A] = Read[A]
  type SegmentValue[A] = Read[A]

  protected def QueryStringValues: Primitives[Read] = ReadPrimitives
  protected def SegmentValues: Primitives[Read] = ReadPrimitives

  override def combineQueryStrings[A, B](first: QueryString[A], second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out] =
    first.flatMap(a => second.map(b => tupler(a, b)))

  override def qs[A](name: String, f: Primitives[Read] => QueryStringValue[A], description: Option[String]): QueryString[Option[A]] = new QueryString[Option[A]] {
    override def decode(uri: Uri): UriDecodeResult[Option[A]] =
      uri.query.params.get(name) match {
        case None    => UriDecodeResult.Matched(None, uri, List.empty)
        case Some(v) => f(QueryStringValues).fromString(v) match {
          case Attempt.Success(vv)     => UriDecodeResult.Matched(Some(vv), uri, List.empty)
          case Attempt.Error(err, cause)      => UriDecodeResult.Fatal(s"Failed to decode query string $name", cause)
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

  override def segment[A](name: String, segment: Primitives[SegmentValue] => SegmentValue[A], description: Option[String]): Path[A] = new Path[A] {
    override def decode(uri: Uri): UriDecodeResult[A] = {
      val path = uri.path.split('/')

      path.headOption.fold[UriDecodeResult[A]](UriDecodeResult.NoMatch) { s =>
        segment(SegmentValues).fromString(s) match {
          case Attempt.Success(vv)     => UriDecodeResult.Matched(vv, uri.copy(path = path.tail.mkString("/")), s":$name" :: Nil)
          case Attempt.Error(err, cause)      => UriDecodeResult.Fatal(s"Failed to decode segment $name", cause)
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
  override implicit val queryStringValueInvariant: Invariant[Read] = Read.readInvariant
  override implicit val queryStringValuePartial: Partial[Read] = Read.readPartial
  override implicit val segmentInvariant: Invariant[Read] = Read.readInvariant
  override implicit val segmentPartial: Partial[Read] = Read.readPartial
}

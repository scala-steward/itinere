package itinere.http4s_server

import cats.Invariant
import cats.implicits._
import fs2.Stream
import itinere.{HttpRequestAlgebra, Read, Tupler}
import org.http4s.util.CaseInsensitiveString
import org.http4s.{Headers, Method, Request => Req}
import shapeless.HNil

trait Http4sServerRequest extends HttpRequestAlgebra with Http4sServerUrl { self: Http4sServer =>
  trait HttpRequestHeaders[A] {
    def decode(headers: Headers): Either[String, A]
  }

  trait HttpRequestEntity[A] {
    def decode(stream: Stream[F, Byte]): F[Either[String, A]]
  }

  type HttpRequest[A] = PartialFunction[Req[F], F[A]]

  override type HttpMethod = Method

  override def GET = Method.GET
  override def PUT = Method.PUT
  override def POST = Method.POST
  override def DELETE = Method.DELETE
  override def PATCH = Method.PATCH

  override def requestHeader[A](name: String, read: Read[A], description: Option[String]): HttpRequestHeaders[A] = new HttpRequestHeaders[A] {
    override def decode(headers: Headers): Either[String, A] =
      headers.get(CaseInsensitiveString(name))
        .fold[Either[String, String]](Left(s"No header found for $name"))(x => Right(x.value))
        .flatMap(str => read.fromString(str).toEither)
  }

  override def combineRequestHeaders[A, B](left: HttpRequestHeaders[A], right: HttpRequestHeaders[B])(implicit T: Tupler[A, B]): HttpRequestHeaders[T.Out] = new HttpRequestHeaders[T.Out] {
    override def decode(headers: Headers): Either[String, T.Out] = for {
      a <- left.decode(headers)
      b <- right.decode(headers)
    } yield T(a, b)
  }

  override def emptyRequestHeaders: HttpRequestHeaders[HNil] = new HttpRequestHeaders[HNil] {
    override def decode(headers: Headers): Either[String, HNil] = Right(HNil)
  }

  override def emptyRequestEntity: HttpRequestEntity[HNil] = new HttpRequestEntity[HNil] {
    override def decode(stream: Stream[F, Byte]): F[Either[String, HNil]] =
      stream.compile.toList.map(s =>
        if(s.isEmpty) Right(HNil)
        else Left("Expected empty body, but given")
      )
  }

  override def request[A, B, C, AB](
                                     method: Method,
                                     url: Url[A],
                                     headers: HttpRequestHeaders[B],
                                     entity: HttpRequestEntity[C])(implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): HttpRequest[TO.Out] =
    Function.unlift[Req[F], F[TO.Out]] { req =>
      if(req.method === method) {
        for {
          a <- url.decode(req.uri).toOption
          b <- headers.decode(req.headers).toOption
        } yield entity
          .decode(req.body)
          .flatMap(_.fold[F[C]](err => F.raiseError(new Throwable(err)), F.pure))
          .map(c => TO.apply(T.apply(a, b), c))
      } else {
        None
      }
    }

  override implicit val httpRequestHeadersInvariantFunctor: Invariant[HttpRequestHeaders] = new Invariant[HttpRequestHeaders] {
    override def imap[A, B](fa: HttpRequestHeaders[A])(f: (A) => B)(g: (B) => A): HttpRequestHeaders[B] = new HttpRequestHeaders[B] {
      override def decode(headers: Headers): Either[String, B] = fa.decode(headers).map(f)
    }
  }
  override implicit val httpRequestEntityInvariantFunctor: Invariant[HttpRequestEntity] = new Invariant[HttpRequestEntity] {
    override def imap[A, B](fa: HttpRequestEntity[A])(f: (A) => B)(g: (B) => A): HttpRequestEntity[B] = new HttpRequestEntity[B] {
      override def decode(stream: Stream[F, Byte]): F[Either[String, B]] = fa.decode(stream).map(_.map(f))
    }
  }
  override implicit val httpRequestInvariantFunctor: Invariant[HttpRequest] = new Invariant[HttpRequest] {
    override def imap[A, B](fa: HttpRequest[A])(f: (A) => B)(g: (B) => A): HttpRequest[B] = {
      case r if fa.isDefinedAt(r) => fa(r).map(f)
    }
  }
}

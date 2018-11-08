package itinere.http4s_server

import cats.Invariant
import cats.data.{EitherT, OptionT}
import cats.implicits._
import fs2.Stream
import itinere.{HttpRequestAlgebra, Read, Tupler}
import org.http4s.util.CaseInsensitiveString
import org.http4s.{DecodeResult, EntityDecoder, Headers, MediaRange, Message, Method, Request => Req}
import shapeless.HNil


trait Http4sServerRequest extends HttpRequestAlgebra with Http4sServerUrl { self: Http4sServer =>
  trait HttpRequestHeaders[A] {
    def decode(headers: Headers): Either[String, A]
  }

  type HttpRequestEntity[A] = EntityDecoder[F, A]

  type HttpRequest[A] = Req[F] => OptionT[F, A]

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
    override def decode(headers: Headers): Either[String, T.Out] = left.decode(headers).flatMap(a => right.decode(headers).map(b => T(a, b)))
  }

  override def emptyRequestHeaders: HttpRequestHeaders[HNil] = new HttpRequestHeaders[HNil] {
    override def decode(headers: Headers): Either[String, HNil] = Right(HNil)
  }

  override def emptyRequestEntity: HttpRequestEntity[HNil] = EntityDecoder.void.map(_ => HNil)

  override def request[A, B, C, AB](
                                     method: Method,
                                     url: Url[A],
                                     headers: HttpRequestHeaders[B],
                                     entity: HttpRequestEntity[C])(implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): HttpRequest[TO.Out] = req => {
    if(req.method === method) {
      for {
        a <- url.decode(req.uri).toOptionT
        b <- OptionT.fromOption[F](headers.decode(req.headers).toOption) //TODO also get errors from here
        c <- OptionT.liftF(req.as[C](F, entity))
      } yield TO.apply(T.apply(a, b), c)
    } else {
      OptionT.none
    }
  }


  override implicit val httpRequestHeadersInvariantFunctor: Invariant[HttpRequestHeaders] = new Invariant[HttpRequestHeaders] {
    override def imap[A, B](fa: HttpRequestHeaders[A])(f: (A) => B)(g: (B) => A): HttpRequestHeaders[B] = new HttpRequestHeaders[B] {
      override def decode(headers: Headers): Either[String, B] = fa.decode(headers).map(f)
    }
  }
  override implicit val httpRequestEntityInvariantFunctor: Invariant[HttpRequestEntity] = new Invariant[HttpRequestEntity] {
    override def imap[A, B](fa: HttpRequestEntity[A])(f: (A) => B)(g: (B) => A): HttpRequestEntity[B] = new HttpRequestEntity[B] {
      override def decode(msg: Message[F], strict: Boolean): DecodeResult[F, B] = fa.decode(msg, strict).map(f)
      override def consumes: Set[MediaRange] = fa.consumes
    }
  }
  override implicit val httpRequestInvariantFunctor: Invariant[HttpRequest] = new Invariant[HttpRequest] {
    override def imap[A, B](fa: HttpRequest[A])(f: (A) => B)(g: (B) => A): HttpRequest[B] = {
      req => fa(req).map(f)
    }
  }

  private def foldF[A, B, C](eitherT: F[Either[A, B]])(fb: B => F[C])(fa: A => F[C]): F[C] =
    F.flatMap(eitherT)(_.fold(fa, fb))
}

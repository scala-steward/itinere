package itinere.http4s_server

import cats.Invariant
import cats.data.OptionT
import cats.implicits._
import itinere.{HttpMethods, HttpRequestAlgebra, Read, Tupler}
import org.http4s.util.CaseInsensitiveString
import org.http4s.{DecodeResult, EntityDecoder, Headers, MediaRange, Message, Method, Request => Req}
import shapeless.HNil


trait Http4sServerRequest extends HttpRequestAlgebra with Http4sServerUrl { self: Http4sServer =>
  trait HttpRequestHeaders[A] {
    def decode(headers: Headers): Either[String, A]
  }

  type HttpRequestEntity[A] = EntityDecoder[F, A]
  type HttpRequest[A] = Req[F] => OptionT[F, RequestMessage[A]]
  type HttpMethod = Method

  val HttpMethod: HttpMethods[Method] = Http4sMethods

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
      val urlDecode = url.decode(req.uri)

      for {
        a <- urlDecode.toOptionT
        b <- OptionT.fromOption[F](headers.decode(req.headers).toOption) //TODO also get errors from here
        c <- OptionT.liftF(req.as[C](F, entity))
        uri = urlDecode.matchedSegments.mkString("/")
      } yield RequestMessage(req.method, uri, TO.apply(T.apply(a, b), c))
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
      req => fa(req).map(x => RequestMessage(x.method, x.uri, f(x.value)))
    }
  }
}

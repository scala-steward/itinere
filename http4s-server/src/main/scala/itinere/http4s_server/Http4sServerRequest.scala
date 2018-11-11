package itinere.http4s_server

import cats.Invariant
import cats.data.{Kleisli, OptionT}
import cats.implicits._
import itinere.{Attempt, HttpMethods, HttpRequestAlgebra, Partial, Primitives, Read, ReadPrimitives, Tupler}
import org.http4s.util.CaseInsensitiveString
import org.http4s.{EntityDecoder, Headers, Method, Request => Req}
import shapeless.HNil

trait Http4sServerRequest extends HttpRequestAlgebra with Http4sServerUrl { self: Http4sServer =>
  type HttpRequest[A] = Kleisli[OptionT[F, ?], Req[F], RequestMessage[A]]
  type HttpRequestHeaders[A] = Kleisli[OptionT[F, ?], Headers, A]
  type HttpRequestEntity[A] = EntityDecoder[F, A]
  type HttpMethod = Method
  type HttpRequestHeaderValue[A] = Read[A]

  val HttpMethod: HttpMethods[Method] = Http4sMethods

  protected def HttpRequestHeaderValues: Primitives[Read] = ReadPrimitives

  override def requestHeader[A](name: String, headerValue: Primitives[HttpRequestHeaderValue] => HttpRequestHeaderValue[A], description: Option[String]): HttpRequestHeaders[A] =
    Kleisli { headers =>
      for {
        rawValue <- headers.get(CaseInsensitiveString(name)) match {
          case Some(value) => OptionT.pure[F](value.value)
          case None        => OptionT.liftF[F, String](F.raiseError(HeaderDecodeFailure(s"Required header '$name' not present.", None)))
        }
        result <- headerValue(HttpRequestHeaderValues).fromString(rawValue) match {
          case Attempt.Success(value)  => OptionT.pure[F](value)
          case Attempt.Error(_, cause) => OptionT.liftF[F, A](F.raiseError(HeaderDecodeFailure(s"Failed to decode header '$name'", cause)))
        }
      } yield result
    }

  override def combineRequestHeaders[A, B](left: HttpRequestHeaders[A], right: HttpRequestHeaders[B])(implicit T: Tupler[A, B]): HttpRequestHeaders[T.Out] =
    (left, right).mapN((a, b) => T.apply(a, b))

  override def emptyRequestHeaders: HttpRequestHeaders[HNil] =
    Kleisli(_ => OptionT.pure(HNil))

  override def emptyRequestEntity: HttpRequestEntity[HNil] = EntityDecoder.void.map(_ => HNil)

  override def request[A, B, C, AB](method: Method, url: Url[A], headers: HttpRequestHeaders[B], entity: HttpRequestEntity[C])(implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): HttpRequest[TO.Out] = Kleisli { req =>
    if (req.method === method) {
      val urlDecode = url.decode(req.uri)
      for {
        a <- urlDecode.toOptionT
        b <- headers.run(req.headers)
        c <- OptionT.liftF(req.as[C](F, entity))
        uri = urlDecode.matchedSegments.mkString("/")
      } yield RequestMessage(req.method.name, uri, TO.apply(T.apply(a, b), c))
    } else {
      OptionT.none
    }
  }

  implicit override val httpRequestHeadersInvariantFunctor: Invariant[HttpRequestHeaders] = new Invariant[HttpRequestHeaders] {
    override def imap[A, B](fa: HttpRequestHeaders[A])(f: A => B)(g: B => A): HttpRequestHeaders[B] = fa.map(f)
  }
  implicit override val httpRequestEntityInvariantFunctor: Invariant[HttpRequestEntity] = new Invariant[HttpRequestEntity] {
    override def imap[A, B](fa: HttpRequestEntity[A])(f: A => B)(g: B => A): HttpRequestEntity[B] =
      fa.map(f)
  }
  implicit override val httpRequestInvariantFunctor: Invariant[HttpRequest] = new Invariant[HttpRequest] {
    override def imap[A, B](fa: HttpRequest[A])(f: A => B)(g: B => A): HttpRequest[B] =
      Kleisli(req => fa(req).map(x => RequestMessage(x.method, x.uri, f(x.value))))
  }

  implicit override val httpRequestHeaderValueInvariant: Invariant[Read] = Read.readInvariant
  implicit override val httpRequestHeaderValuePartial: Partial[Read] = Read.readPartial
}

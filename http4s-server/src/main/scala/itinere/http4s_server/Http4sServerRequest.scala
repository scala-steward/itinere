package itinere.http4s_server

import cats.Invariant
import cats.data.{EitherT, Kleisli, OptionT}
import cats.implicits._
import itinere.{Attempt, HttpMethods, HttpRequestAlgebra, Partial, Primitives, Read, ReadPrimitives, Tupler}
import org.http4s.util.CaseInsensitiveString
import org.http4s.{EntityDecoder, Headers, Method, Request => Req, Response => Resp}
import shapeless.HNil

trait Http4sServerRequest extends HttpRequestAlgebra with Http4sServerUrl { self: Http4sServer =>
  type HttpRequest[A] = Kleisli[OptionT[F, ?], Req[F], RequestMessage[Either[Resp[F], A]]]
  type HttpRequestHeaders[A] = Kleisli[EitherT[F, Resp[F], ?], RequestMessage[Headers], A]
  type HttpRequestEntity[A] = EntityDecoder[F, A]
  type HttpMethod = Method
  type HttpRequestHeaderValue[A] = Read[A]

  val HttpMethod: HttpMethods[Method] = Http4sMethods

  protected def HttpRequestHeaderValues: Primitives[Read] = ReadPrimitives

  override def requestHeader[A](name: String, headerValue: Primitives[HttpRequestHeaderValue] => HttpRequestHeaderValue[A], description: Option[String]): HttpRequestHeaders[A] =
    Kleisli { msg =>
      for {
        rawValue <- msg.value.get(CaseInsensitiveString(name)) match {
          case Some(value) => EitherT.pure[F, Resp[F]](value.value)
          case None        => EitherT.left(ErrorHandler.headerMissing(msg.endpoint, name))
        }
        result <- headerValue(HttpRequestHeaderValues).fromString(rawValue) match {
          case Attempt.Success(value)  => EitherT.pure[F, Resp[F]](value)
          case Attempt.Error(_, cause) => EitherT.left(ErrorHandler.headerDecodeFailure(msg.endpoint, name, cause))
        }
      } yield result
    }

  override def combineRequestHeaders[A, B](left: HttpRequestHeaders[A], right: HttpRequestHeaders[B])(implicit T: Tupler[A, B]): HttpRequestHeaders[T.Out] =
    (left, right).mapN((a, b) => T.apply(a, b))

  override def emptyRequestHeaders: HttpRequestHeaders[HNil] =
    Kleisli(_ => EitherT.pure(HNil))

  override def emptyRequestEntity: HttpRequestEntity[HNil] = EntityDecoder.void.map(_ => HNil)

  override def request[A, B, C, AB](method: Method, url: Url[A], headers: HttpRequestHeaders[B], entity: HttpRequestEntity[C])(implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): HttpRequest[TO.Out] = Kleisli { req =>
    def processMessage(a: RequestMessage[A]): EitherT[F, Resp[F], TO.Out] =
      for {
        b <- headers.run(a.withValue(req.headers))
        c <- req.attemptAs[C](entity).leftFlatMap(failure => EitherT.left[C](ErrorHandler.bodyFailure(a.endpoint, "Body decode failure", Some(failure))))
      } yield TO.apply(T.apply(a.value, b), c)

    if (req.method === method) url.decode(req.uri).toOptionT(req.method).flatMap(a => OptionT.liftF(processMessage(a).value.map(a.withValue)))
    else OptionT.none
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
      Kleisli(req => fa(req).map(x => RequestMessage(x.endpoint, x.value.map(f))))
  }

  implicit override val httpRequestHeaderValueInvariant: Invariant[Read] = Read.readInvariant
  implicit override val httpRequestHeaderValuePartial: Partial[Read] = Read.readPartial
}

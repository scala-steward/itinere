package itinere.http4s_server


import cats.Invariant
import fs2._
import itinere.{CoCartesian, HttpResponseAlgebra, Tupler}
import org.http4s.{Headers, Status, Response => Resp}
import shapeless.{CNil, HNil}

trait Http4sServerResponse extends HttpResponseAlgebra { self: Http4sServer =>
  override type HttpResponseHeaders[A] = A => Headers
  override type HttpResponseEntity[A] = A => Stream[F, Byte]
  override type HttpResponse[A] = A => Resp[F]

  override def emptyResponseHeaders: (HNil) => Headers = _ => Headers.empty

  override def emptyResponse: (HNil) => Stream[F, Byte] = _ => Stream.empty

  override def cnil: (CNil) => Resp[F] = _ => Resp[F](Status.Forbidden, body = Stream.empty)

  override def response[A, B](
                               statusCode: Int,
                               headers: (A) => Headers,
                               entity: (B) => Stream[F, Byte])(implicit T: Tupler[A, B]): (T.Out) => Resp[F] = out => {
    val (a,b) = T.unapply(out)
    val h = headers(a)
    val e = entity(b)
    val s = Status.fromInt(statusCode).getOrElse(sys.error(s"Invalid status code $statusCode"))

    Resp(s, headers = h, body = e)
  }

  override implicit val httpResponseResponseHeadersInvariantFunctor: Invariant[Lambda[A => Function[A, Headers]]] = new Invariant[Function[?, Headers]] {
    override def imap[A, B](fa: Function[A, Headers])(f: (A) => B)(g: (B) => A): Function[B, Headers] = b => fa(g(b))
  }
  override implicit val httpResponseEntityInvariantFunctor: Invariant[Lambda[A => Function[A, Stream[F, Byte]]]] = new Invariant[Function[?, Stream[F, Byte]]] {
    override def imap[A, B](fa: Function[A, Stream[F, Byte]])(f: (A) => B)(g: (B) => A): Function[B, Stream[F, Byte]] = b => fa(g(b))
  }
  override implicit val httpResponseInvariantFunctor: Invariant[Lambda[A => Function[A, Resp[F]]]] = new Invariant[Function[?, Resp[F]]] {
    override def imap[A, B](fa: Function[A, Resp[F]])(f: (A) => B)(g: (B) => A): Function[B, Resp[F]] = b => fa(g(b))
  }
  override implicit val httpResponseCocartesian: CoCartesian[Lambda[A => Function[A, Resp[F]]]] = new CoCartesian[Function[?, Resp[F]]] {
    override def sum[A, B](fa: Function[A, Resp[F]], fb: Function[B, Resp[F]]): Function[Either[A, B], Resp[F]] = {
      case Left(a) => fa(a)
      case Right(b) => fb(b)
    }
  }
}

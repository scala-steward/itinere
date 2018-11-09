package itinere.http4s_server


import cats.Invariant
import fs2._
import itinere.{CoCartesian, HttpResponseAlgebra, HttpStatusCodes, Tupler}
import org.http4s.{Headers, Status, Response => Resp}
import shapeless.{CNil, HNil}

trait Http4sServerResponse extends HttpResponseAlgebra { self: Http4sServer =>

  type HttpResponseHeaders[A] = A => Headers
  type HttpResponseEntity[A] = A => Stream[F, Byte]
  type HttpResponse[A] = A => Resp[F]
  type HttpStatus = Status

  val HttpStatus: HttpStatusCodes[Status] = Http4sStatusCodes

  def emptyResponseHeaders: (HNil) => Headers = _ => Headers.empty

  def emptyResponse: (HNil) => Stream[F, Byte] = _ => Stream.empty

  def cnil: (CNil) => Resp[F] = _ => Resp[F](Status.Forbidden, body = Stream.empty)

  def response[A, B](
                               statusCode: Status,
                               headers: (A) => Headers,
                               entity: (B) => Stream[F, Byte])(implicit T: Tupler[A, B]): (T.Out) => Resp[F] = out => {
    val (a,b) = T.unapply(out)
    val h = headers(a)
    val e = entity(b)

    Resp(statusCode, headers = h, body = e)
  }

  implicit val httpResponseResponseHeadersInvariantFunctor: Invariant[Lambda[A => Function[A, Headers]]] = new Invariant[Function[?, Headers]] {
    override def imap[A, B](fa: Function[A, Headers])(f: (A) => B)(g: (B) => A): Function[B, Headers] = b => fa(g(b))
  }
  implicit val httpResponseEntityInvariantFunctor: Invariant[Lambda[A => Function[A, Stream[F, Byte]]]] = new Invariant[Function[?, Stream[F, Byte]]] {
    override def imap[A, B](fa: Function[A, Stream[F, Byte]])(f: (A) => B)(g: (B) => A): Function[B, Stream[F, Byte]] = b => fa(g(b))
  }
  implicit val httpResponseInvariantFunctor: Invariant[Lambda[A => Function[A, Resp[F]]]] = new Invariant[Function[?, Resp[F]]] {
    override def imap[A, B](fa: Function[A, Resp[F]])(f: (A) => B)(g: (B) => A): Function[B, Resp[F]] = b => fa(g(b))
  }
  implicit val httpResponseCocartesian: CoCartesian[Lambda[A => Function[A, Resp[F]]]] = new CoCartesian[Function[?, Resp[F]]] {
    override def sum[A, B](fa: Function[A, Resp[F]], fb: Function[B, Resp[F]]): Function[Either[A, B], Resp[F]] = {
      case Left(a) => fa(a)
      case Right(b) => fb(b)
    }
  }
}


package itinere.http4s_server


import cats.Invariant
import fs2._
import itinere.{CoCartesian, HttpResponseAlgebra, HttpStatusCodes, Tupler}
import org.http4s.{EntityEncoder, Headers, Status, Response => Resp}
import shapeless.{CNil, HNil}

trait Http4sServerResponse extends HttpResponseAlgebra { self: Http4sServer =>

  type HttpResponseHeaders[A] = A => Headers
  type HttpResponseEntity[A] = EntityEncoder[F, A]
  type HttpResponse[A] = A => Resp[F]
  type HttpStatus = Status
  type HttpHeader

  val HttpStatus: HttpStatusCodes[Status] = Http4sStatusCodes

  def emptyResponseHeaders: HNil => Headers = _ => Headers.empty

  def emptyResponse: EntityEncoder[F, HNil] = EntityEncoder.emptyEncoder[F, HNil]

  def cnil: CNil => Resp[F] = _ => Resp[F](Status.Forbidden, body = Stream.empty)

  def response[A, B](statusCode: HttpStatus, headers: HttpResponseHeaders[A], entity: HttpResponseEntity[B])(implicit T: Tupler[A, B]): T.Out => Resp[F] = out => {
    val (a,b) = T.unapply(out)
    val h = headers(a)
    val e = entity.toEntity(b).body

    Resp(statusCode, headers = h, body = e)
  }

  implicit val httpResponseResponseHeadersInvariantFunctor: Invariant[Lambda[A => Function[A, Headers]]] = new Invariant[Function[?, Headers]] {
    override def imap[A, B](fa: Function[A, Headers])(f: (A) => B)(g: (B) => A): Function[B, Headers] = b => fa(g(b))
  }
  implicit val httpResponseEntityInvariantFunctor: Invariant[EntityEncoder[F, ?]] = new Invariant[EntityEncoder[F, ?]] {
    override def imap[A, B](fa: EntityEncoder[F, A])(f: A => B)(g: B => A): EntityEncoder[F, B] = fa.contramap(g)
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

trait HttpHeaders[A] {

}
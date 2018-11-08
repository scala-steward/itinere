package itinere

import cats.Invariant
import cats.implicits._
import shapeless._

import scala.util.Try


trait HttpEndpointAlgebra extends HttpRequestAlgebra with HttpResponseAlgebra {
  type HttpRequest[A]
  type HttpResponse[A]
  type HttpEndpoint[A, B]

  def endpoint[A, B](request: HttpRequest[A], response: HttpResponse[B], description: Option[String] = None): HttpEndpoint[A, B]
}


trait UrlAlgebra {

  type QueryString[A]
  type Path[A] <: Url[A]
  type Url[A]

  implicit class QueryStringOps[A](first: QueryString[A]) {
    final def & [B](second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out] =
      combineQueryStrings(first, second)
  }

  def combineQueryStrings[A, B](first: QueryString[A], second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out]
  def qs[A](name: String, value: Read[A], description: Option[String] = None): QueryString[A]
  def optQs[A](name: String, value: Read[A], description: Option[String] = None): QueryString[Option[A]]

  implicit class PathOps[A](first: Path[A]) {
    final def / (second: String)(implicit tupler: Tupler[A, HNil]): Path[tupler.Out] = chainPaths(first, staticPathSegment(second))
    final def / [B](second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out] = chainPaths(first, second)
    final def /? [B](qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out] = urlWithQueryString(first, qs)
  }

  def staticPathSegment(segment: String): Path[HNil]
  def segment[A](name: String, value: Read[A], description: Option[String] = None): Path[A]
  def chainPaths[A, B](first: Path[A], second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out]
  val path: Path[HNil] = staticPathSegment("")
  def urlWithQueryString[A, B](path: Path[A], qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out]

  implicit val queryStringInvariantFunctor: Invariant[QueryString]
  implicit val pathInvariantFunctor: Invariant[Path]
  implicit val urlInvariantFunctor: Invariant[Url]
}

trait HttpResponseAlgebra {
  type HttpResponseHeaders[A]
  type HttpResponseEntity[A]
  type HttpResponse[A]

  final class CoproductHttpResponseBuilder[B <: Coproduct](coproduct: HttpResponse[B]) {
    def add[A](resp: HttpResponse[A]): CoproductHttpResponseBuilder[A :+: B] = {
      val newCoproduct = httpResponseCocartesian.sum(resp, coproduct).imap {
        case Left(l) => Inl(l)
        case Right(r) => Inr(r)
      } {
        case Inl(l) => Left(l)
        case Inr(r) => Right(r)
      }

      new CoproductHttpResponseBuilder(newCoproduct)
    }
    def as[A](implicit T: Transformer[HttpResponse, B, A]) = T(coproduct)
  }

  def coproductResponseBuilder = new CoproductHttpResponseBuilder(cnil)

  def emptyResponseHeaders: HttpResponseHeaders[HNil]

  def emptyResponse: HttpResponseEntity[HNil]
  def cnil: HttpResponse[CNil]

  def response[A, B](statusCode: Int, headers: HttpResponseHeaders[A] = emptyResponseHeaders, entity: HttpResponseEntity[B] = emptyResponse)(implicit T: Tupler[A, B]): HttpResponse[T.Out]

  implicit val httpResponseResponseHeadersInvariantFunctor: Invariant[HttpResponseHeaders]
  implicit val httpResponseEntityInvariantFunctor: Invariant[HttpResponseEntity]
  implicit val httpResponseInvariantFunctor: Invariant[HttpResponse]
  implicit val httpResponseCocartesian: CoCartesian[HttpResponse]
}

trait HttpRequestAlgebra extends UrlAlgebra {
  type HttpRequestHeaders[A]
  type HttpRequestEntity[A]
  type HttpRequest[A]

  type HttpMethod

  def GET: HttpMethod
  def PUT: HttpMethod
  def POST: HttpMethod
  def DELETE: HttpMethod
  def PATCH: HttpMethod

  implicit class RichHttpRequestHeaders[A](val left: HttpRequestHeaders[A]) {
    def ~[B](right: HttpRequestHeaders[B])(implicit T: Tupler[A, B]): HttpRequestHeaders[T.Out] =
      combineRequestHeaders(left, right)(T)
  }

  def requestHeader[A](name: String, value: Read[A], description: Option[String] = None): HttpRequestHeaders[A]

  def combineRequestHeaders[A, B](left: HttpRequestHeaders[A], right: HttpRequestHeaders[B])(implicit T: Tupler[A, B]): HttpRequestHeaders[T.Out]

  def emptyRequestHeaders: HttpRequestHeaders[HNil]
  def emptyRequestEntity: HttpRequestEntity[HNil]

  def request[A, B, C, AB](method: HttpMethod, url: Url[A], headers: HttpRequestHeaders[B] = emptyRequestHeaders, entity: HttpRequestEntity[C] = emptyRequestEntity)
                          (implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): HttpRequest[TO.Out]


  implicit val httpRequestHeadersInvariantFunctor: Invariant[HttpRequestHeaders]
  implicit val httpRequestEntityInvariantFunctor: Invariant[HttpRequestEntity]
  implicit val httpRequestInvariantFunctor: Invariant[HttpRequest]
}

trait JsonLike {
  def jsonDecoder[A](value: Json[A]): FromJson[A]
  def jsonEncoder[A](value: Json[A]): ToJson[A]
}

trait HttpJsonAlgebra { self: HttpRequestAlgebra with HttpResponseAlgebra =>
  def jsonResponse[A](json: Json[A], description: Option[String] = None): HttpResponseEntity[A]
  def jsonRequest[A](json: Json[A], description: Option[String] = None): HttpRequestEntity[A]
}


trait Read[A] {
  def fromString(string: String): Attempt[A]
}
object Read {
  def fromString[A](f: String => Attempt[A]): Read[A] = new Read[A] {
    override def fromString(string: String): Attempt[A] = f(string)
  }

  def fromTry[A](f: String => Try[A]): Read[A] = fromString(s => Attempt.fromTry(f(s)))

  val int: Read[Int] = fromTry(s => Try(s.toInt))
  val long: Read[Long] = fromTry(s => Try(s.toLong))
  val string: Read[String] = fromString(Attempt.success)

  implicit val readInstances: Partial[Read] with Invariant[Read] = new Partial[Read] with Invariant[Read] {
    override def pmap[A, B](fa: Read[A])(f: A => Attempt[B])(g: B => A): Read[B] = Read.fromString(str => fa.fromString(str).flatMap(f))
    override def imap[A, B](fa: Read[A])(f: A => B)(g: B => A): Read[B] = Read.fromString(str => fa.fromString(str).map(f))
  }
}

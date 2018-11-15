package itinere

import cats.{Invariant, Show}
import cats.implicits._
import shapeless._

trait HttpEndpointAlgebra extends HttpRequestAlgebra with HttpResponseAlgebra {
  type HttpRequest[A]
  type HttpResponse[A]
  type HttpEndpoint[A, B]

  def endpoint[A, B, C: Show](request: HttpRequest[A], response: HttpResponse[B], tag: C, summary: String): HttpEndpoint[A, B]

}

trait UrlAlgebra {
  type QueryString[A]
  type Path[A] <: Url[A]
  type Url[A]

  type QueryStringValue[A]
  type SegmentValue[A]

  protected def QueryStringValues: Primitives[QueryStringValue]
  protected def SegmentValues: Primitives[SegmentValue]

  implicit class QueryStringOps[A](first: QueryString[A]) {
    final def &[B](second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out] =
      combineQueryStrings(first, second)
  }

  def combineQueryStrings[A, B](first: QueryString[A], second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out]
  def qs[A](name: String, queryStringValue: Primitives[QueryStringValue] => QueryStringValue[A], description: Option[String] = None): QueryString[Option[A]]

  implicit class PathOps[A](first: Path[A]) {
    final def /(second: String)(implicit tupler: Tupler[A, HNil]): Path[tupler.Out] = chainPaths(first, staticPathSegment(second))
    final def /[B](second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out] = chainPaths(first, second)
    final def /?[B](qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out] = urlWithQueryString(first, qs)
  }

  def staticPathSegment(segment: String): Path[HNil]
  def segment[A](name: String, segment: Primitives[SegmentValue] => SegmentValue[A], description: Option[String] = None): Path[A]
  def chainPaths[A, B](first: Path[A], second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out]
  val path: Path[HNil] = staticPathSegment("")
  def urlWithQueryString[A, B](path: Path[A], qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out]

  implicit val queryStringInvariantFunctor: Invariant[QueryString]
  implicit val pathInvariantFunctor: Invariant[Path]
  implicit val urlInvariantFunctor: Invariant[Url]

  implicit val queryStringValueInvariant: Invariant[QueryStringValue]
  implicit val queryStringValuePartial: Partial[QueryStringValue]
  implicit val segmentInvariant: Invariant[SegmentValue]
  implicit val segmentPartial: Partial[SegmentValue]
}

trait HttpResponseAlgebra {
  type HttpResponseHeaders[A]
  type HttpResponseEntity[A]
  type HttpResponse[A]
  type HttpStatus

  val HttpStatus: HttpStatusCodes[HttpStatus]

  final class AnyOfResponse[B <: Coproduct](coproduct: HttpResponse[B]) {
    def opt[A](resp: HttpResponse[A]): AnyOfResponse[A :+: B] = {
      val newCoproduct = httpResponseCocartesian
        .sum(resp, coproduct)
        .imap {
          case Left(l)  => Inl(l)
          case Right(r) => Inr(r)
        } {
          case Inl(l) => Left(l)
          case Inr(r) => Right(r)
        }

      new AnyOfResponse(newCoproduct)
    }
    def as[A](implicit T: Transformer[HttpResponse, B, A]) = T(coproduct)
  }

  def anyOf = new AnyOfResponse(cnil)

  def emptyResponseHeaders: HttpResponseHeaders[HNil]

  def cnil: HttpResponse[CNil]

  def response[A, B](statusCode: HttpStatus, description: String, headers: HttpResponseHeaders[A] = emptyResponseHeaders, entity: HttpResponseEntity[B])(implicit T: Tupler[A, B]): HttpResponse[T.Out]

  implicit val httpResponseResponseHeadersInvariantFunctor: Invariant[HttpResponseHeaders]
  implicit val httpResponseEntityInvariantFunctor: Invariant[HttpResponseEntity]
  implicit val httpResponseInvariantFunctor: Invariant[HttpResponse]
  implicit val httpResponseCocartesian: CoCartesian[HttpResponse]
}

trait HttpRequestAlgebra extends UrlAlgebra {
  type HttpRequestHeaders[A]
  type HttpRequestEntity[A]
  type HttpRequest[A]
  type HttpRequestHeaderValue[A]
  type HttpMethod

  val HttpMethod: HttpMethods[HttpMethod]

  protected def HttpRequestHeaderValues: Primitives[HttpRequestHeaderValue]

  implicit class RichHttpRequestHeaders[A](val left: HttpRequestHeaders[A]) {
    def ~[B](right: HttpRequestHeaders[B])(implicit T: Tupler[A, B]): HttpRequestHeaders[T.Out] =
      combineRequestHeaders(left, right)(T)
  }

  def requestHeader[A](name: String, headerValue: Primitives[HttpRequestHeaderValue] => HttpRequestHeaderValue[A], description: Option[String] = None): HttpRequestHeaders[A]

  def combineRequestHeaders[A, B](left: HttpRequestHeaders[A], right: HttpRequestHeaders[B])(implicit T: Tupler[A, B]): HttpRequestHeaders[T.Out]

  def emptyRequestHeaders: HttpRequestHeaders[HNil]
  def emptyRequestEntity: HttpRequestEntity[HNil]

  def request[A, B, C, AB](method: HttpMethod, url: Url[A], headers: HttpRequestHeaders[B] = emptyRequestHeaders, entity: HttpRequestEntity[C] = emptyRequestEntity)(implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): HttpRequest[TO.Out]

  implicit val httpRequestHeadersInvariantFunctor: Invariant[HttpRequestHeaders]
  implicit val httpRequestEntityInvariantFunctor: Invariant[HttpRequestEntity]
  implicit val httpRequestInvariantFunctor: Invariant[HttpRequest]

  implicit val httpRequestHeaderValueInvariant: Invariant[HttpRequestHeaderValue]
  implicit val httpRequestHeaderValuePartial: Partial[HttpRequestHeaderValue]
}

trait JsonLike {
  def jsonDecoder[A](value: Json[A]): FromJson[A]
  def jsonEncoder[A](value: Json[A]): ToJson[A]
}

trait HttpJsonAlgebra { self: HttpRequestAlgebra with HttpResponseAlgebra =>
  def jsonResponse[A](json: Json[A]): HttpResponseEntity[A]
  def jsonRequest[A](json: Json[A]): HttpRequestEntity[A]
}

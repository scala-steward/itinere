package itinere

import cats.{Invariant, Show}
import cats.implicits._
import shapeless._

import scala.util.Try


trait HttpEndpointAlgebra extends HttpRequestAlgebra with HttpResponseAlgebra {
  type HttpRequest[A]
  type HttpResponse[A]
  type HttpEndpoint[A, B]

  def endpoint[A, B](request: HttpRequest[A], response: HttpResponse[B], description: Option[String] = None): HttpEndpoint[A, B]

  implicit class HttpRequestOps[A](req: HttpRequest[A]) {
    def ~>[B](resp: HttpResponse[B]): HttpEndpoint[A, B] = endpoint(req, resp)
  }
}

trait UrlAlgebra extends Primitives {
  type QueryString[A]
  type Path[A] <: Url[A]
  type Url[A]

  implicit class QueryStringOps[A](first: QueryString[A]) {
    final def & [B](second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out] =
      combineQueryStrings(first, second)
  }

  def combineQueryStrings[A, B](first: QueryString[A], second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out]
  def qs[A](name: String, description: Option[String] = None)(implicit QSV: Primitive[A]): QueryString[Option[A]]

  implicit class PathOps[A](first: Path[A]) {
    final def / (second: String)(implicit tupler: Tupler[A, HNil]): Path[tupler.Out] = chainPaths(first, staticPathSegment(second))
    final def / [B](second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out] = chainPaths(first, second)
    final def /? [B](qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out] = urlWithQueryString(first, qs)
  }

  def staticPathSegment(segment: String): Path[HNil]
  def segment[A](name: String, description: Option[String] = None)(implicit S: Primitive[A]): Path[A]
  def chainPaths[A, B](first: Path[A], second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out]
  val path: Path[HNil] = staticPathSegment("")
  def urlWithQueryString[A, B](path: Path[A], qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out]

  implicit val queryStringInvariantFunctor: Invariant[QueryString]
  implicit val pathInvariantFunctor: Invariant[Path]
  implicit val urlInvariantFunctor: Invariant[Url]
}

trait HttpMethods[A] {
  // $COVERAGE-OFF$Too many methods to test
  val ACL: A
  val `BASELINE-CONTROL`: A
  val BIND: A
  val CHECKIN: A
  val CHECKOUT: A
  val CONNECT: A
  val COPY: A
  val DELETE: A
  val GET: A
  val HEAD: A
  val LABEL: A
  val LINK: A
  val LOCK: A
  val MERGE: A
  val MKACTIVITY: A
  val MKCALENDAR: A
  val MKCOL: A
  val MKREDIRECTREF: A
  val MKWORKSPACE: A
  val MOVE: A
  val OPTIONS: A
  val ORDERPATCH: A
  val PATCH: A
  val POST: A
  val PROPFIND: A
  val PROPPATCH: A
  val PUT: A
  val REBIND: A
  val REPORT: A
  val SEARCH: A
  val TRACE: A
  val UNBIND: A
  val UNCHECKOUT: A
  val UNLINK: A
  val UNLOCK: A
  val UPDATE: A
  val UPDATEREDIRECTREF: A
  val `VERSION-CONTROL`: A
  // $COVERAGE-ON$
}

trait HttpStatusCodes[A] {
  // $COVERAGE-OFF$Too many codes to test
  val Continue: A
  val SwitchingProtocols: A
  val Processing: A
  val EarlyHints: A
  val Ok: A
  val Created: A
  val Accepted: A
  val NonAuthoritativeInformation: A
  val NoContent: A
  val ResetContent: A
  val PartialContent: A
  val MultiStatus: A
  val AlreadyReported: A
  val IMUsed: A
  val MultipleChoices: A
  val MovedPermanently: A
  val Found: A
  val SeeOther: A
  val NotModified: A
  val UseProxy: A
  val TemporaryRedirect: A
  val PermanentRedirect: A
  val BadRequest: A
  val Unauthorized: A
  val PaymentRequired: A
  val Forbidden: A
  val NotFound: A
  val MethodNotAllowed: A
  val NotAcceptable: A
  val ProxyAuthenticationRequired: A
  val RequestTimeout: A
  val Conflict: A
  val Gone: A
  val LengthRequired: A
  val PreconditionFailed: A
  val PayloadTooLarge: A
  val UriTooLong: A
  val UnsupportedMediaType: A
  val RangeNotSatisfiable: A
  val ExpectationFailed: A
  val MisdirectedRequest: A
  val UnprocessableEntity: A
  val Locked: A
  val FailedDependency: A
  val TooEarly: A
  val UpgradeRequired: A
  val PreconditionRequired: A
  val TooManyRequests: A
  val RequestHeaderFieldsTooLarge: A
  val UnavailableForLegalReasons: A
  val InternalServerError: A
  val NotImplemented: A
  val BadGateway: A
  val ServiceUnavailable: A
  val GatewayTimeout: A
  val HttpVersionNotSupported: A
  val VariantAlsoNegotiates: A
  val InsufficientStorage: A
  val LoopDetected: A
  val NotExtended: A
  val NetworkAuthenticationRequired: A
  // $COVERAGE-ON$
}

trait HttpResponseAlgebra {
  type HttpResponseHeaders[A]
  type HttpResponseEntity[A]
  type HttpResponse[A]
  type HttpStatus

  val HttpStatus: HttpStatusCodes[HttpStatus]

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

  def response[A, B](statusCode: HttpStatus, headers: HttpResponseHeaders[A] = emptyResponseHeaders, entity: HttpResponseEntity[B] = emptyResponse)(implicit T: Tupler[A, B]): HttpResponse[T.Out]

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

  val HttpMethod: HttpMethods[HttpMethod]

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

  def GET[A, B, AB](url: Url[A], headers: HttpRequestHeaders[B] = emptyRequestHeaders)
                      (implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, HNil]): HttpRequest[TO.Out] =
    request(HttpMethod.GET, url, headers, emptyRequestEntity)

  def POST[A, B, C, AB](url: Url[A], headers: HttpRequestHeaders[B] = emptyRequestHeaders, entity: HttpRequestEntity[C] = emptyRequestEntity)
                      (implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): HttpRequest[TO.Out] =
    request[A, B, C, AB](HttpMethod.POST, url, headers, entity)


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

trait Primitives {
  type Primitive[A]

  implicit def string: Primitive[String]
  implicit def int: Primitive[Int]
  implicit def long: Primitive[Long]

  implicit val primitiveInvariant: Invariant[Primitive]
  implicit val primitivePartial: Partial[Primitive]

}

trait ReadPrimitives extends Primitives {
  type Primitive[A] = Read[A]

  implicit def string: Read[String] = Read.string
  implicit def int: Read[Int] = Read.int
  implicit def long: Read[Long] = Read.long
  implicit val primitiveInvariant: Invariant[Read] = Read.readInvariant
  implicit val primitivePartial: Partial[Read] = Read.readPartial
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

  val readPartial: Partial[Read] = new Partial[Read] {
    override def pmap[A, B](fa: Read[A])(f: A => Attempt[B])(g: B => A): Read[B] = Read.fromString(str => fa.fromString(str).flatMap(f))
  }

  val readInvariant: Invariant[Read] = new Invariant[Read] {
    override def imap[A, B](fa: Read[A])(f: A => B)(g: B => A): Read[B] = Read.fromString(str => fa.fromString(str).map(f))
  }
}

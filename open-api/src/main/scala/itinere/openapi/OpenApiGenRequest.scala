package itinere.openapi
import cats.Invariant
import itinere.{Attempt, HttpMethods, HttpRequestAlgebra, Partial, Primitives, Tupler}

trait OpenApiGenRequest extends HttpRequestAlgebra with OpenApiGenUrls {
  type HttpRequestHeaders[A] = List[OpenApiParameter] => List[OpenApiParameter]
  type HttpRequestEntity[A] = OpenApiBodyEntity
  type HttpRequest[A] = OpenApiOperation
  type HttpMethod = String
  type HttpRequestHeaderValue[A] = OpenApiType

  val HttpMethod: HttpMethods[String] = OpenApiMethods

  protected def HttpRequestHeaderValues: Primitives[HttpRequestHeaderValue] = OpenApiTypes

  def requestHeader[A](headerName: String, headerValue: Primitives[HttpRequestHeaderValue] => OpenApiType, description: Option[String]): List[OpenApiParameter] => List[OpenApiParameter] =
    headers => headers :+ OpenApiParameter(name = headerName, in = OpenApiParameter.In.Header, required = true, description = description, `type` = headerValue(HttpRequestHeaderValues))

  def combineRequestHeaders[A, B](left: List[OpenApiParameter] => List[OpenApiParameter], right: List[OpenApiParameter] => List[OpenApiParameter])(implicit T: Tupler[A, B]): List[OpenApiParameter] => List[OpenApiParameter] =
    headers => left(right(headers))

  def emptyRequestHeaders: List[OpenApiParameter] => List[OpenApiParameter] = headers => headers
  def emptyRequestEntity: OpenApiBodyEntity = OpenApiBodyEntity.Empty

  def request[A, B, C, AB](method: HttpMethod, url: OpenApiPath, headers: List[OpenApiParameter] => List[OpenApiParameter], entity: OpenApiBodyEntity)(implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): OpenApiOperation =
    OpenApiOperation(method.toLowerCase(), url, requestBody = entity, parameters = headers(Nil))

  implicit override val httpRequestHeadersInvariantFunctor: Invariant[Lambda[A => Function[List[OpenApiParameter], List[OpenApiParameter]]]] = new Invariant[Lambda[A => Function[List[OpenApiParameter], List[OpenApiParameter]]]] {
    override def imap[A, B](fa: Function[List[OpenApiParameter], List[OpenApiParameter]])(f: A => B)(g: B => A): Function[List[OpenApiParameter], List[OpenApiParameter]] = fa
  }
  implicit override val httpRequestEntityInvariantFunctor: Invariant[Lambda[A => OpenApiBodyEntity]] = new Invariant[Lambda[A => OpenApiBodyEntity]] {
    override def imap[A, B](fa: OpenApiBodyEntity)(f: A => B)(g: B => A): OpenApiBodyEntity = fa
  }
  implicit override val httpRequestInvariantFunctor: Invariant[HttpRequest] = new Invariant[HttpRequest] {
    override def imap[A, B](fa: OpenApiOperation)(f: A => B)(g: B => A): OpenApiOperation = fa
  }
  implicit override val httpRequestHeaderValueInvariant: Invariant[Lambda[A => OpenApiType]] = new Invariant[Lambda[A => OpenApiType]] {
    override def imap[A, B](fa: OpenApiType)(f: A => B)(g: B => A): OpenApiType = fa
  }
  implicit override val httpRequestHeaderValuePartial: Partial[Lambda[A => OpenApiType]] = new Partial[Lambda[A => OpenApiType]] {
    override def pmap[A, B](fa: OpenApiType)(f: A => Attempt[B])(g: B => A): OpenApiType = fa
  }
}

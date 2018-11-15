package itinere.openapi
import cats.Invariant
import itinere.{CoCartesian, HttpResponseAlgebra, HttpStatusCodes, Tupler}

trait OpenApiGenResponse extends HttpResponseAlgebra {

  type HttpResponseHeaders[A] = List[OpenApiParameter] => List[OpenApiParameter]
  type HttpResponseEntity[A] = OpenApiBodyEntity
  type HttpResponse[A] = OpenApiResponses
  type HttpStatus = Int

  val HttpStatus: HttpStatusCodes[Int] = OpenApiStatusCodes

  def response[A, B](statusCode: Int, description: String, headers: List[OpenApiParameter] => List[OpenApiParameter], entity: OpenApiBodyEntity)(implicit T: Tupler[A, B]): OpenApiResponses =
    OpenApiResponses(Map(statusCode -> OpenApiResponse(description, Map.empty, entity)))

  def emptyResponseHeaders: List[OpenApiParameter] => List[OpenApiParameter] = headers => headers
  def cnil: OpenApiResponses = OpenApiResponses(Map.empty)

  implicit override val httpResponseCocartesian: CoCartesian[Lambda[A => OpenApiResponses]] = new CoCartesian[Lambda[A => OpenApiResponses]] {
    override def sum[A, B](fa: OpenApiResponses, fb: OpenApiResponses): OpenApiResponses = OpenApiResponses(fa.byStatusCode ++ fb.byStatusCode)
  }
  implicit override val httpResponseResponseHeadersInvariantFunctor: Invariant[Lambda[A => List[OpenApiParameter] => List[OpenApiParameter]]] =
    new Invariant[Lambda[A => List[OpenApiParameter] => List[OpenApiParameter]]] {
      override def imap[A, B](fa: List[OpenApiParameter] => List[OpenApiParameter])(f: (A) => B)(g: B => A): List[OpenApiParameter] => List[OpenApiParameter] = fa
    }
  implicit override val httpResponseEntityInvariantFunctor: Invariant[Lambda[A => OpenApiBodyEntity]] = new Invariant[Lambda[A => OpenApiBodyEntity]] {
    override def imap[A, B](fa: OpenApiBodyEntity)(f: (A) => B)(g: (B) => A): OpenApiBodyEntity = fa
  }
  implicit override val httpResponseInvariantFunctor: Invariant[Lambda[A => OpenApiResponses]] = new Invariant[Lambda[A => OpenApiResponses]] {
    override def imap[A, B](fa: OpenApiResponses)(f: (A) => B)(g: (B) => A): OpenApiResponses = fa
  }
}

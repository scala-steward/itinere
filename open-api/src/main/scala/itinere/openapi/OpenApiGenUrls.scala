package itinere.openapi
import cats.Invariant
import itinere.{Attempt, Partial, Primitives, Tupler, UrlAlgebra}

trait OpenApiGenUrls extends UrlAlgebra {

  override type QueryString[A] = List[OpenApiParameter]
  override type QueryStringValue[A] = OpenApiType
  override type SegmentValue[A] = OpenApiType
  override type Path[A] = OpenApiPath
  override type Url[A] = OpenApiPath

  protected def QueryStringValues: OpenApiTypes.type = OpenApiTypes
  protected def SegmentValues: OpenApiTypes.type = OpenApiTypes

  override def segment[A](name: String, segment: Primitives[SegmentValue] => OpenApiType, description: Option[String]): OpenApiPath =
    OpenApiPath(s"{$name}", List(OpenApiParameter.path(name, description, segment(SegmentValues))))

  override def qs[A](name: String, queryStringValue: Primitives[QueryStringValue] => OpenApiType, description: Option[String]): List[OpenApiParameter] =
    List(OpenApiParameter.query(name, description, queryStringValue(QueryStringValues)))

  override def combineQueryStrings[A, B](first: List[OpenApiParameter], second: List[OpenApiParameter])(implicit tupler: Tupler[A, B]): List[OpenApiParameter] = first ++ second

  override def staticPathSegment(segment: String): OpenApiPath = OpenApiPath(segment)

  override def chainPaths[A, B](first: OpenApiPath, second: OpenApiPath)(implicit tupler: Tupler[A, B]): OpenApiPath =
    OpenApiPath(s"${first.id}/${second.id}", first.parameters ++ second.parameters)

  override def urlWithQueryString[A, B](path: OpenApiPath, qs: List[OpenApiParameter])(implicit tupler: Tupler[A, B]): OpenApiPath = path.copy(parameters = path.parameters ++ qs)

  implicit override val queryStringInvariantFunctor: Invariant[Lambda[A => List[OpenApiParameter]]] = new Invariant[Lambda[A => List[OpenApiParameter]]] {
    override def imap[A, B](fa: List[OpenApiParameter])(f: (A) => B)(g: (B) => A): List[OpenApiParameter] = fa
  }
  implicit override val pathInvariantFunctor: Invariant[Lambda[A => OpenApiPath]] = new Invariant[Lambda[A => OpenApiPath]] {
    override def imap[A, B](fa: OpenApiPath)(f: (A) => B)(g: (B) => A): OpenApiPath = fa
  }
  implicit override val urlInvariantFunctor: Invariant[Lambda[A => OpenApiPath]] = new Invariant[Lambda[A => OpenApiPath]] {
    override def imap[A, B](fa: OpenApiPath)(f: (A) => B)(g: (B) => A): OpenApiPath = fa
  }
  implicit val queryStringValueInvariant: Invariant[Lambda[A => OpenApiType]] = new Invariant[Lambda[A => OpenApiType]] {
    override def imap[A, B](fa: OpenApiType)(f: A => B)(g: B => A): OpenApiType = fa
  }
  implicit val queryStringValuePartial: Partial[Lambda[A => OpenApiType]] = new Partial[Lambda[A => OpenApiType]] {
    override def pmap[A, B](fa: OpenApiType)(f: A => Attempt[B])(g: B => A): OpenApiType = fa
  }
  implicit val segmentInvariant: Invariant[Lambda[A => OpenApiType]] = new Invariant[Lambda[A => OpenApiType]] {
    override def imap[A, B](fa: OpenApiType)(f: A => B)(g: B => A): OpenApiType = fa
  }
  implicit val segmentPartial: Partial[Lambda[A => OpenApiType]] = new Partial[Lambda[A => OpenApiType]] {
    override def pmap[A, B](fa: OpenApiType)(f: A => Attempt[B])(g: B => A): OpenApiType = fa
  }
}

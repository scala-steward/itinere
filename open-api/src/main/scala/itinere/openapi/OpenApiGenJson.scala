package itinere.openapi
import itinere.{HttpJsonAlgebra, Json => J}

trait OpenApiGenJson extends HttpJsonAlgebra { self: OpenApiGen =>
  override def jsonResponse[A](json: J[A]): HttpResponseEntity[A] =
    OpenApiBodyEntity.Json(json.apply[ToJsonSchema].schema)

  override def jsonRequest[A](json: J[A]): HttpRequestEntity[A] =
    OpenApiBodyEntity.Json(json.apply[ToJsonSchema].schema)
}

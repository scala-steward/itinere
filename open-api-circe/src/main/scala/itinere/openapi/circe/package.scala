package itinere.openapi

import io.circe.{Json => J}
import qq.droste.{Algebra, scheme}

package object circe {
  private val asJson: Algebra[JsonF, J] = Algebra {
    case JsonF.Object(fields) => J.fromFields(fields.toList)
    case JsonF.Null           => J.Null
    case JsonF.Str(value)     => J.fromString(value)
    case JsonF.Bool(value)    => J.fromBoolean(value)
    case JsonF.Number(value)  => J.fromBigDecimal(value.bigDecimal)
    case JsonF.Array(values)  => J.fromValues(values)
  }

  def openApiToJson(openApiRoot: OpenApiRoot): J =
    scheme.cata(asJson).apply(OpenApiRoot.toJson(openApiRoot))
}

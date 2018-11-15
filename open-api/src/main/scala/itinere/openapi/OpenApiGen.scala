package itinere.openapi

import cats.Show
import cats.data.Writer
import cats.implicits._
import itinere.HttpEndpointAlgebra

class OpenApiGen extends HttpEndpointAlgebra with OpenApiGenUrls with OpenApiGenRequest with OpenApiGenResponse {
  override type HttpEndpoint[A, B] = OpenApiOperation

  override def endpoint[A, B, C: Show](request: OpenApiOperation, response: OpenApiResponses, tag: C, summary: String): OpenApiOperation =
    request.copy(responses = response, summary = Some(summary), tag = Some(Show[C].show(tag)))
}
object OpenApiGen {
  def api(info: OpenApiInfo, servers: Set[OpenApiServer])(ops: OpenApiOperation*): OpenApiRoot = {
    val (manifest, operations) = ops.toList.traverse[Writer[JsonSchemaManifest, ?], OpenApiOperation](_.toReferenceTree).run

    OpenApiRoot(info, operations, manifest, servers)
  }
}

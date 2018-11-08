package itinere.http4s_server

import itinere.{FromJson, HttpJsonAlgebra, Json, JsonLike}
import fs2._
import fs2.text
import cats.implicits._

trait Http4sServerJson extends HttpJsonAlgebra { self: Http4sServer with JsonLike =>
  override def jsonResponse[A](
    json: Json[A],
    description: Option[String]
  ): HttpResponseEntity[A] = new HttpResponseEntity[A] {
    override def apply(entity: A): fs2.Stream[F, Byte] =
      Stream[F, String](jsonEncoder(json).encode(entity)) through text.utf8Encode
  }


  override def jsonRequest[A](json: Json[A], description: Option[String]): HttpRequestEntity[A] = new HttpRequestEntity[A] {
    override def decode(stream: fs2.Stream[F, Byte]): F[Either[String, A]] =
      F.map(stream.through(text.utf8Decode).compile.foldMonoid)(t => jsonDecoder(json).decode(t).toEither)
  }
}

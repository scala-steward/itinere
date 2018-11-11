package itinere.http4s_server

import cats.data.EitherT
import cats.implicits._
import fs2.text
import itinere.{Attempt, HttpJsonAlgebra, Json, JsonLike}
import org.http4s._

trait Http4sServerJson extends HttpJsonAlgebra { self: Http4sServer with JsonLike =>

  override def jsonResponse[A](json: Json[A], description: Option[String]): HttpResponseEntity[A] =
    EntityEncoder.stringEncoder[F].contramap(jsonEncoder(json).encode(_))

  override def jsonRequest[A](json: Json[A], description: Option[String]): HttpRequestEntity[A] = new HttpRequestEntity[A] {
    override def decode(msg: Message[F], strict: Boolean): DecodeResult[F, A] = EitherT {
      F.map(msg.body.through(text.utf8Decode).compile.foldMonoid)(
        t =>
          jsonDecoder(json).decode(t) match {
            case Attempt.Success(value) =>
              Right(value)
            case Attempt.Error(err, cause) =>
              Left(MalformedMessageBodyFailure(err, cause))
        }
      )
    }

    override def consumes: Set[MediaRange] = Set(MediaRange.`application/*`)
  }
}

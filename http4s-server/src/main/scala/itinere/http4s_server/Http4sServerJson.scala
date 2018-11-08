package itinere.http4s_server

import cats.data.EitherT
import cats.implicits._
import fs2.{text, _}
import itinere.{Attempt, HttpJsonAlgebra, Json, JsonLike}
import org.http4s.{DecodeResult, MalformedMessageBodyFailure, MediaRange, Message}

trait Http4sServerJson extends HttpJsonAlgebra { self: Http4sServer with JsonLike =>
  override def jsonResponse[A](
    json: Json[A],
    description: Option[String]
  ): HttpResponseEntity[A] = new HttpResponseEntity[A] {
    override def apply(entity: A): fs2.Stream[F, Byte] =
      Stream[F, String](jsonEncoder(json).encode(entity)) through text.utf8Encode
  }


  override def jsonRequest[A](json: Json[A], description: Option[String]): HttpRequestEntity[A] = new HttpRequestEntity[A] {
    override def decode(msg: Message[F], strict: Boolean): DecodeResult[F, A] = EitherT {
      F.map(msg.body.through(text.utf8Decode).compile.foldMonoid)(t => jsonDecoder(json).decode(t) match {
        case Attempt.Success(value)   =>
          Right(value)
        case Attempt.Exception(err)   =>
          Left(MalformedMessageBodyFailure("Error while decoding json", Some(err)))
        case Attempt.Error(err)       =>
          Left(MalformedMessageBodyFailure(err, None))
      })
    }

    override def consumes
      : Set[MediaRange] = Set(MediaRange.`application/*`)
  }
}

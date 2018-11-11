package itinere.circe

import java.util.UUID

import cats.implicits._
import io.circe.{Decoder, Encoder}
import itinere._
import shapeless.CNil

trait CirceJsonLike extends JsonLike {
  private implicit val decoder: JsonAlgebra[Decoder] = new JsonAlgebra[Decoder] with CirceDecoderObjectN {

    override def int(bound: Range): Decoder[Int] = Decoder.decodeInt

    override def float(bound: Range): Decoder[Float] = Decoder.decodeFloat

    override def double(bound: Range): Decoder[Double] = Decoder.decodeDouble

    override def long(bound: Range): Decoder[Long] = Decoder.decodeLong

    override def string(description: StringDescriptor): Decoder[String] = Decoder.decodeString

    override val bool: Decoder[Boolean] = Decoder.decodeBoolean

    override val cnil: Decoder[CNil] = Decoder.const(null)

    override val uuid: Decoder[UUID] = Decoder.decodeUUID

    override def option[A](from: Decoder[A]): Decoder[Option[A]] = Decoder.decodeOption(from)

    override def list[A](of: Decoder[A]): Decoder[List[A]] = Decoder.decodeList(of)

    override def set[A](of: Decoder[A]): Decoder[Set[A]] = Decoder.decodeSet(of)

    override def vector[A](of: Decoder[A]): Decoder[Vector[A]] = Decoder.decodeVector(of)

    override def seq[A](of: Decoder[A]): Decoder[Seq[A]] = Decoder.decodeSeq(of)

    override def pmap[A, B](fa: Decoder[A])(f: A => Attempt[B])(g: B => A): Decoder[B] = fa.emap(a => f(a).toEither)

    override def imap[A, B](fa: Decoder[A])(f: A => B)(g: B => A): Decoder[B] = fa.map(f)

    override def sum[A, B](fa: Decoder[A], fb: Decoder[B]): Decoder[Either[A, B]] = fa.map(Left.apply) or fb.map(Right.apply)
  }

  private implicit val encoder: JsonAlgebra[Encoder] = new JsonAlgebra[Encoder] with CirceEncoderObjectN {
    override def int(bound: Range): Encoder[Int] = Encoder.encodeInt

    override def float(bound: Range): Encoder[Float] = Encoder.encodeFloat

    override def double(bound: Range): Encoder[Double] = Encoder.encodeDouble

    override def long(bound: Range): Encoder[Long] = Encoder.encodeLong

    override def string(description: StringDescriptor): Encoder[String] = Encoder.encodeString

    override val bool: Encoder[Boolean] = Encoder.encodeBoolean

    override val cnil: Encoder[CNil] = new Encoder[CNil] {
      override def apply(a: CNil): io.circe.Json = io.circe.Json.Null
    }

    override val uuid: Encoder[UUID] = Encoder.encodeUUID

    override def option[A](from: Encoder[A]): Encoder[Option[A]] = Encoder.encodeOption(from)

    override def list[A](of: Encoder[A]): Encoder[List[A]] = Encoder.encodeList(of)

    override def set[A](of: Encoder[A]): Encoder[Set[A]] = Encoder.encodeSet(of)

    override def vector[A](of: Encoder[A]): Encoder[Vector[A]] = Encoder.encodeVector(of)

    override def seq[A](of: Encoder[A]): Encoder[Seq[A]] = Encoder.encodeSeq(of)

    override def pmap[A, B](fa: Encoder[A])(f: A => Attempt[B])(g: B => A): Encoder[B] = fa.contramap(g)

    override def imap[A, B](fa: Encoder[A])(f: A => B)(g: B => A): Encoder[B] = fa.contramap(g)
    override def sum[A, B](
      fa: Encoder[A],
      fb: Encoder[B]
    ): Encoder[Either[A, B]] = new Encoder[Either[A, B]] {
      override def apply(a: Either[A, B]): io.circe.Json = a match {
        case Left(left)   => fa(left)
        case Right(right) => fb(right)
      }
    }
  }

  override def jsonDecoder[A](json: Json[A]): FromJson[A] = new FromJson[A] {
    override def decode(input: String): Attempt[A] =
      for {
        jsonValue <- Attempt.fromThrowable(io.circe.parser.parse(input))
        value     <- Attempt.fromThrowable(json.apply[Decoder].decodeJson(jsonValue))
      } yield value
  }

  override def jsonEncoder[A](json: Json[A]): ToJson[A] = new ToJson[A] {
    override def encode(value: A): String = json.apply[Encoder].apply(value).noSpaces
  }
}

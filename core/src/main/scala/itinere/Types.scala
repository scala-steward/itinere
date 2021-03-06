package itinere

import cats.~>

trait CoCartesian[F[_]] {
  def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
}

trait Partial[F[_]] {
  def pmap[A, B](fa: F[A])(f: A => Attempt[B])(g: B => A): F[B]
}

trait FromJson[A] {
  def decode(input: String): Attempt[A]
}

trait ToJson[A] {
  def encode(value: A): String
}

final case class Member[F[_], A, B] private (
  fa: F[A],
  getter: B => A,
  documentation: Option[String]
) {
  def transform[G[_]](f: F ~> G): Member[G, A, B] = copy(fa = f(fa))
}

sealed trait LengthBound

object LengthBound {
  case object Unbounded extends LengthBound
  case class Atmost(value: Int) extends LengthBound
  case class Atleast(value: Int) extends LengthBound
  case class Exact(value: Int) extends LengthBound
  case class Interval(low: Int, high: Int) extends LengthBound
}

case class Bound(value: BigDecimal, inclusive: Boolean)

case class Range(lower: Bound, upper: Bound)

sealed abstract class IntegerType(val format: String)

object IntegerType {
  case object Int32 extends IntegerType("int32")
  case object Int64 extends IntegerType("int64")
}

sealed abstract class NumberType(val format: String)

object NumberType {
  case object Float extends NumberType("float")
  case object Double extends NumberType("double")
}

sealed abstract class StringType(val format: String)

object StringType {
  case object DateTime extends StringType("date-time")
  case object Email extends StringType("email")
  case object Hostname extends StringType("hostname")
  case object IPV4 extends StringType("ipv4")
  case object IPV6 extends StringType("ipv6")
  case object Uri extends StringType("uri")
}

sealed trait StringDescriptor

object StringDescriptor {
  case class Type(stringType: StringType) extends StringDescriptor
  case class Length(lengthBound: LengthBound) extends StringDescriptor
  case class Pattern(pattern: String) extends StringDescriptor
  case object Unbounded extends StringDescriptor
}

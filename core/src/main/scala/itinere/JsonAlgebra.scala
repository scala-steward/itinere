package itinere

import java.time.Instant
import java.util.UUID

import cats.Invariant
import shapeless.CNil

trait JsonAlgebra[F[_]] extends JsonAlgebraFormatN[F] with Invariant[F] with Partial[F] with CoCartesian[F] {
  def int(bound: Range): F[Int]
  def float(bound: Range): F[Float]
  def double(bound: Range): F[Double]
  def long(bound: Range): F[Long]

  def string(description: StringDescription): F[String]
  val bool: F[Boolean]
  val cnil: F[CNil]

  val uuid: F[UUID]
  val instant: F[Instant]

  def option[A](from: F[A]): F[Option[A]]
  def list[A](of: F[A]): F[List[A]]
  def set[A](of: F[A]): F[Set[A]]
  def vector[A](of: F[A]): F[Vector[A]]
  def seq[A](of: F[A]): F[Seq[A]]
}

object JsonAlgebra {
  def apply[F[_]](implicit F: JsonAlgebra[F]): JsonAlgebra[F] = F
}

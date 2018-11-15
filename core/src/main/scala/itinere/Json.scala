package itinere

import cats.Invariant
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}

trait Json[A] {
  def apply[F[_]: JsonAlgebra]: F[A]
}

object Json extends JsonDslFormatN { self =>

  val int: Json[Int] = new Json[Int] {
    override def apply[F[_]: JsonAlgebra]: F[Int] = implicitly[JsonAlgebra[F]].int(Range(Bound(Int.MinValue, true), Bound(Int.MaxValue, true)))
  }

  val string: Json[String] = new Json[String] {
    override def apply[F[_]: JsonAlgebra]: F[String] = implicitly[JsonAlgebra[F]].string(StringDescriptor.Unbounded)
  }

  val bool: Json[Boolean] = new Json[Boolean] {
    override def apply[F[_]: JsonAlgebra]: F[Boolean] = implicitly[JsonAlgebra[F]].bool
  }

  val long: Json[Long] = new Json[Long] {
    override def apply[F[_]: JsonAlgebra]: F[Long] = implicitly[JsonAlgebra[F]].long(Range(Bound(Long.MinValue, true), Bound(Long.MaxValue, true)))
  }

  val double: Json[Double] = new Json[Double] {
    override def apply[F[_]: JsonAlgebra]: F[Double] = implicitly[JsonAlgebra[F]].double(Range(Bound(Double.MinValue, true), Bound(Double.MaxValue, true)))
  }

  val float: Json[Float] = new Json[Float] {
    override def apply[F[_]: JsonAlgebra]: F[Float] = implicitly[JsonAlgebra[F]].float(Range(Bound(BigDecimal(Float.MinValue.toDouble), true), Bound(BigDecimal(Float.MaxValue.toDouble), true)))
  }

  val cnil: Json[CNil] = new Json[CNil] {
    override def apply[F[_]: JsonAlgebra]: F[CNil] = implicitly[JsonAlgebra[F]].cnil
  }

  def imap[A, B](fa: Json[A])(f: A => B)(g: B => A): Json[B] = new Json[B] {
    override def apply[F[_]: JsonAlgebra]: F[B] = implicitly[JsonAlgebra[F]].imap(fa.apply[F])(f)(g)
  }

  def pmap[A, B](fa: Json[A])(f: A => Attempt[B])(g: B => A): Json[B] = new Json[B] {
    override def apply[F[_]: JsonAlgebra]: F[B] = implicitly[JsonAlgebra[F]].pmap(fa.apply[F])(f)(g)
  }

  def option[A](value: Json[A]): Json[Option[A]] = new Json[Option[A]] {
    override def apply[F[_]: JsonAlgebra]: F[Option[A]] = implicitly[JsonAlgebra[F]].option(value.apply[F])
  }

  def list[A](of: Json[A]): Json[List[A]] = new Json[List[A]] {
    override def apply[F[_]: JsonAlgebra]: F[List[A]] = implicitly[JsonAlgebra[F]].list(of.apply[F])
  }

  def set[A](of: Json[A]): Json[Set[A]] = new Json[Set[A]] {
    override def apply[F[_]: JsonAlgebra]: F[Set[A]] = implicitly[JsonAlgebra[F]].set(of.apply[F])
  }

  def vector[A](of: Json[A]): Json[Vector[A]] = new Json[Vector[A]] {
    override def apply[F[_]: JsonAlgebra]: F[Vector[A]] = implicitly[JsonAlgebra[F]].vector(of.apply[F])
  }

  def seq[A](of: Json[A]): Json[Seq[A]] = new Json[Seq[A]] {
    override def apply[F[_]: JsonAlgebra]: F[Seq[A]] = implicitly[JsonAlgebra[F]].seq(of.apply[F])
  }

  def or[A, B](fa: Json[A], fb: Json[B]): Json[Either[A, B]] = new Json[Either[A, B]] {
    override def apply[F[_]: JsonAlgebra]: F[Either[A, B]] = implicitly[JsonAlgebra[F]].sum(fa.apply[F], fb.apply[F])
  }

  implicit class RichJason[A](val fa: Json[A]) {
    def imap[B](f: A => B)(g: B => A): Json[B] = self.imap(fa)(f)(g)
    def pmap[B](f: A => Attempt[B])(g: B => A): Json[B] = self.pmap(fa)(f)(g)
    def |[B](fb: Json[B]): UnionBuilder[A :+: B :+: CNil] =
      new UnionBuilder[CNil](cnil).add(fb).add(fa)
  }

  implicit val invariantFunctor: Invariant[Json] = new Invariant[Json] {
    override def imap[A, B](fa: Json[A])(f: A => B)(g: B => A): Json[B] = self.imap(fa)(f)(g)
  }

  final class UnionBuilder[B <: Coproduct](fb: Json[B]) {

    def add[A](fa: Json[A]): UnionBuilder[A :+: B] = {
      val coproduct = imap(or(fa, fb)) {
        case Left(l)  => Inl(l)
        case Right(r) => Inr(r)
      } {
        case Inl(l) => Left(l)
        case Inr(r) => Right(r)
      }

      new UnionBuilder(coproduct)
    }

    def |[A](fa: Json[A]): UnionBuilder[A :+: B] = add(fa)

    def as[A](implicit T: Transformer[Json, B, A]): Json[A] = T(fb)
  }
}

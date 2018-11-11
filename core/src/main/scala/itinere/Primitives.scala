package itinere
import cats.Invariant

import scala.util.Try
import cats.implicits._

trait Primitives[F[_]] {
  def string: F[String]
  def int: F[Int]
  def long: F[Long]
}

object ReadPrimitives extends Primitives[Read] {
  def string: Read[String] = Read.string
  def int: Read[Int] = Read.int
  def long: Read[Long] = Read.long
}

//object ShowPrimitives extends Primitives[Show] {
//  override implicit def string: Show[String] = Show[String]
//  override implicit def int: Show[Int] = Show[Int]
//  override implicit def long: Show[Long] = Show[Long]
//
//  override implicit val primitiveInvariant: Invariant[Show] = new Invariant[Show] {
//    override def imap[A, B](fa: Show[A])(f: A => B)(g: B => A): Show[B] = fa.contramap(g)
//  }
//  override implicit val primitivePartial: Partial[Show] = new Partial[Show] {
//    override def pmap[A, B](fa: Show[A])(f: A => Attempt[B])(g: B => A): Show[B] = fa.contramap(g)
//  }
//}

trait Read[A] {
  def fromString(string: String): Attempt[A]
}
object Read {
  def fromString[A](f: String => Attempt[A]): Read[A] = new Read[A] {
    override def fromString(string: String): Attempt[A] = f(string)
  }

  def fromTry[A](f: String => Try[A]): Read[A] = fromString(s => Attempt.fromTry(f(s)))

  val int: Read[Int] = fromTry(s => Try(s.toInt))
  val long: Read[Long] = fromTry(s => Try(s.toLong))
  val string: Read[String] = fromString(Attempt.success)

  val readPartial: Partial[Read] = new Partial[Read] {
    override def pmap[A, B](fa: Read[A])(f: A => Attempt[B])(g: B => A): Read[B] = Read.fromString(str => fa.fromString(str).flatMap(f))
  }

  val readInvariant: Invariant[Read] = new Invariant[Read] {
    override def imap[A, B](fa: Read[A])(f: A => B)(g: B => A): Read[B] = Read.fromString(str => fa.fromString(str).map(f))
  }
}

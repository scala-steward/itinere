package itinere
import cats.Monad

import scala.util.Try

sealed trait Attempt[+A] { self =>
  def toEither: Either[String, A] = self match {
    case Attempt.Success(value) => Right(value)
    case Attempt.Exception(ex) => Left(ex.getMessage)
    case Attempt.Error(err) => Left(err)
  }
}

object Attempt {
  final case class Success[A](value: A) extends Attempt[A]
  final case class Exception(error: Throwable) extends Attempt[Nothing]
  final case class Error(error: String) extends Attempt[Nothing]

  def exception(ex: Throwable): Attempt[Nothing] = Attempt.Exception(ex)
  def error(msg: String): Attempt[Nothing] = Attempt.Error(msg)
  def success[A](value: A): Attempt[A] = Attempt.Success(value)

  def fromTry[A](t: Try[A]): Attempt[A] = t match {
    case scala.util.Failure(err) => exception(err)
    case scala.util.Success(value) => success(value)
  }

  def fromEither[L, R](t: Either[String, R]): Attempt[R] = t match {
    case Left(err) => error(err)
    case Right(value) => success(value)
  }

  def fromOption[A](option: Option[A], ifEmpty: String): Attempt[A] = option match {
    case None => error(ifEmpty)
    case Some(value) => success(value)
  }

  def fromThrowable[L, R](t: Either[Throwable, R]): Attempt[R] = t match {
    case Left(err) => exception(err)
    case Right(value) => success(value)
  }

  implicit val monad: Monad[Attempt] = new Monad[Attempt] {
    override def flatMap[A, B](fa: Attempt[A])(f: A => Attempt[B]): Attempt[B] = fa match {
      case Attempt.Success(value)   => f(value)
      case Attempt.Exception(err) => exception(err)
      case Attempt.Error(err)     => error(err)
    }

    override def tailRecM[A, B](a: A)(f: A => Attempt[Either[A, B]]): Attempt[B] = f(a) match {
      case Attempt.Success(Left(l))   => tailRecM(l)(f)
      case Attempt.Success(Right(r)) => success(r)
      case Attempt.Exception(err) => exception(err)
      case Attempt.Error(err)     => error(err)
    }

    override def pure[A](x: A): Attempt[A] = success(x)
  }
}

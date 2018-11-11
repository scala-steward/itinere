package itinere
import cats.{Eq, Monad}

import scala.util.Try

sealed trait Attempt[+A] { self =>
  def toEither: Either[String, A] = self match {
    case Attempt.Success(value) => Right(value)
    case Attempt.Error(err, _)  => Left(err)
  }
}

object Attempt {
  final case class Success[A](value: A) extends Attempt[A]
  final case class Error(error: String, cause: Option[Throwable]) extends Attempt[Nothing]

  def error(msg: String, cause: Option[Throwable]): Attempt[Nothing] = Attempt.Error(msg, cause)
  def success[A](value: A): Attempt[A] = Attempt.Success(value)

  def fromTry[A](t: Try[A]): Attempt[A] = t match {
    case scala.util.Failure(err)   => error("Computation failed", Some(err))
    case scala.util.Success(value) => success(value)
  }

  def fromEither[L, R](t: Either[String, R]): Attempt[R] = t match {
    case Left(err)    => error(err, None)
    case Right(value) => success(value)
  }

  def fromOption[A](option: Option[A], ifEmpty: String): Attempt[A] = option match {
    case None        => error(ifEmpty, None)
    case Some(value) => success(value)
  }

  def fromThrowable[L, R](t: Either[Throwable, R]): Attempt[R] = t match {
    case Left(err)    => error(err.getMessage, Some(err))
    case Right(value) => success(value)
  }

  implicit def eq[A: Eq]: Eq[Attempt[A]] = Eq.instance {
    case (Success(left), Success(right)) => Eq[A].eqv(left, right)
    case (Error(leftMsg, Some(leftCause)), Error(rightMsg, Some(rightCause))) =>
      leftMsg == rightMsg && leftCause.getMessage == rightCause.getMessage
    case (Error(leftMsg, None), Error(rightMsg, None)) =>
      leftMsg == rightMsg
    case _ => false
  }

  implicit val monad: Monad[Attempt] = new Monad[Attempt] {
    override def flatMap[A, B](fa: Attempt[A])(f: A => Attempt[B]): Attempt[B] = fa match {
      case Attempt.Success(value)    => f(value)
      case Attempt.Error(err, cause) => Error(err, cause)
    }

    override def tailRecM[A, B](a: A)(f: A => Attempt[Either[A, B]]): Attempt[B] = f(a) match {
      case Attempt.Success(Left(l))  => tailRecM(l)(f)
      case Attempt.Success(Right(r)) => Success(r)
      case Attempt.Error(err, cause) => Error(err, cause)
    }

    override def pure[A](x: A): Attempt[A] = success(x)
  }
}

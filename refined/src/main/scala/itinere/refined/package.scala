package itinere

import eu.timepit.refined.api.{RefType, Refined, Validate}
import eu.timepit.refined.numeric.Positive


package object refined {

  implicit class RichIntJson(val Json: Json[Int]) {
    private def withRefined[P](bound: Range)(implicit V: Validate[Int, P], R: RefType[Refined]): Json[Int Refined P] =
      new Json[Refined[Int, P]] {
        override def apply[F[_] : JsonAlgebra]: F[Refined[Int, P]] =
          JsonAlgebra[F].pmap(JsonAlgebra[F].int(bound))(p => Attempt.fromEither(R.refine(p)))(R.unwrap)
      }

    def positive[N <: Int](implicit V: Validate.Plain[Int, Positive]): Json[Int Refined Positive] =
      withRefined(Range(Bound(1, true), Bound(Int.MaxValue, true)))
  }

  implicit class RichLongJson(val Json: Json[Long]) {
    private def withRefined[P](bound: Range)(implicit V: Validate[Long, P], R: RefType[Refined]): Json[Long Refined P] =
      new Json[Refined[Long, P]] {
        override def apply[F[_] : JsonAlgebra]: F[Refined[Long, P]] =
          JsonAlgebra[F].pmap(JsonAlgebra[F].long(bound))(p => Attempt.fromEither(R.refine(p)))(R.unwrap)
      }

    def positive[N <: Int](implicit V: Validate.Plain[Int, Positive]): Json[Long Refined Positive] =
      withRefined(Range(Bound(1l, true), Bound(Long.MaxValue, true)))
  }
}

trait RefinedPrimitives { self: HttpEndpointAlgebra =>
  implicit class RefinedSyntax[F[_], A](fa: F[A])(implicit P: Partial[F]) {
    def refined[P](implicit V: Validate[A, P], R: RefType[Refined]): F[Refined[A, P]] =
      P.pmap(fa)(p => Attempt.fromThrowable(R.refine(p).left.map(err => new Throwable(err))))(R.unwrap)
  }
}

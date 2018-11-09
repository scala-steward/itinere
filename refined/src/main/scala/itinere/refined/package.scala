package itinere

import eu.timepit.refined.api.{RefType, Refined, Validate}
import eu.timepit.refined.numeric.Positive

package object refined {

//  implicit class RichStringJson(val Json: Json[String]) {
//
//    private def withRefined[P](description: StringDescription)(implicit V: Validate[String, P], R: RefType[Refined]): Json[String Refined P] =
//      new Json[Refined[String, P]] {
//        override def apply[F[_] : JsonAlgebra]: F[Refined[String, P]] =
//          JsonAlgebra[F].pmap(JsonAlgebra[F].string(description))(p => Attempt.fromEither(R.refine(p)))(R.unwrap)
//      }
//
//    def nonEmpty[S <: String](implicit V: Validate.Plain[String, NonEmpty], R: RefType[Refined], S: Witness.Aux[S]): Json[String Refined NonEmpty] =
//      withRefined(StringDescription.Length(LengthBound.Atleast(1)))
//  }

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

trait RefinedPrimitives extends Primitives {
  implicit def refined[A, P](implicit P: Primitive[A], V: Validate[A, P], R: RefType[Refined]): Primitive[A Refined P] =
    P.pmap(p => Attempt.fromThrowable(R.refine(p).left.map(err => new Throwable(err))))(R.unwrap)
}

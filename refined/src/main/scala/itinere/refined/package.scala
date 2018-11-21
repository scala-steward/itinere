package itinere

import eu.timepit.refined.api.{RefType, Refined, Validate}
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.collection.{Empty, MaxSize, MinSize, Size}
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.Interval.{ClosedOpen, OpenClosed}
import eu.timepit.refined.numeric.{Interval, Negative, Positive}
import eu.timepit.refined.string.{MatchesRegex, Url}
import shapeless.Witness

package object refined {

  implicit class RichIntJson(json: Json[Int]) extends NumericExtensions[Int](Int.MinValue, Int.MaxValue) {
    protected def withRefined[P](bound: Range)(implicit V: Validate[Int, P], R: RefType[Refined]): Json[Int Refined P] =
      new Json[Refined[Int, P]] {
        override def apply[F[_]: JsonAlgebra]: F[Refined[Int, P]] =
          JsonAlgebra[F].pmap(JsonAlgebra[F].int(bound))(p => Attempt.fromEither(R.refine(p)))(R.unwrap)
      }
  }

  implicit class RichLongJson(json: Json[Long]) extends NumericExtensions[Long](Long.MinValue, Long.MaxValue) {
    protected def withRefined[P](bound: Range)(implicit V: Validate[Long, P], R: RefType[Refined]): Json[Long Refined P] =
      new Json[Refined[Long, P]] {
        override def apply[F[_]: JsonAlgebra]: F[Refined[Long, P]] =
          JsonAlgebra[F].pmap(JsonAlgebra[F].long(bound))(p => Attempt.fromEither(R.refine(p)))(R.unwrap)
      }
  }

  implicit class RichFloatJson(json: Json[Float]) extends NumericExtensions[Float](Float.MinValue.toDouble, Float.MaxValue.toDouble) {
    protected def withRefined[P](bound: Range)(implicit V: Validate[Float, P], R: RefType[Refined]): Json[Float Refined P] =
      new Json[Refined[Float, P]] {
        override def apply[F[_]: JsonAlgebra]: F[Refined[Float, P]] =
          JsonAlgebra[F].pmap(JsonAlgebra[F].float(bound))(p => Attempt.fromEither(R.refine(p)))(R.unwrap)
      }
  }

  implicit class RichDoubleJson(json: Json[Double]) extends NumericExtensions[Double](Double.MinValue, Double.MaxValue) {
    protected def withRefined[P](bound: Range)(implicit V: Validate[Double, P], R: RefType[Refined]): Json[Double Refined P] =
      new Json[Refined[Double, P]] {
        override def apply[F[_]: JsonAlgebra]: F[Refined[Double, P]] =
          JsonAlgebra[F].pmap(JsonAlgebra[F].double(bound))(p => Attempt.fromEither(R.refine(p)))(R.unwrap)
      }
  }

  implicit class RichStringJson(json: Json[String]) {
    private def withRefined[P](desc: StringDescriptor)(implicit V: Validate[String, P], R: RefType[Refined]): Json[String Refined P] =
      new Json[Refined[String, P]] {
        override def apply[F[_]: JsonAlgebra]: F[Refined[String, P]] =
          JsonAlgebra[F].pmap(JsonAlgebra[F].string(desc))(p => Attempt.fromEither(R.refine(p)))(R.unwrap)
      }

    def notEmpty(implicit V: Validate[String, Not[Empty]], R: RefType[Refined]): Json[String Refined Not[Empty]] =
      withRefined(StringDescriptor.Length(LengthBound.Atleast(1)))

    def matchesRegex[S <: String](implicit V: Validate.Plain[String, MatchesRegex[S]], R: RefType[Refined], WS: Witness.Aux[S]): Json[String Refined MatchesRegex[S]] =
      withRefined(StringDescriptor.Pattern(WS.value))

    def minLength[L <: Int](implicit VH: Validate[String, MinSize[L]], L: Witness.Aux[L]): Json[Refined[String, MinSize[L]]] =
      withRefined(StringDescriptor.Length(LengthBound.Atleast(L.value)))

    def maxLength[H <: Int](implicit VH: Validate[String, MaxSize[H]], H: Witness.Aux[H]): Json[Refined[String, MaxSize[H]]] =
      withRefined(StringDescriptor.Length(LengthBound.Atmost(H.value)))

    def sized[L <: Int, H <: Int](implicit VH: Validate[String, Size[Interval.Closed[L, H]]], L: Witness.Aux[L], H: Witness.Aux[H]): Json[Refined[String, Size[Interval.Closed[L, H]]]] =
      withRefined(StringDescriptor.Length(LengthBound.Interval(L.value, H.value)))

    def exactSize[L <: Int](implicit VH: Validate[String, Size[Equal[L]]], L: Witness.Aux[L]): Json[Refined[String, Size[Equal[L]]]] =
      withRefined(StringDescriptor.Length(LengthBound.Exact(L.value)))

    def url(implicit V: Validate.Plain[String, Url], R: RefType[Refined]): Json[String Refined Url] =
      withRefined(StringDescriptor.Type(StringType.Uri))
  }
}

abstract class NumericExtensions[T](min: BigDecimal, max: BigDecimal)(implicit T: Numeric[T]) {
  protected def withRefined[P](bound: Range)(implicit V: Validate[T, P], R: RefType[Refined]): Json[T Refined P]

  def negative[N <: T](implicit V: Validate.Plain[T, Negative]): Json[T Refined Negative] =
    withRefined(Range(Bound(min, true), Bound(0, false)))

  def positive[N <: T](implicit V: Validate.Plain[T, Positive]): Json[T Refined Positive] =
    withRefined(Range(Bound(1, true), Bound(max, true)))

  def intervalOpen[L <: T, H <: T](implicit VL: Validate[T, Interval.Open[L, H]], L: Witness.Aux[L], H: Witness.Aux[H]): Json[T Refined Interval.Open[L, H]] =
    withRefined(Range(Bound(T.toDouble(L.value), false), Bound(T.toDouble(H.value), false)))

  def intervalOpenClosed[L <: T, H <: T](implicit VL: Validate[T, OpenClosed[L, H]], L: Witness.Aux[L], H: Witness.Aux[H]): Json[T Refined Interval.OpenClosed[L, H]] =
    withRefined(Range(Bound(T.toDouble(L.value), false), Bound(T.toDouble(H.value), true)))

  def intervalClosedOpen[L <: T, H <: T](implicit VL: Validate[T, ClosedOpen[L, H]], L: Witness.Aux[L], H: Witness.Aux[H]): Json[T Refined Interval.ClosedOpen[L, H]] =
    withRefined(Range(Bound(T.toDouble(L.value), true), Bound(T.toDouble(H.value), false)))

  def intervalClosed[L <: T, H <: T](implicit VL: Validate[T, Interval.Closed[L, H]], L: Witness.Aux[L], H: Witness.Aux[H]): Json[T Refined Interval.Closed[L, H]] =
    withRefined(Range(Bound(T.toDouble(L.value), true), Bound(T.toDouble(H.value), true)))
}

trait RefinedPrimitives { self: HttpEndpointAlgebra =>
  implicit class RefinedSyntax[F[_], A](fa: F[A])(implicit P: Partial[F]) {
    def refined[P](implicit V: Validate[A, P], R: RefType[Refined]): F[Refined[A, P]] =
      P.pmap(fa)(p => Attempt.fromThrowable(R.refine(p).left.map(err => new Throwable(err)), "Refinement decode error"))(R.unwrap)
  }
}

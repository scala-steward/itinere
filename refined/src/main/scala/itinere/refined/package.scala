package itinere

import eu.timepit.refined.api.{RefType, Refined, Validate}
import eu.timepit.refined.numeric.{Greater, Interval, Less, Positive}
import eu.timepit.refined.string.{MatchesRegex, Url}
import shapeless.Witness

package object refined {

  implicit class RichStringJson(val Json: Json[String]) {

    private def withRefined[P](description: StringDescription)(implicit V: Validate[String, P], R: RefType[Refined]): Json[String Refined P] =
      new Json[Refined[String, P]] {
        override def apply[F[_] : JsonAlgebra]: F[Refined[String, P]] =
          JsonAlgebra[F].pmap(JsonAlgebra[F].string(description))(p => Attempt.fromEither(R.refine(p)))(R.unwrap)
      }

    def matchesRegex[S <: String](implicit V: Validate.Plain[String, MatchesRegex[S]], R: RefType[Refined], WS: Witness.Aux[S]): Json[String Refined MatchesRegex[S]] =
      withRefined(StringDescription.Pattern(WS.value))

    def url[S <: String](implicit V: Validate.Plain[String, Url], R: RefType[Refined]): Json[String Refined Url] =
      withRefined(StringDescription.Type(StringType.Uri))
  }

  implicit class RichIntJson(val Json: Json[Int]) {
    private def withRefined[P](bound: Range)(implicit V: Validate[Int, P], R: RefType[Refined]): Json[Int Refined P] =
      new Json[Refined[Int, P]] {
        override def apply[F[_] : JsonAlgebra]: F[Refined[Int, P]] =
          JsonAlgebra[F].pmap(JsonAlgebra[F].int(bound))(p => Attempt.fromEither(R.refine(p)))(R.unwrap)
      }

    def positive[N <: Int](implicit V: Validate.Plain[Int, Positive]): Json[Int Refined Positive] =
      withRefined(Range(Bound(1, true), Bound(Int.MaxValue, true)))

    def intervalOpen[L <: Int, H <: Int](implicit VL: Validate.Plain[Int, Greater[L]], VH: Validate.Plain[Int, Less[H]], L: Witness.Aux[L], H: Witness.Aux[H]): Json[Int Refined Interval.Open[L, H]] =
      withRefined(Range(Bound(L.value, true), Bound(H.value, true)))
  }

  implicit class RichReads[A](val reads: Read[A]) {
    def refined[P](implicit V: Validate[A, P], R: RefType[Refined]): Read[A Refined P] =
      reads.pmap(p => Attempt.fromThrowable(R.refine(p).left.map(err => new Throwable(err))))(R.unwrap)
  }

}

import cats.Invariant
package object itinere {

  implicit class PartialSyntax[F[_], A](fa: F[A])(implicit P: Partial[F]) {
    def pmap[B](f: A => Attempt[B])(g: B => A): F[B] = P.pmap(fa)(f)(g)
  }

  implicit class InvariantFunctorOps[F[_], A](fa: F[A])(implicit I: Invariant[F]) {
    def as[B](implicit T: Transformer[F, A, B]): F[B] = T(fa)
  }

  def member[A, B](json: Json[A], getter: B => A, documentation: Option[String] = None): Member[Json, A, B] =
    Member[Json, A, B](json, getter, documentation)
}

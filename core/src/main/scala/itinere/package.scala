package object itinere extends JsonDsl {

  implicit class PartialSyntax[F[_], A](fa: F[A])(implicit P: Partial[F]) {
    def pmap[B](f: A => Attempt[B])(g: B => A): F[B] = P.pmap(fa)(f)(g)
  }

  def member[A, B](avro: Json[A], getter: B => A, documentation: Option[String] = None): Member[Json, A, B] =
    Member[Json, A, B](avro, getter, documentation)
}

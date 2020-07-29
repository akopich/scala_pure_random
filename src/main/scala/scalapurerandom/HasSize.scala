package scalapurerandom

import cats.{Foldable, NonEmptyReducible}
import cats.implicits._

trait HasSize[F[_], S] {
  def size(f: F[_]): S
}

trait HasSizeHelper { self =>
  implicit def foldableHasSize[F[_]](implicit f: Foldable[F]): HasSize[F, Nat] = fa => Nat(f.size(fa).toInt)

  implicit lazy val nelIsReducible: NonEmptyReducible[NEL, List] = new NonEmptyReducible[NEL, List] {
    override def split[A](fa: NEL[A]): (A, List[A]) = (fa.head, fa.tail)
  }

  implicit lazy val nevIsReducible: NonEmptyReducible[NEV, Vector] = new NonEmptyReducible[NEV, Vector] {
    override def split[A](fa: NEV[A]): (A, Vector[A]) = (fa.head, fa.tail)
  }

  implicit def nonEmptyReducibleSize[F[_], G[_]](implicit r: NonEmptyReducible[F, G],
                                                 s: HasSize[G, Nat]): HasSize[F, PosInt] =
    fa => inc(s.size(r.split(fa)._2))

  def size[F[_], S](f: F[_])(implicit s: HasSize[F, S]): S = s.size(f)
}

package scalapurerandom

import cats.data._

trait HasSize[F[_], S] {
  def size(f: F[_]): S
}

trait HasSizeHelper { self =>
  implicit val listHasSize: HasSize[List, Nat] = f => Nat(f.size)

  implicit val vecHasSize: HasSize[Vector, Nat] = f => Nat(f.size)

  implicit val nelHasSize: HasSize[NonEmptyList, PosInt] = f => inc(self.size(f.tail))

  implicit val nevHasSize: HasSize[NEV, PosInt] = f => inc(self.size(f.tail))

  def size[F[_], S](f: F[_])(implicit s: HasSize[F, S]): S = s.size(f)
}

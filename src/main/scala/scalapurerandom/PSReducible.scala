package scalapurerandom

import cats.Reducible
import cats.kernel.Semigroup
import cats.implicits._
import simulacrum.{op, typeclass}

import scala.collection.parallel.CollectionConverters._

@typeclass trait PSReducible[R[_]] {
  def preduce[A](as: R[A])(implicit semi: Semigroup[A]): A
}

object ParReducibleInstance {
  implicit def parReducibleNEL: PSReducible[NEL] = new PSReducible[NEL] {
    override def preduce[A](as: NEL[A])(implicit semi: Semigroup[A]): A = as.toList.par.reduce(semi.combine)
  }

  implicit def parReducibleNEV: PSReducible[NEV] = new PSReducible[NEV] {
    override def preduce[A](as: NEV[A])(implicit semi: Semigroup[A]): A = as.toVector.par.reduce(semi.combine)
  }
}

object SeqReducibleInstance {
  implicit def seqReducible[R[_]: Reducible]: PSReducible[R] = new PSReducible[R] {
    override def preduce[A](as: R[A])(implicit semi: Semigroup[A]): A = as.reduce
  }
}


package scalapurerandom

import simulacrum.{op, typeclass}

import scala.collection.parallel.CollectionConverters._

@typeclass trait PSFunctor[F[_]] {
  @op("pmap") def pmap[A, B](fa: F[A])(f: A => B): F[B]
}

object ParFunctorInstances {
  implicit val vectorFunctorPar: PSFunctor[Vector] = new PSFunctor[Vector] {
    override def pmap[A, B](fa: Vector[A])(f: A => B): Vector[B] =
      fa.par.map(f).seq
  }

  implicit val nevFunctorPar: PSFunctor[NEV] = new PSFunctor[NEV] {
    override def pmap[A, B](fa: NEV[A])(f: A => B): NEV[B] = toNEV(vectorFunctorPar.pmap(fa.toVector)(f))
  }
}

object SeqFunctorInstances {
  implicit val vectorFunctorSeq: PSFunctor[Vector] = new PSFunctor[Vector] {
    override def pmap[A, B](fa: Vector[A])(f: A => B): Vector[B] =
      fa.map(f)
  }

  implicit val nevFunctorSeq: PSFunctor[NEV] = new PSFunctor[NEV] {
    override def pmap[A, B](fa: NEV[A])(f: A => B): NEV[B] = toNEV(vectorFunctorSeq.pmap(fa.toVector)(f))
  }
}
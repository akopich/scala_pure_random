package scalapurerandom

import cats.Functor
import cats.implicits._
import simulacrum.{op, typeclass}

import scala.collection.parallel.CollectionConverters._

@typeclass trait PSFunctor[F[_]] {
  def pmap[A, B](fa: F[A])(f: A => B): F[B]
}

object ParFunctorInstances {
  implicit val vectorFunctorPar: PSFunctor[Vector] = new PSFunctor[Vector] {
    override def pmap[A, B](fa: Vector[A])(f: A => B): Vector[B] =
      fa.par.map(f).seq
  }

  implicit val listFunctorPar: PSFunctor[List] = new PSFunctor[List] {
    override def pmap[A, B](fa: List[A])(f: A => B): List[B] =
      fa.par.map(f).seq.toList
  }

  implicit val nevFunctorPar: PSFunctor[NEV] = new PSFunctor[NEV] {
    override def pmap[A, B](fa: NEV[A])(f: A => B): NEV[B] = toNEV(vectorFunctorPar.pmap(fa.toVector)(f))
  }


  implicit val nelFunctorPar: PSFunctor[NEL] = new PSFunctor[NEL] {
    override def pmap[A, B](fa: NEL[A])(f: A => B): NEL[B] = toNEL(listFunctorPar.pmap(fa.toList)(f))
  }
}

object SeqFunctorInstances {
  implicit def functorSeq[F[_]: Functor]: PSFunctor[F] = new PSFunctor[F] {
    override def pmap[A, B](fa: F[A])(f: A => B): F[B] = fa.map(f)
  }
}
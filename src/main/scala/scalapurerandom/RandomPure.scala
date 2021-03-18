package scalapurerandom

import breeze.numerics.log
import cats.data.{IndexedStateT, NonEmptyList, NonEmptyVector, State, StateT}
import spire.random.rng.MersenneTwister64
import breeze.linalg.{DenseVector, cholesky}
import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import cats.effect.IO._
import cats.effect.IO


class MersenneTwisterImmutable(private val gen: MersenneTwister64) {
  def apply[T](action : MersenneTwister64 => T): (MersenneTwisterImmutable, T) = {
    val newgen = gen.copyInit
    (new MersenneTwisterImmutable(newgen), action(newgen))
  }
}


trait RandomPure {
  implicit class RichRandom[G, T](r: State[G, T]) {
    def sample(gen: G): T = r.runA(gen).value
  }

  implicit class RichRandomT[G, M[_]: Monad, T](r: StateT[M, G, T]) {
    def sample(gen: G): M[T] = r.runA(gen)
  }

  def lift[G, M[_]: Monad, T](r: State[G, T]): StateT[M, G, T] = r.transformF(eval => implicitly[Monad[M]].pure(eval.value))

  def sampleMeanPar[G: Splittable, T: Averageble](random: State[G, T], n: PosInt)
                                  (implicit cs: ContextShift[IO]): StateT[IO, G, T] = {
    val chunkSizes = getChunkSizes(n)
    val semi = implicitly[Averageble[T]].semi.additive

    fromState(implicitly[Splittable[G]].randomSplit(size(chunkSizes)).map { gens =>
      gens.zipWith(chunkSizes) { (a, b) => (a,b) }.parTraverse{ case(gen, size) =>
        IO { replicateA(size, random).sample(gen).reduce(semi) }
      }.map(_.reduce(semi) |/| n)
    })
  }

  def samplePar[G:Splittable, T](random: State[G, T], n: PosInt)
                  (implicit cs: ContextShift[IO]): StateT[IO, G, NEL[T]] = {
    val chunkSizes = getChunkSizes(n)

    fromState(implicitly[Splittable[G]].randomSplit(size(chunkSizes)).map { gens =>
      (gens, chunkSizes).parTupled.parTraverse { case(gen, size) =>
        IO { NonEmptyChain.fromNonEmptyList(replicateA(size, random).sample(gen)) }
      }.map(_.reduce.toNonEmptyList)
    })
  }

  private def getChunkSizes(n: PosInt) =  toNEL((0 until n.toInt)
      .groupBy(_ % Runtime.getRuntime.availableProcessors).toSeq
      .map(_._2.length)
      .filter(_ > 0)
      .map(PosInt.apply))

  def sampleMean[G, M[_]: Monad, T: Averageble](random: StateT[M, G, T], n : PosInt)
                                            (implicit nelReducer: PSReducible[NEL]): StateT[M, G, T] =
    replicateA(n, random).map(x => average(x))

  def sampleMeanVarGeneralized[G, M[_]: Monad, T: Averageble, Outer: Averageble]
                                                              (random: StateT[M, G, T], n: PosInt)
                                                              (minus: (T, T) => T)
                                                              (outer: T => Outer)
                                                              (implicit nelReducer: PSReducible[NEL],
                                                               nelFunctor: PSFunctor[NEL]): StateT[M, G, (T, Outer)] = {
    replicateA(n, random).map { s =>
      val m = average(s)
      (m, average(s.pmap { v =>
        val centered = minus(v, m)
        outer(centered)
      }))
    }
  }

  def sampleMeanVar[G, M[_]: Monad](random: StateT[M, G, Double], n: PosInt)
                                (implicit nelReducer: PSReducible[NEL],
                                 nelFunctor: PSFunctor[NEL]): StateT[M, G, (Double, Double)] = {
    sampleMeanVarGeneralized(random, n) { (v, m) => v - m} { centered => centered * centered }
  }

  def sampleMeanAndCov[G, M[_]: Monad](random: StateT[M, G, DV], n : PosInt)
                                   (implicit nelReducer: PSReducible[NEL],
                                    nelFunctor: PSFunctor[NEL]): StateT[M, G, (DV, DM)] = {
    sampleMeanVarGeneralized(random, n) { (v, m) => v - m} { centered => centered * centered.t }
  }
}


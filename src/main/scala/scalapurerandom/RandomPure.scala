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
  type Gen = MersenneTwisterImmutable
  type Random[T] = State[Gen, T]
  def  Random[T]  = State[Gen, T] _

  type RandomT[F[_], T] = StateT[F, Gen, T]
  def  RandomT[F[_]: Monad, T] = StateT[F, Gen, T] _

  def const[T](t: T): Random[T] = Random{ rng => (rng, t) }

  implicit class MultRandom[T: TimesScalar](r: Random[T]) {
    def *(sr: Random[Double]): Random[T] = times(r, sr)
  }

  implicit class RichRandom[T](r: Random[T]) {
    def sample(gen: Gen): T = r.runA(gen).value
  }

  implicit class RichRandomT[M[_]: Monad, T](r: RandomT[M, T]) {
    def sample(gen: Gen): M[T] = r.runA(gen)
  }

  def times[T: TimesScalar](xr: Random[T], yr: Random[Double]): Random[T] = for {
    x <- xr
    y <- yr
  } yield x * y

  def mixture[T](ra: Random[T], rb: Random[T]): Random[T] = for {
    a <- ra
    b <- rb
    p <- uniform01
  } yield if (p > 0.5d) a else b

  def lift[M[_]: Monad, T](r: Random[T]): RandomT[M, T] = r.transformF(eval => implicitly[Monad[M]].pure(eval.value))

  def randomSplit(n: Int): Random[List[Gen]] = (0 until n).view.map(_ =>
    for {
      seed <- long
    } yield getGen(seed)).toList.sequence

  def randomSplit(n: PosInt): Random[NEL[Gen]] = for {
    tail <- randomSplit(n.dec.toInt)
    seed <- long
  } yield NonEmptyList(getGen(seed), tail)

  def next(bits: Int): Random[Int] = Random { gen =>
    gen(_.nextBits(bits))
  }

  def int: Random[Int] = Random { gen =>
    gen(_.nextInt())
  }

  def int(n:Int): Random[Int] = Random { gen =>
    gen(_.nextInt(n))
  }

  def long: Random[Long] = Random { gen =>
    gen(_.nextLong())
  }

  def uniform01: Random[Double] = Random { gen =>
    gen(_.nextDouble())
  }

  def gaussian(mu: Double, sigma: Double): Random[Double] = Random { gen =>
    gen(_.nextGaussian(mu, sigma))
  }

  def laplace(location: Double, scale: Double): Random[Double] = uniform01.map { u =>
    // from numpy
    if (u < 0.5) location + scale * log(2 * u)
    else location - scale * log(2 * (1 - u))
  }

  def getGen(seed: Long): Gen = new MersenneTwisterImmutable(MersenneTwister64.fromTime(time =  seed))

  def standardGaussian(dim: Int): Random[DV] = Random { gen =>
    gen(mt => DenseVector.fill(dim)(mt.nextGaussian()))
  }

  def centeredGaussian(cov: DM): Random[DV] = {
    val L = cholesky(cov)
    standardGaussian(cov.rows).map(L * _)
  }

  def sampleMeanPar[T: Averageble](random: Random[T], n: PosInt)
                                  (implicit cs: ContextShift[IO]): RandomT[IO, T] = {
    val chunkSizes = getChunkSizes(n)
    val semi = implicitly[Averageble[T]].semi.additive

    fromState(randomSplit(size(chunkSizes)).map { gens =>
      gens.zipWith(chunkSizes) { (a, b) => (a,b) }.parTraverse{ case(gen, size) =>
        IO { replicateA(size, random).sample(gen).reduce(semi) }
      }.map(_.reduce(semi) |/| n)
    })
  }

  def samplePar[T](random: Random[T], n: PosInt)
                  (implicit cs: ContextShift[IO]): RandomT[IO, NEL[T]] = {
    val chunkSizes = getChunkSizes(n)

    fromState(randomSplit(size(chunkSizes)).map { gens =>
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

  def sampleMean[M[_]: Monad, T: Averageble](random: RandomT[M, T], n : PosInt): RandomT[M, T] =
    replicateA(n, random).map(x => average(x))

  def sampleMeanVarGeneralized[M[_]: Monad, T: Averageble, Outer: Averageble](random: RandomT[M, T], n: PosInt)
                                                                             (minus: (T, T) => T)
                                                                             (outer: T => Outer): RandomT[M, (T, Outer)] = {
    replicateA(n, random).map { s =>
      val m = average(s)
      (m, average(s.map { v =>
        val centered = minus(v, m)
        outer(centered)
      }))
    }
  }

  def sampleMeanVar[M[_]: Monad](random: RandomT[M, Double], n: PosInt): RandomT[M, (Double, Double)] = {
    sampleMeanVarGeneralized(random, n) { (v, m) => v - m} { centered => centered * centered }
  }

  def sampleMeanAndCov[M[_]: Monad](random: RandomT[M, DV], n : PosInt): RandomT[M, (DV, DM)] = {
    sampleMeanVarGeneralized(random, n) { (v, m) => v - m} { centered => centered * centered.t }
  }
}


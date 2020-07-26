package scalapurerandom

import breeze.numerics.log
import Averageble._
import cats.data.{IndexedStateT, State, StateT}
import spire.random.rng.MersenneTwister64
import scalapurerandom.TimesScalar.ops._
import breeze.linalg.{DenseVector, cholesky}
import cats._
import cats.implicits._
import cats.effect._
import cats.effect.IO._
import cats.effect.IO
import cats.data.IndexedStateT._
import cats.arrow.FunctionK.lift


class MersenneTwisterImmutable(private val gen: MersenneTwister64) {
  def apply[T](action : MersenneTwister64 => T): (MersenneTwisterImmutable, T) = {
    val newgen = gen.copyInit
    (new MersenneTwisterImmutable(newgen), action(newgen))
  }
}


trait RandomPure {
  type Gen = MersenneTwisterImmutable
  type Random[T] = State[Gen, T]
  def Random[T]  = State[Gen, T] _

  type RandomT[F[_], T] = StateT[F, Gen, T]

  def const[T](t: T): Random[T] = Random{ rng => (rng, t) }

  implicit class MultRandom[T: TimesScalar](r: Random[T]) {
    def *(sr: Random[Double]): Random[T] = times(r, sr)
  }

  implicit class RichRandom[T](r: Random[T]) {
    def sample(gen: Gen): T = r.runA(gen).value
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

  def randomSplit(n: Int): Random[List[Gen]] = (0 until n).view.map(_ =>
    for {
      seed <- long
    } yield getGen(seed)).toList.sequence

  def randomSplit(n: PosInt): Random[NEV[Gen]] = (n times None).map { _ =>
    long.map(getGen)
  } sequence

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

  def getGen(seed: Long) = new MersenneTwisterImmutable(MersenneTwister64.fromTime(time =  seed))

  def standardGaussian(dim: Int): Random[DV] = Random { gen =>
    gen(mt => DenseVector.fill(dim)(mt.nextGaussian()))
  }

  def centeredGaussian(cov: DM): Random[DV] = {
    val L = cholesky(cov)
    standardGaussian(cov.rows).map(L * _)
  }

  def sampleMeanPar[T: Averageble](random: Random[T], n: PosInt)(implicit cs: ContextShift[IO]): Random[IO[T]] = {
    randomSplit(n).map { gens =>
      gens.parTraverse(gen => IO { random.sample(gen) } ).map(samples => average(samples))
    }
  }

  def sampleMean[T: Averageble](random: Random[T], n : PosInt): Random[T] =
    replicateA(n, random).map(x => average(x))

  def sampleMeanVarGeneralized[T: Averageble, Outer: Averageble](random: Random[T], n: PosInt)
                                                                (minus: (T, T) => T)
                                                                (outer: T => Outer): Random[(T, Outer)] = {
    replicateA(n, random).map { s =>
      val m = average(s)
      (m, average(s.map { v =>
        val centered = minus(v, m)
        outer(centered)
      }))
    }
  }

  def sampleMeanVar(random: Random[Double], n: PosInt): Random[(Double, Double)] = {
    sampleMeanVarGeneralized(random, n) { (v, m) => v - m} { centered => centered * centered }
  }

  def sampleMeanAndCov(random: Random[DV], n : PosInt): Random[(DV, DM)] = {
    sampleMeanVarGeneralized(random, n) { (v, m) => v - m} { centered => centered * centered.t }
  }
}


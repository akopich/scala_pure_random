package scalapurerandom

import breeze.linalg._
import breeze.numerics.log
import cats._
import cats.data._
import cats.implicits._
import Averageble._
import spire.random.rng.MersenneTwister64
import TimesScalar.ops._
import breeze.math.Field

class MersenneTwisterImmutable(private val gen: MersenneTwister64) {
  def apply[T](action : MersenneTwister64 => T): (MersenneTwisterImmutable, T) = {
    val newgen = gen.copyInit
    (new MersenneTwisterImmutable(newgen), action(newgen))
  }
}


object RandomPure {
  type Gen = MersenneTwisterImmutable
  type Random[T] = State[Gen, T]
  def Random[T]  = State[Gen, T] _

  def const[T](t: T): Random[T] = Random{ rng => (rng, t) }

  implicit class MultRandom[T: TimesScalar](r: Random[T]) {
    def |*|(sr: Random[Double]): Random[T] = times(r, sr)
  }

  implicit class RichRandom[T](r: Random[T]) {
    def sample(gen: Gen): T = r.runA(gen).value
  }

  def times[T: TimesScalar](xr: Random[T], yr: Random[Double]): Random[T] = for {
    x <- xr
    y <- yr
  } yield x |*| y

  def mixture[T](ra: Random[T], rb: Random[T]): Random[T] = for {
    a <- ra
    b <- rb
    p <- uniform01
  } yield if (p > 0.5d) a else b

  def randomSplit(n: Int): Random[List[Gen]] = (0 until n).view.map(_ =>
    for {
      seed <- long
    } yield getGen(seed)).toList.sequence

  def randomSplit(n: Pos): Random[NonEmptyVector[Gen]] = (n times None).map {_ =>
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

  def sampleMean[T: Averageble](random: Random[T], n : Pos): Random[T] = (n times None).traverse(_ => random).map(x => average(x))

  def sampleMeanVar(random: Random[Double], n: Pos): Random[(Double, Double)] = {
    val samplesR = (n times None).traverse(_ => random)
    val meanR = samplesR.map(x => average(x))

    for {
      s <- samplesR
      m <- meanR
    } yield (m, average(s.map { v =>
      val centered = v - m
      centered * centered
    }))
  }

  def sampleMeanAndCov(random: Random[DV], n : Pos): Random[(DV, DM)] = {
    val samplesR = (n times None).traverse(_ => random)
    val meanR = samplesR.map(x => average(x))
    for {
      s <- samplesR
      m <- meanR
    } yield (m, average(s.map { v =>
      val centered = v - m
      centered * centered.t
    }))
  }
}


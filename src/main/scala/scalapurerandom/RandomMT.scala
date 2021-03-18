package scalapurerandom

import breeze.linalg.{DenseVector, cholesky}
import breeze.numerics.log
import cats._
import cats.data._
import cats.implicits._
import spire.random.rng.MersenneTwister64

object RandomMT {
  type Gen = MersenneTwisterImmutable
  type Random[T] = State[Gen, T]
  def  Random[T]  = State[Gen, T] _

  type RandomT[F[_], T] = StateT[F, Gen, T]
  def  RandomT[F[_]: Monad, T] = StateT[F, Gen, T] _

  def const[T](t: T): Random[T] = Random{ rng => (rng, t) }

  implicit lazy val genIsSplittable: Splittable[Gen] = new Splittable[Gen] {
    def randomSplit(n: Int): Random[List[Gen]] = (0 until n).view.map(_ =>
      long.map(getGen)
    ).toList.sequence

    def randomSplit(n: PosInt): Random[NEL[Gen]] = for {
      tail <- randomSplit(n.dec.toInt)
      seed <- long
    } yield NonEmptyList(getGen(seed), tail)
  }

  implicit class MultRandom[T: TimesScalar](r: Random[T]) {
    def *(sr: Random[Double]): Random[T] = times(r, sr)
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

}

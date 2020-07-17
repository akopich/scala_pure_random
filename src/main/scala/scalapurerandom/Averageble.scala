package scalapurerandom


import HeadTailDecomposable._
import algebra.instances.IntAlgebra
import algebra.instances.DoubleAlgebra
import algebra.ring.AdditiveSemigroup
import cats.data.Reader
import spire.implicits._

trait Averageble[T] extends AdditiveSemigroup[T] { self =>
  def |/|(x:T, cnt: Pos): T

  def compose[U](other: Averageble[U]): Averageble[(T, U)] = new Averageble[(T, U)] {
    override def |/|(x: (T, U), cnt: Pos): (T, U) = (self.|/|(x._1, cnt), other.|/|(x._2, cnt))

    override def plus(x: (T, U), y: (T, U)): (T, U) = (self.plus(x._1, y._1), other.plus(x._2, y._2))
  }
}

object Averageble {
  implicit class AvgWrapper[T:Averageble](value: T) {
    def |/|(cnt: Pos): T = implicitly[Averageble[T]].|/|(value, cnt)
  }

  def average[T: Averageble](a: NEV[T]): T = a.reduce(implicitly[Averageble[T]].additive) |/| size(a)

  implicit def intAverageble: IntAlgebra with Averageble[Int] = new IntAlgebra with Averageble[Int] {
    override def |/|(x: Int, cnt: Pos): Int = x / cnt
  }

  implicit def doubleAverageble: Averageble[Double] = new DoubleAlgebra with Averageble[Double]  {
    override def |/|(x: Double, cnt: Pos): Double = x / cnt
  }

  implicit def DVAverageble: Averageble[DV] = new DVSemigroup with Averageble[DV] {
    override def |/|(x: DV, cnt: Pos): DV = x / cnt.toDouble
  }

  implicit def DMAverageble: Averageble[DM] = new DMSemigroup with Averageble[DM] {
    override def |/|(x: DM, cnt: Pos): DM = x / cnt.toDouble
  }

  implicit def functionAverageble[A, B: Averageble]: Averageble[A => B] = new Averageble[A => B] {
    override def |/|(x: A => B, cnt: Pos): A => B = (a: A) => x(a) |/| cnt

    override def plus(x: A => B, y: A => B): A => B = (a: A) => x(a) + y(a)
  }

  implicit def readerAverageble[A: Averageble, E]: Averageble[Reader[E, A]] = new Averageble[Reader[E, A]] {
    type C[B] = Reader[E, B]
    override def |/|(fa: C[A], cnt: Pos): C[A] = fa.map(_ |/| cnt)

    override def plus(fx: C[A], fy: C[A]): C[A] = for {
      x <- fx
      y <- fy
    } yield x + y
  }

  implicit def randomAverageble[A: Averageble] : Averageble[Random[A]] = new RandomSemi[A] with Averageble[Random[A]] {
    override def |/|(x: Random[A], cnt: Pos): Random[A] = x.map(_ |/| cnt)
  }
}

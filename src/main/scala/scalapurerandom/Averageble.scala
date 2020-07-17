package scalapurerandom

import cats._
import cats.data._
import cats.implicits._
import cats.kernel.instances.{DoubleGroup, IntGroup}
import HeadTailDecomposable._
import RandomPure._


trait Averageble[T] extends Semigroup[T] { self =>
  def |/|(x:T, cnt: Pos): T

  def compose[U](other: Averageble[U]): Averageble[(T, U)] = new Averageble[(T, U)] {
    override def |/|(x: (T, U), cnt: Pos): (T, U) = (self.|/|(x._1, cnt), other.|/|(x._2, cnt))

    override def combine(x: (T, U), y: (T, U)): (T, U) = (self.combine(x._1, y._1), other.combine(x._2, y._2))
  }
}

object Averageble {
  implicit class AvgWrapper[T:Averageble](value: T) {
    def |/|(cnt: Pos): T = implicitly[Averageble[T]].|/|(value, cnt)
  }

  def average[T: Averageble](a: NEV[T]): T = a.reduce |/| size(a)

  implicit def intAverageble: IntGroup with Averageble[Int] = new IntGroup with Averageble[Int]  {
    override def |/|(x: Int, cnt: Pos): Int = x / cnt
  }

  implicit def doubleAverageble: Averageble[Double] = new DoubleGroup with Averageble[Double]  {
    override def |/|(x: Double, cnt: Pos): Double = x / cnt
  }

  implicit def DVAverageble: Averageble[DV] = new Averageble[DV] {
    override def |/|(x: DV, cnt: Pos): DV = x / cnt.toDouble

    override def combine(x: DV, y: DV): DV = x + y
  }

  implicit def DMAverageble: Averageble[DM] = new Averageble[DM] {
    override def |/|(x: DM, cnt: Pos): DM = x / cnt.toDouble

    override def combine(x: DM, y: DM): DM = x + y
  }

  implicit def functionAverageble[A, B: Averageble]: Averageble[A => B] = new Averageble[A => B] {
    override def |/|(x: A => B, cnt: Pos): A => B = (a: A) => x(a) |/| cnt

    override def combine(x: A => B, y: A => B): A => B = (a: A) => x(a) |+| y(a)
  }

  implicit def readerAverageble[A: Averageble, E]: Averageble[Reader[E, A]] = new Averageble[Reader[E, A]] {
    type C[B] = Reader[E, B]
    override def |/|(fa: C[A], cnt: Pos): C[A] = fa.map(_ |/| cnt)

    override def combine(fx: C[A], fy: C[A]): C[A] = for {
      x <- fx
      y <- fy
    } yield x |+| y
  }

  implicit def randomAverageble[A: Averageble] : Averageble[Random[A]] = new Averageble[Random[A]] {
    override def |/|(x: Random[A], cnt: Pos): Random[A] = x.map(_ |/| cnt)

    override def combine(rx: Random[A], ry: Random[A]): Random[A] = for {
      x <- rx
      y <- ry
    } yield x |+| y
  }
}

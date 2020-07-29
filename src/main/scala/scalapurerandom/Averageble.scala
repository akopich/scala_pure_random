package scalapurerandom


import algebra.instances.IntAlgebra
import algebra.instances.DoubleAlgebra
import algebra.ring.AdditiveSemigroup
import cats.data.Reader
import spire.implicits._

trait Averageble[T] { self =>
  val semi: AdditiveSemigroup[T]

  def |/|(x:T, cnt: PosInt): T

  def compose[U](other: Averageble[U]): Averageble[(T, U)] = new Averageble[(T, U)] {
    override def |/|(x: (T, U), cnt: PosInt): (T, U) = (self.|/|(x._1, cnt), other.|/|(x._2, cnt))

    override val semi: AdditiveSemigroup[(T, U)] = (x: (T, U), y: (T, U)) =>
      (self.semi.plus(x._1, y._1), other.semi.plus(x._2, y._2))
  }
}

trait AveragebleHelper {
  implicit class AvgWrapper[T:Averageble](value: T) {
    def |/|(cnt: PosInt): T = implicitly[Averageble[T]].|/|(value, cnt)
  }

  def average[T: Averageble](a: NEL[T]): T = a.reduce(implicitly[Averageble[T]].semi.additive) |/| size(a)

  implicit def intAverageble = new Averageble[Int] {
    override val semi: AdditiveSemigroup[Int] = new IntAlgebra

    override def |/|(x: Int, cnt: PosInt): Int = x / cnt.toInt
  }

  implicit def doubleAverageble: Averageble[Double] = new Averageble[Double]  {
    override val semi: AdditiveSemigroup[Double] = new DoubleAlgebra

    override def |/|(x: Double, cnt: PosInt): Double = x / cnt.toInt
  }

  implicit def DVAverageble: Averageble[DV] = new Averageble[DV] {
    override val semi: AdditiveSemigroup[DV] = new DVSemigroup

    override def |/|(x: DV, cnt: PosInt): DV = x / cnt.toInt.toDouble
  }

  implicit def DMAverageble: Averageble[DM] = new Averageble[DM] {
    override val semi: AdditiveSemigroup[DM] = new DMSemigroup

    override def |/|(x: DM, cnt: PosInt): DM = x / cnt.toInt.toDouble
  }

  implicit def functionAverageble[A, B: Averageble]: Averageble[A => B] = new Averageble[A => B] {
    override def |/|(x: A => B, cnt: PosInt): A => B = (a: A) => x(a) |/| cnt

    override val semi: AdditiveSemigroup[A => B] = (x: A => B, y: A => B) =>
      (a: A) => implicitly[Averageble[B]].semi.plus(x(a), y(a))
  }

  implicit def readerAverageble[A: Averageble, E]: Averageble[Reader[E, A]] = new Averageble[Reader[E, A]] {
    type C[B] = Reader[E, B]
    override def |/|(fa: C[A], cnt: PosInt): C[A] = fa.map(_ |/| cnt)

    override val semi: AdditiveSemigroup[C[A]] = (fx: C[A], fy: C[A]) => for {
      x <- fx
      y <- fy
    } yield implicitly[Averageble[A]].semi.plus(x, y)
  }

  implicit def randomAverageble[A: Averageble] : Averageble[Random[A]] = new Averageble[Random[A]] {
    implicit private val semiA = implicitly[Averageble[A]].semi
    override val semi: AdditiveSemigroup[Random[A]] = new RandomSemi[A]

    override def |/|(x: Random[A], cnt: PosInt): Random[A] = x.map(_ |/| cnt)
  }
}

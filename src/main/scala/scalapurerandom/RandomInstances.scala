package scalapurerandom

import algebra.ring.AdditiveSemigroup
import algebra.ring.Field
import spire.implicits._

class RandomSemi[T: AdditiveSemigroup] extends AdditiveSemigroup[Random[T]] {
  override def plus(xr: Random[T], yr: Random[T]): Random[T] =  for {
    x <- xr
    y <- yr
  } yield x + y
}

class RandomField[F : Field] extends RandomSemi[F] with Field[Random[F]] {
  override def div(xr: Random[F], yr: Random[F]): Random[F] = for {
    x <- xr
    y <- yr
  } yield x / y

  override def one: Random[F] = const(implicitly[Field[F]].one)

  override def negate(xr: Random[F]): Random[F] = xr.map(x => -x)

  override def zero: Random[F] = const(implicitly[Field[F]].zero)

  override def times(xr: Random[F], yr: Random[F]): Random[F] = for {
    x <- xr
    y <- yr
  } yield x * y
}

trait RandomInstances {
  implicit val randomDVSemi: AdditiveSemigroup[Random[DV]] = new RandomSemi[DV]

  implicit val randomDoubleField: Field[Random[Double]] = new RandomField[Double]
}

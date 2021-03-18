package scalapurerandom

import algebra.ring.AdditiveSemigroup
import algebra.ring.Field
import cats.data.State
import spire.implicits._

class RandomSemi[G, T: AdditiveSemigroup] extends AdditiveSemigroup[State[G, T]] {
  override def plus(xr: State[G, T], yr: State[G, T]): State[G, T] =  for {
    x <- xr
    y <- yr
  } yield x + y
}

class RandomField[G, F : Field] extends RandomSemi[G, F] with Field[State[G, F]] {
  override def div(xr: State[G, F], yr: State[G, F]): State[G, F] = for {
    x <- xr
    y <- yr
  } yield x / y

  private def const(f: F) = State { (rng: G) => (rng, f) }

  override def one: State[G, F] = const(implicitly[Field[F]].one)

  override def negate(xr: State[G, F]): State[G, F] = xr.map(x => -x)

  override def zero: State[G, F] = const(implicitly[Field[F]].zero)

  override def times(xr: State[G, F], yr: State[G, F]): State[G, F] = for {
    x <- xr
    y <- yr
  } yield x * y
}

trait RandomInstances {
  implicit def randomDVSemi[G]: AdditiveSemigroup[State[G, DV]] = new RandomSemi[G, DV]

  implicit def randomDoubleField[G]: Field[State[G, Double]] = new RandomField[G, Double]
}

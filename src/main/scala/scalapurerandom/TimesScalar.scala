package scalapurerandom

import algebra.ring.Field
import simulacrum._

@typeclass trait TimesScalar[A] {
  @op("|*|") def times(x: A, y: Double): A
}

object TimesScalar {
  implicit def doubletimesScalar = new TimesScalar[Double] {
    override def times(x: Double, y: Double): Double = x * y
  }

  implicit val dvMultSemigroup = new TimesScalar[DV] {
    override def times(x: DV, y: Double): DV = x * y
  }
}
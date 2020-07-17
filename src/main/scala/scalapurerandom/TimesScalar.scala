package scalapurerandom

import algebra.ring.Field
import scalapurerandom.RandomPure._
import simulacrum._
import spire.implicits._

@typeclass trait TimesScalar[A] {
  @op("*") def times(x: A, y: Double): A
}

object TimesScalar {
  implicit val dvTimesScalar = new TimesScalar[DV] {
    override def times(x: DV, y: Double): DV = x * y
  }
}
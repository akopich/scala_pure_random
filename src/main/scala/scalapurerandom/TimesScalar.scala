package scalapurerandom

import simulacrum._

@typeclass trait TimesScalar[A] {
  @op("*") def times(x: A, y: Double): A
}

trait TimesScalarHelper extends TimesScalar.ToTimesScalarOps {
  implicit val dvTimesScalar = new TimesScalar[DV] {
    override def times(x: DV, y: Double): DV = x * y
  }
}
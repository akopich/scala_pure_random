package scalapurerandom

import algebra.ring.AdditiveSemigroup

trait DVSemigroup extends AdditiveSemigroup[DV] {
  override def plus(x: DV, y: DV): DV = x + y
}

trait DMSemigroup extends AdditiveSemigroup[DM] {
  override def plus(x: DM, y: DM): DM = x + y
}

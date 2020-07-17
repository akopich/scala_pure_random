package scalapurerandom

import algebra.ring.AdditiveSemigroup

class DVSemigroup extends AdditiveSemigroup[DV] {
  override def plus(x: DV, y: DV): DV = x + y
}

class DMSemigroup extends AdditiveSemigroup[DM] {
  override def plus(x: DM, y: DM): DM = x + y
}

object BreezeInstances {
  implicit val dvIsSemi = new DVSemigroup
}

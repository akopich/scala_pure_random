package scalapurerandom

import cats.data.State
import scalapurerandom.RandomMT.{Gen, Random}

trait Splittable[G] {
  def randomSplit(n: Int): State[G, List[G]]

  def randomSplit(n: PosInt): State[G, NEL[G]]
}


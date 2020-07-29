import breeze.linalg.{DenseMatrix, DenseVector}
import cats.data.{NonEmptyList, NonEmptyVector}

package object scalapurerandom extends NatHelperTrait
  with RandomInstances
  with RandomPure
  with TimesScalarHelper
  with NEVHelper
  with HasSizeHelper
  with AveragebleHelper
  with BreezeInstances {

  type NEV[A] = NonEmptyVector[A]
  def NEV[A] = NonEmptyVector[A] _

  type NEL[A] = NonEmptyList[A]
  def NEL[A] = NonEmptyList[A] _

  type DV = DenseVector[Double]

  type DM = DenseMatrix[Double]
}

import breeze.linalg.{DenseMatrix, DenseVector}
import cats.data.NonEmptyVector

package object scalapurerandom {
  type NEV[A] = NonEmptyVector[A]
  def NEV[A] = NonEmptyVector[A] _

  type DV = DenseVector[Double]
  type DM = DenseMatrix[Double]
}

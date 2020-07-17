import breeze.linalg.{DenseMatrix, DenseVector}
import cats.data.NonEmptyVector

package object scalapurerandom {
  type NEV[A] = NonEmptyVector[A]

  type DV = DenseVector[Double]
  type DM = DenseMatrix[Double]
}

import breeze.linalg.{DenseMatrix, DenseVector}
import cats.Applicative
import cats.data.{NonEmptyList, NonEmptyVector, State, StateT}

package object scalapurerandom extends NatHelperTrait
  with RandomInstances
  with RandomPure
  with TimesScalarHelper
  with NEVHelper
  with HasSizeHelper
  with AveragebleHelper
  with BreezeInstances
  with PSFunctor.ToPSFunctorOps {

  type NEV[A] = NonEmptyVector[A]

  def NEV[A] = NonEmptyVector[A] _

  type NEL[A] = NonEmptyList[A]

  def NEL[A] = NonEmptyList[A] _

  type DV = DenseVector[Double]

  type DM = DenseMatrix[Double]

  def fromState[F[_], A, B](s: State[A, F[B]])(implicit F: Applicative[F]): StateT[F, A, B] =
    s.transformF { eval =>
      val (a, fb) = eval.value
      F.map(fb)((a, _))
    }
}

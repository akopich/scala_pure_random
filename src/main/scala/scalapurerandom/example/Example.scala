package scalapurerandom.example

import spire.algebra.Field
import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import scalapurerandom.RandomPure._
import scalapurerandom.Nat._
import scalapurerandom.{Pos, _}
import scalapurerandom.TimesScalar.ops._
import breeze.linalg._
import breeze.numerics._
import cats.kernel.instances.{DoubleGroup, IntGroup}
import scalapurerandom.HeadTailDecomposable.size
import scalapurerandom.Averageble._
import algebra.instances._
import algebra.ring._

object Example extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val standardNormal: Random[Double] = gaussian(0d, 1d)

    val nonStandardNormal: Random[Double] = standardNormal |*| const(200d) |+| const(100d)

    val n: Pos = p"1000"

    val exampleWithDoubles : Random[IO[Unit]] = for {
      (sm, sv)   <- sampleMeanVar(standardNormal, n)
      (nsm, nsv) <- sampleMeanVar(nonStandardNormal, n)
    } yield IO {
      println(s"For standard normal mean = $sm, variance = $sv")
      println(s"For non-standard normal mean = $nsm, variance = $nsv")
    }

    val mean = DenseVector(1d, 10d, 100d)

    val multivariateNormal = centeredGaussian(diag(DenseVector(1d, 2d, 3d))) |+| const(mean)
    val exampleWithVectors: Random[IO[Unit]] = sampleMeanAndCov(multivariateNormal, n).map { case (mean, cov) => IO {
        println(s"Empirical mean = $mean")
        println("Empirical covariance")
        println(cov)
      }
    }

    List(exampleWithDoubles, exampleWithVectors).sequence
      .sample(getGen(13L))
      .reduce(_ |+| _)
      .as(ExitCode.Success)
  }
}

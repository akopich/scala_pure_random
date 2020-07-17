package scalapurerandom.example

import cats.implicits._
import cats.effect._
import scalapurerandom.RandomPure._
import scalapurerandom.Nat._
import scalapurerandom.Pos
import breeze.linalg.{DenseVector, diag}
import scalapurerandom.RandomInstances._
import spire.syntax.field._


object Example extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val standardNormal: Random[Double] = gaussian(0d, 1d)

    val nonStandardNormal: Random[Double] = standardNormal * const(200d) + const(100d)

    val n: Pos = p"1000"

    val exampleWithDoubles : Random[IO[Unit]] = for {
      (sm, sv)   <- sampleMeanVar(standardNormal, n)
      (nsm, nsv) <- sampleMeanVar(nonStandardNormal, n)
    } yield IO {
      println(s"For standard normal mean = $sm, variance = $sv")
      println(s"For non-standard normal mean = $nsm, variance = $nsv")
    }

    val mean = DenseVector(1d, 10d, 100d)


    val multivariateNormal = centeredGaussian(diag(DenseVector(1d, 2d, 3d))) + const(mean)
    val exampleWithVectors: Random[IO[Unit]] = sampleMeanAndCov(multivariateNormal, n).map { case (mean, cov) => IO {
        println(s"Empirical mean = $mean")
        println("Empirical covariance")
        println(cov)
      }
    }

    val cochi: Random[Double] = standardNormal / standardNormal
    val exampleCochi: Random[IO[Unit]] = sampleMeanVar(cochi, p"100000").map { case (mean, cov) => IO {
        println(s"For Cauchy mean = $mean, variance(undefined, thus diverges) = $cov")
      }
    }

    List(exampleWithDoubles, exampleWithVectors, exampleCochi).sequence
      .sample(getGen(13L))
      .reduce(_ *> _)
      .as(ExitCode.Success)
  }
}

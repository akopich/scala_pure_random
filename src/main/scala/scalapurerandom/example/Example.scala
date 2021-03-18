package scalapurerandom.example

import java.lang.management.ManagementFactory

import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import spire.syntax.field._
import breeze.linalg.{DenseVector, diag}
import scalapurerandom._
import ParReducibleInstance._
import ParFunctorInstances._
import RandomMT._

object Example extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val a: PosInt = size(NonEmptyVector.of(1,2,3))
    val aa: Nat = size(List(1,2,3))

    val standardNormal: Random[Double] = gaussian(0d, 1d)

    val nonStandardNormal: Random[Double] = standardNormal * const(200d) + const(100d)

    val n: PosInt = p"100000"

    val exampleWithDoubles : Random[IO[Unit]] = for {
      (sm, sv)   <- sampleMeanVar(standardNormal, n)
      (nsm, nsv) <- sampleMeanVar(nonStandardNormal, n)
    } yield IO {
      println(s"For standard normal mean = $sm, variance = $sv")
      println(s"For non-standard normal mean = $nsm, variance = $nsv")
    }

    val mean: DV = DenseVector(1d, 10d, 100d)

    val multivariateNormal = centeredGaussian(diag(DenseVector(1d, 2d, 3d))) + const(mean)
    val exampleWithVectors: Random[IO[Unit]] = sampleMeanAndCov(multivariateNormal, n).map { case (mean, cov) => IO {
        println(s"Empirical mean = $mean")
        println("Empirical covariance")
        println(cov)
      }
    }

    val cochi: Random[Double] = standardNormal / standardNormal
    val exampleCochi: Random[IO[Unit]] = sampleMeanVar(cochi, n).map { case (mean, cov) => IO {
        println(s"For Cauchy mean = $mean, variance(undefined, thus diverges) = $cov")
      }
    }

    val examplePar: RandomT[IO, Unit] = sampleMeanPar(nonStandardNormal, pow(p"2", p"22")).map { mean =>
      println(s"Mean estimated in parallel is $mean")
    }

    val io: IO[List[Unit]] = (List(exampleWithDoubles, exampleWithVectors, exampleCochi)
      .map(fromState(_)) :+ examplePar).sequence.sample(getGen(13L))

    io.as(ExitCode.Success)
  }
}

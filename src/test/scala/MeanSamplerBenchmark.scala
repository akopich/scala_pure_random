import PSReducibleBenchmark.{data, using}
import org.scalameter.api._
import scalapurerandom._

import cats._
import cats.data._
import cats.implicits._

import cats.effect.{ContextShift, IO}
import scala.concurrent.ExecutionContext


object MeanSamplerBenchmark extends Bench.LocalTime {
  val sizes = Gen.range("size")(10000, 30000, 10000)

  val random = uniform01

  performance of "Sample Mean" in {
    measure method "sequential" in {
      using(sizes) in {
        size => {
          implicit val instance: PSReducible[NEL] = SeqReducibleInstance.seqReducible[NEL]
          sampleMean(random, PosInt(size)).sample(getGen(13L))
        }
      }
    }

    measure method "sequential with parallel aggregate" in {
      using(sizes) in {
        size => {
          import ParReducibleInstance._
          sampleMean(random, PosInt(size)).sample(getGen(13L))
        }
      }
    }


    measure method "parallel" in {
      using(sizes) in {
        size => {
          implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
          sampleMeanPar(random, PosInt(size)).sample(getGen(13L)).unsafeRunSync()
        }
      }
    }
  }

}

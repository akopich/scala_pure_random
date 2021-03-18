import org.scalameter.api._
import scalapurerandom._
import RandomMT._

object PSReducibleBenchmark extends Bench.LocalTime {
  val sizes = Gen.range("size")(1000000, 3000000, 1000000)

  val data = sizes.map(size => replicateAV(PosInt(size), uniform01).sample(getGen(13L)))

  performance of "Average" in {
    measure method "parallel average" in {
      import ParReducibleInstance._
      using(data) in average
    }

    measure method "sequential average" in {
      import SeqReducibleInstance._
      using(data) in average
    }
  }
}

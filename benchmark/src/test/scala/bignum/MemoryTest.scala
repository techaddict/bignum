package bignum

import org.scalameter.api._

class MemoryTest extends PerformanceTest.Regression {

  def persistor = new SerializationPersistor
  override def measurer = new Executor.Measurer.MemoryFootprint

  val sizes = Gen.range("size")(1000, 5000, 1000)
  val ranges = for {
    size <- sizes
  } yield BigInt2((1 to size).mkString)

  performance of "BigInt2" in {
    measure method "map" config (
      exec.benchRuns -> 10,
      exec.independentSamples -> 1
    ) in {
      using(ranges) in {
        r => r + r
      }
    }
  }

}

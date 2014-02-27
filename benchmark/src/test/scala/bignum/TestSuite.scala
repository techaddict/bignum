package bignum

import org.scalameter.api._

class TestSuite extends PerformanceTest.Regression {
  def persistor = Persistor.None

  include[MemoryTest]
  include[RegressionTest]
}

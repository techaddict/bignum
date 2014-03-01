package bignum

import com.google.caliper.Param
import com.google.caliper.{Runner => CaliperRunner}

object ShiftBenchmark {
  def main(args: Array[String]) {
    CaliperRunner.main(classOf[ShiftBenchmark], args: _*)
  }
}

class ShiftBenchmark extends SimpleScalaBenchmark {

  @Param(Array("32", "64", "200", "500", "1000", "5000", "10000", "50000"))
  val param: Int = 0

  var bigint = BigInt("0")
  var bigint2 = BigInt2("0")
  var biginteger = new java.math.BigInteger("0")
  var shift = 0

  override def setUp() {
    val rng = new java.util.Random(System.currentTimeMillis)
    val a = new java.math.BigInteger(param, rng).toString
    shift = rng.nextInt(param)

    bigint = BigInt(a)
    bigint2 = BigInt2(a)
    biginteger = new java.math.BigInteger(a)
  }

  def timeBigIntLeft(reps: Int) = repeat(reps) {
    var result = bigint
    var i = 0
    while (i < 2) {
      result = result << shift
      i = i + 1
    }
    result
  }

  def timeBigInt2Left(reps: Int) = repeat(reps) {
    var result = bigint2
    var i = 0
    while (i < 2) {
      result = result << shift
      i = i + 1
    }
    result
  }

  def timeBigIntegerLeft(reps: Int) = repeat(reps) {
    var result = biginteger
    var i = 0
    while (i < 2) {
      result = result.shiftLeft(shift)
      i = i + 1
    }
    result
  }

}

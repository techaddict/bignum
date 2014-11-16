package bignum.benchmark

import com.google.caliper.Param
import com.google.caliper.{Runner => CaliperRunner}
import bignum.BigInt2

import annotation.tailrec

object MultiplyBenchmark {
  def main(args: Array[String]) {
    CaliperRunner.main(classOf[MultiplyBenchmark], args: _*)
  }
}

class MultiplyBenchmark extends SimpleScalaBenchmark {

  @Param(Array("200", "500", "1000", "5000", "10000", "20000"))
  val length: Int = 0

  var bigint = BigInt("0")
  var bigint2 = BigInt2("0")
  var biginteger = new java.math.BigInteger("0")
  override def setUp() {
    val rng = new java.util.Random()
    val a = new java.math.BigInteger(length, rng).toString

    bigint = BigInt(a)
    bigint2 = BigInt2(a)
    biginteger = new java.math.BigInteger(a)
  }

  def timeBigInt(reps: Int) = repeat(reps) {
    var result = bigint
    tfor(0)(_ < 5, _ + 1) { i =>
      result = result * result
    }
    result
  }

  def timeBigInt2(reps: Int) = repeat(reps) {
    var result = bigint2
    tfor(0)(_ < 5, _ + 1) { i =>
      result = result * result
    }
    result
  }

  def timeBigInteger(reps: Int) = repeat(reps) {
    var result = biginteger
    tfor(0)(_ < 5, _ + 1) { i =>
      result = result.multiply(result)
    }
    result
  }

  @tailrec
  final def tfor[@specialized T](i: T)(test: T => Boolean, inc: T => T)(f: T => Unit) {
    if(test(i)) {
      f(i)
      tfor(inc(i))(test, inc)(f)
    }
  }

}

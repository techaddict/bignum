package bignum.benchmark

import com.google.caliper.Param
import com.google.caliper.{Runner => CaliperRunner}
import org.apfloat.Apint
import bignum.BigInt2

import annotation.tailrec

object DivideBenchmark {
  def main(args: Array[String]) {
    CaliperRunner.main(classOf[DivideBenchmark], args: _*)
  }
}

class DivideBenchmark extends SimpleScalaBenchmark {

  @Param(Array("200", "500", "1000"))
  val length: Int = 0

  var bigint = BigInt("0")
  var bigint2 = BigInt2("0")
  var biginteger = new java.math.BigInteger("0")
  var apint = new Apint("0")
  var bigint0 = BigInt("0")
  var bigint20 = BigInt2("0")
  var biginteger0 = new java.math.BigInteger("0")
  var apint0 = new Apint("0")
  override def setUp() {
    val rng = new java.util.Random()
    val a0 = new java.math.BigInteger(length, rng).toString
    val a = new java.math.BigInteger(length*3, rng).toString

    bigint = BigInt(a)
    bigint2 = BigInt2(a)
    biginteger = new java.math.BigInteger(a)
    apint = new Apint(a)
    bigint0 = BigInt(a0)
    bigint20 = BigInt2(a0)
    biginteger0 = new java.math.BigInteger(a0)
    apint0 = new Apint(a0)
  }

  def timeBigInt(reps: Int) = repeat(reps) {
    var result = bigint
    var result0 = bigint0
    tfor(0)(_ < 2, _ + 1) { i =>
      result = result / result0
    }
    result
  }

  def timeBigInt2(reps: Int) = repeat(reps) {
    var result = bigint2
    var result0 = bigint20
    tfor(0)(_ < 2, _ + 1) { i =>
      result = result / result0
    }
    result
  }

  def timeBigInteger(reps: Int) = repeat(reps) {
    var result = biginteger
    var result0 = biginteger0
    tfor(0)(_ < 2, _ + 1) { i =>
      result = result.divide(result0)
    }
    result
  }

  def timeApint(reps: Int) = repeat(reps) {
    var result = apint
    var result0 = apint0
    tfor(0)(_ < 2, _ + 1) { i =>
      result = result.divide(result0)
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

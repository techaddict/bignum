package bignum

import com.google.caliper.Param

class Multiply extends SimpleScalaBenchmark {

  @Param(Array("32", "64", "200", "500", "1000", "5000", "10000", "50000"))
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
    var i = 0
    while (i < 5) {
      result = result * result
      i = i + 1
    }
    result
  }

  def timeBigInt2(reps: Int) = repeat(reps) {
    var result = bigint2
    var i = 0
    while (i < 5) {
      result = result * result
      i = i + 1
    }
    result
  }

  def timeBigInteger(reps: Int) = repeat(reps) {
    var result = biginteger
    var i = 0
    while (i < 5) {
      result = result.multiply(result)
      i = i + 1
    }
    result
  }

  override def tearDown() {
  }

}

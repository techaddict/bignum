package bignum

import scala.annotation.tailrec
import scala.math.{ ScalaNumber, ScalaNumericConversions }

import java.util.ArrayList

import UtilCommon._
import UtilBigEndian._
import UtilLittleEndian._

@SerialVersionUID(1L)
final class BigInt2 private[bignum](sign0: Int, digits0: Array[Int])
  extends ScalaNumber with ScalaNumericConversions with Ordered[BigInt2]
  with Serializable {

  if (sign0 < -1 || sign0 > 1)
    throw new IllegalArgumentException(sign0 + "signum Should be either -1, +1, 0")
  private[bignum] var sign: Int = sign0
  private[bignum] var digits: Array[Int] = digits0

  def +(that: BigInt2): BigInt2 = {
    /* Check if one of the numbers are zero and return the other one.
     * `that` needs to be checked first, so that a NPE gets thrown if it is null. */
    if (this.isZero) return that
    else if (that.isZero) return this
    // Check if both numbers have the same sign.
    // If true, keep the sign and add the numbers.
    else if (this.signum == that.signum) {
      val resDigits =
        if (this.digits.length >= that.digits.length)
          arrayPlusArray(this.digits, that.digits)
        else
          arrayPlusArray(that.digits, this.digits)
      new BigInt2(this.signum, resDigits)
    }
    // Different signs.
    else {
      // Compare arrays:
      val cmp = compareArrays(this.digits, that.digits)
      if (cmp != 0){
        val resDigits =
          if (cmp > 0)
            arrayMinusArray(this.digits, that.digits)
          else
            arrayMinusArray(that.digits, this.digits)
        val resSign =
          if (cmp == this.signum) 1
          else -1
        BigInt2(resSign, resDigits)
      }
      // Same value, different sign:
      else BigInt2.zero
    }
  }

  def -(that: BigInt2): BigInt2 = {
    // If that is zero, return this.
    if (that.signum == 0) this
    // If this is Zero, return that, negated.
    else if (this.signum == 0) -that
    // Invariant: Both numbers are non-zero.
    // Different Signs
    else if (this.signum != that.signum) {
      val resultArr =
        if (this.digits.length >= that.digits.length)
          arrayPlusArray(this.digits, that.digits)
        else
          arrayPlusArray(that.digits, this.digits)
      new BigInt2(this.signum, resultArr)
    }
    // Same signs.
    // Compare arrays:
    else {
      val result = compareArrays(this.digits, that.digits)
      if (result != 0) {
        val resArr =
          if (result > 0)
            arrayMinusArray(this.digits, that.digits)
          else
            arrayMinusArray(that.digits, this.digits)
        val resSign =
          if (result == this.signum) 1
          else -1
        new BigInt2(resSign, resArr)
      }
      // Same value, Same sign:
      else BigInt2.zero
    }
  }

  def *(that: BigInt2): BigInt2 = {
    /* `that` needs to be checked first, so that a NPE gets thrown if it is null. */
    val xlen = mag.length
    val ylen = that.mag.length

    if (that.isZero || this.isZero)
      BigInt2.zero
    else if (this.isOne)
      that
    else if (that.isOne)
      this
    else if ((xlen < KaratsubaThreshold) || (ylen < KaratsubaThreshold)) {
      val resSign = if (this.sign != that.sign) -1 else 1
      val resDigits = inplaceMultArrays(this.digits, that.digits)
      BigInt2(resSign, removeLeadingZeroes(resDigits))
    }
    else if ((xlen < ToomCookThreshold) && (ylen < ToomCookThreshold))
      return multiplyKaratsuba(this, that);
    else if (!shouldMultiplySchoenhageStrassen(xlen) || !shouldMultiplySchoenhageStrassen(ylen))
      return multiplyToomCook3(this, that);
    else
      return multiplySchoenhageStrassen(this, that)
  }

  def /(that: BigInt2): BigInt2 = {
    if (that.isZero)
      throw new ArithmeticException

    if (this.isZero)
      return BigInt2.zero

    if (that.isOne)
      return this

    if (mag.length < BurnikelZieglerThreshold || that.mag.length < BurnikelZieglerThreshold)
      return divideKnuth(that);
    else if (!shouldDivideBarrett(mag.length * 32) || !shouldDivideBarrett(that.mag.length * 32))
      return divideBurnikelZiegler(that);
    else
      return divideBarrett(that);
  }

  def abs = if (sign < 0) -this else this

  def withSignum(signum: Int) =
    if (signum < -1 || signum > 1) {
      throw new IllegalArgumentException(s"Invalid signum: $signum")
    } else if (this.signum == signum) {
      this
    } else if (signum == 0) {
      BigInt2.zero
    } else {
      new BigInt2(signum, digits)
    }

  def unary_- : BigInt2 = BigInt2(-sign, digits)
  def signum = sign

  /** Returns a BigInt2 whose value is (`this` raised to the power of `exp`). */
  def pow(exponent: Int): BigInt2 = {
    if (exponent < 0)
      throw new ArithmeticException(s"Negative exponent: $exponent");

    if (signum == 0) {
      if (exponent == 0)
        return BigInt2.one
      else
        return BigInt2.zero
    } else {
      if (exponent == 0)
        return BigInt2.one
      if (exponent == 1)
        return this
    }

    var partToSquare = this.abs

    // Factor out powers of two from the base, as the exponentiation of
    // these can be done by left shifts only.
    // The remaining part can then be exponentiated faster. The
    // powers of two will be multiplied back at the end.
    val powersOfTwo = partToSquare.lowestSetBit // getLowestSetBit

    var remainingBits = 0

    // Factor the powers of two out quickly by shifting right, if needed.
    if (powersOfTwo > 0) {
      partToSquare = partToSquare >> powersOfTwo
      remainingBits = partToSquare.bitLength
      if (remainingBits == 1) // Nothing left but +/- 1?
        if (signum < 0 && (exponent & 1) == 1)
          return BigInt2.minusOne << (powersOfTwo * exponent)
        else
          return BigInt2.one << (powersOfTwo * exponent)
    } else {
      remainingBits = partToSquare.bitLength
      if (remainingBits == 1) // Nothing left but +/- 1?
        if (signum < 0 && (exponent & 1) == 1)
          return BigInt2.minusOne
        else
          return BigInt2.one
    }

    // This is a quick way to approximate the size of the result,
    // similar to doing log2[n] * exponent. This will give an upper bound
    // of how big the result can be, and which algorithm to use.
    val scaleFactor = remainingBits * exponent;

    // Use slightly different algorithms for small and large operands.
    // See if the result will safely fit into a long. (Largest 2^63-1)
    if (partToSquare.mag.length == 1 && scaleFactor <= 62) {
      // Small number algorithm. Everything fits into a long.
      val newSign: Int = if (signum < 0 && (exponent & 1) == 1) -1 else 1
      var result: Long = 1;
      var baseToPow2: Long = partToSquare.mag(0) & 0xFFFFFFFFL

      var workingExponent: Int = exponent;

      // Perform exponentiation using repeated squaring trick
      while (workingExponent != 0) {
        if ((workingExponent & 1) == 1)
          result = result * baseToPow2;

        workingExponent >>>= 1
        if (workingExponent != 0)
          baseToPow2 = baseToPow2 * baseToPow2;
      }

      // Multiply back the powers of two (quickly, by shifting left)
      if (powersOfTwo > 0) {
        val bitsToShift: Int = powersOfTwo * exponent;
        if (bitsToShift + scaleFactor <= 62) // Fits in long?
          return BigInt2((result << bitsToShift) * newSign)
        else
          return BigInt2(result * newSign) << bitsToShift
      } else
        return BigInt2(result * newSign)
    } else {
      // Large number algorithm. This is basically identical to
      // the algorithm above, but calls multiply() and square()
      // which may use more efficient algorithms for large numbers.
      var answer: BigInt2 = BigInt2.one;

      var workingExponent: Int = exponent;
      // Perform exponentiation using repeated squaring trick
      while (workingExponent != 0) {
        if ((workingExponent & 1) == 1)
          answer = answer * partToSquare

        workingExponent >>>= 1
        if (workingExponent != 0)
          partToSquare = partToSquare.square
      }
      // Multiply back the (exponentiated) powers of two (quickly,
      // by shifting left)
      if (powersOfTwo > 0)
        answer = answer << (powersOfTwo * exponent);

      if (signum < 0 && (exponent & 1) == 1)
        return -answer
      else
        return answer
    }
  }

  def min(a: BigInt2): BigInt2 = if (this.compare(a) < 0) this else a
  def max(a: BigInt2): BigInt2 = if (this.compare(a) > 0) this else a
  final def compare(that: BigInt2): Int = {
    if (this.signum == that.signum) {
      (this.signum: @annotation.switch) match {
        case 1 => compareArrays(this.digits, that.digits)
        case -1 => compareArrays(that.digits, this.digits)
        case 0 => 0
      }
    } else {
      if (this.signum > that.signum) 1
      else -1
    }
  }

  /**
    * Returns this value as an `Int`.
    * If the magnitude is too large, -1 will be returned.
    */
  def intValue: Int = if (digits.length < 2) sign * digits(0) else -1

  /**
    * Returns this value as a `Long`.
    * If the magnitude is too large, -1 will be returned.
    */
  def longValue: Long = {
    val value =
      if (digits.length > 1) ((digits(1).toLong) << 32) | (digits(0) & 0xFFFFFFFFL)
      else (digits(0) & 0xFFFFFFFFL)
    if (digits.length < 3) sign * value else -1L
  }

  /**
    * Returns this value as a `Float`. Might lose precision.
    * If the magnitude is too large, `Float.MaxValue` (iff `sign == 1`)
    * or `Float.MinValue` (iff `sign == -1`) are returned.
    */
  def floatValue: Float = toString.toFloat

  /**
    * Returns this value as a `Double`. Might lose precision.
    * If the magnitude is too large, `Double.MaxValue` (iff `sign == 1`)
    * or `Double.MinValue` (iff `sign == -1`) are returned.
    */
  def doubleValue: Double = toString.toDouble

  def isWhole = true
  def underlying = this

  /** Leftshift of BigInt2. */
  def <<(n: Int): BigInt2 =
    if ((n == 0) || (sign == 0))
      this
    else if (n > 0)
      shiftLeft(this, n)
    else
      shiftRight(this, -n)

  /** (Signed) rightshift of BigInt2. */
  def >>(n: Int): BigInt2 =
    if ((n == 0) || (sign == 0))
      this
    else if (n > 0)
      shiftRight(this, n)
    else
      shiftLeft(this, -n)

  def testBit(n: Int): Boolean = {
    if (n < 0)
      throw new IllegalArgumentException("Negative Bit for Testing")
    else if (this.isZero) false
    else {
      (getLittleEndianElem(this, n / 32) & (1 << (n % 32))) != 0
    }
  }

  /**
    * Get the the index, which contains first nonZero Element
    * Little Endian
    */
  private[bignum] def firstNonZeroElem: Int = {
    var pos = digits.length - 1
    while (pos >= 0 && digits(pos) == 0)
      pos -= 1
    digits.length - 1 - pos
  }

  def unary_~ : BigInt2 = {
    val result = new Array[Int](digits.length)
    var i = 0
    while (i < digits.length) {
      result(i) = ~digits(i)
      i += 1
    }
    new BigInt2(???, result)
  }

  /** Bitwise and of BigInt2s. */
  def &(that: BigInt2): BigInt2 = ???

  /** Bitwise or of BigInt2s. */
  def |(that: BigInt2): BigInt2 = ???

  /** Bitwise exclusive-or of BigInt2s. */
  def ^(that: BigInt2): BigInt2 = ???

  /** Bitwise and-not of BigInt2s. Returns a BigInt2 whose value is (this & ~that). */
  def &~(that: BigInt2): BigInt2 = ???

  private[this] def equalsArrays(a: Array[Int]): Boolean =
    if (a.length != digits.length)
      false
    else {
      var i = digits.length - 1
      while ((i >= 0) && (digits(i) == a(i)))
        i -= 1
      (i < 0)
    }
  override def equals(that: Any): Boolean = that match {
    case a: BigInt2 => compare(a) == 0
    case b: BigInt => toString == b.toString
    case _ => false
  }

  def isOne = this equals BigInt2.one
  def isZero: Boolean = this equals BigInt2.zero
  def isPositive: Boolean = this.signum == 1
  def isNegative: Boolean = this.signum == -1
  def isEven: Boolean = isZero || ((digits(0) & 0x1) == 0)
  def isOdd: Boolean = !isZero && ((digits(0) & 0x1) == 1)

  def isValidLong: Boolean = this >= BigInt2(Long.MinValue) && this <= BigInt2(Long.MaxValue)
  /** Returns `true` iff this can be represented exactly by [[scala.Float]]; otherwise returns `false`. */
  def isValidFloat = {
    val bitLen = bitLength
    (bitLen <= 24 ||
      {
        val lowest = lowestSetBit
        bitLen <= java.lang.Float.MAX_EXPONENT + 1 && // exclude this < -2^128 && this >= 2^128
          lowest >= bitLen - 24 &&
          lowest < java.lang.Float.MAX_EXPONENT + 1 // exclude this == -2^128
      })
  }
  /** Returns `true` iff this can be represented exactly by [[scala.Double]]; otherwise returns `false`. */
  def isValidDouble = {
    val bitLen = bitLength
    (bitLen <= 53 ||
      {
        val lowest = lowestSetBit
        bitLen <= java.lang.Double.MAX_EXPONENT + 1 && // exclude this < -2^1024 && this >= 2^1024
          lowest >= bitLen - 53 &&
          lowest < java.lang.Double.MAX_EXPONENT + 1 // exclude this == -2^1024
      })
  }

  def square: BigInt2 = {
    if (signum == 0)
      return BigInt2.zero
    else
      this * this
  }

  /**
    * Returns the number of bits in the minimal two's-complement representation of this BigInt,
    * excluding a sign bit.
    */
  def bitLength: Int = {
    if (isZero) 0
    else if (signum < 0) {
      val elem = digits(0) + (if (firstNonZeroElem == digits.length - 1) -1 else 0)
      digits.length * 32 - Integer.numberOfLeadingZeros(elem)
    }
    else
      digits.length * 32 - Integer.numberOfLeadingZeros(digits(0))
  }

  /**
    * Returns the number of bits in the two's complement representation of this BigInt
    * that differ from its sign bit.
    */
  def bitCount: Int = {
    var i = 0
    var bits = 0
    val len = digits.length
    if (isNegative) {
      i = firstNonZeroElem + 1
      bits = (i << 5) - Integer.bitCount(-digits(len - i))
    }
    i = len - 1 - i
    while (i >= 0) {
      bits += Integer bitCount digits(i)
      i -= 1
    }
    bits
  }

  // Get's the lowest Set Bit and if sign is 0 then -1
  def lowestSetBit: Int = {
    if (sign == 0) -1
    else {
      val i = firstNonZeroElem
      return ((i << 5) + Integer.numberOfTrailingZeros(digits(digits.length - 1 - i)))
    }
  }

  /** Returns a BigInteger whose value is {@code (this / val)} using an O(n^2) algorithm from Knuth.
    *
    * @param val value by which this BigInteger is to be divided.
    * @return {@code this / val}
    * @throws ArithmeticException if {@code val} is zero.
    * @see MutableBigInteger#divideKnuth(MutableBigInteger, MutableBigInteger, boolean)
    */
  private[bignum] def divideKnuth(that: BigInt2): BigInt2 = divideAndRemainderKnuth(that)._1
  private[bignum] def remainderKnuth(that: BigInt2): BigInt2 = divideAndRemainderKnuth(that)._2
  private[bignum] def divideAndRemainderKnuth(that: BigInt2): (BigInt2, BigInt2) = {
    if (0 == that.sign) {
      throw new ArithmeticException("BigInt2 divide by zero")
    }
    val thatSign = that.sign
    if (that.isOne) {
      return ((if (that.sign > 0) this else -this), BigInt2.zero)
    }
    val thisSign = sign
    val thisLen = digits.length
    val thatLen = that.digits.length
    if (1 == thatLen) {
      divideAndRemainderByInteger(this, that.digits(0), thatSign)
    }
    val cmp = compareArrays(digits, that.digits)
    if (cmp == 0)
      (if (thisSign == thatSign) BigInt2.one else BigInt2.minusOne, BigInt2.zero)
    else if (cmp < 0) (BigInt2.zero, this)
    else divideAndRemainderKnuthPositive(this, that)
  }
//  {
//    val a = this.abs
//    val b = that.abs
//    val (quot, rem) = divideAndRemainderKnuthPositive(abs, that.ab) // <== FIXME
//    val newSignum = if (this.signum == that.signum) 1 else -1
//    (quot withSignum newSignum, rem withSignum this.signum)
//  }

  /**
    * Estimates whether Barrett Division will be more efficient than Burnikel-Ziegler when
    * dividing two numbers of a given length in bits.
    * @param bitLength the number of bits in each of the two inputs
    * @return <code>true</code> if Barrett is more efficient, <code>false</code> if Burnikel-Ziegler is more efficient
    */
  private def shouldDivideBarrett(bitLength: Int): Boolean = {
    if (bitLength < 3300000)
      return false
    if (bitLength < 4100000)
      return true
    if (bitLength < 5900000)
      return false
    if (bitLength < 8300000)
      return true
    if (bitLength < 9700000)
      return false
    if (bitLength < 16000000)
      return true
    if (bitLength < 19000000)
      return false
    return true
  }

  /**
    * Calculates <code>this / val</code> using the Burnikel-Ziegler algorithm.
    * @param val the divisor
    * @return <code>this / val</code>
    */
  private def divideBurnikelZiegler(that: BigInt2): BigInt2 =
    divideAndRemainderBurnikelZiegler(that)._1

  /**
    * Calculates <code>this % val</code> using the Burnikel-Ziegler algorithm.
    * @param val the divisor
    * @return <code>this % val</code>
    */
  private def remainderBurnikelZiegler(that: BigInt2): BigInt2 =
    divideAndRemainderBurnikelZiegler(that)._2

  /**
    * Computes <code>this / val</code> and <code>this % val</code> using the
    * Burnikel-Ziegler algorithm.
    * @param val the divisor
    * @return an array containing the quotient and remainder
    */
  private def divideAndRemainderBurnikelZiegler(that: BigInt2): (BigInt2, BigInt2) = {
    var (div, rem) = divideAndRemainderBurnikelZieglerPositive(abs, that.abs)

    // fix signs
    if (signum * that.signum < 0)
      div = -div
    if (signum < 0)
      rem = -rem
    return (div, rem)
  }

  /**
    * Returns a <code>BigInteger</code> containing <code>blockLength</code> ints from
    * <code>this</code> number, starting at <code>index*blockLength</code>.<br/>
    * Used by Burnikel-Ziegler division.
    * @param index the block index
    * @param numBlocks the total number of blocks in <code>this</code> number
    * @param blockLength length of one block in units of 32 bits
    * @return
    */
  private[bignum] def getBlock(index: Int, numBlocks: Int, blockLength: Int): BigInt2 = {
    val blockStart: Int = index * blockLength;
    if (blockStart >= mag.length)
      return BigInt2.zero

    var blockEnd = 0
    if (index == numBlocks - 1)
      blockEnd = (bitLength + 31) / 32;
    else
      blockEnd = (index + 1) * blockLength;
    if (blockEnd > mag.length)
      return BigInt2.zero

    val newMag = removeLeadingZeroes(java.util.Arrays.copyOfRange(mag, mag.length - blockEnd, mag.length - blockStart))
    return new BigInt2(signum, newMag)
  }

  /**
    * Returns a number equal to <code>this.shiftRightInts(n).getLower(n)</code>.<br/>
    * Used by Burnikel-Ziegler division.
    * @param n a non-negative number
    * @return <code>n</code> bits of <code>this</code> starting at bit <code>n</code>
    */
  private[bignum] def shiftAndTruncate(n: Int): BigInt2 = {
    if (mag.length <= n)
      return BigInt2.zero
    if (mag.length <= 2 * n) {
      val newMag = removeLeadingZeroes(java.util.Arrays.copyOfRange(mag, 0, mag.length - n))
      return new BigInt2(signum, newMag)
    } else {
      val newMag = removeLeadingZeroes(java.util.Arrays.copyOfRange(mag, mag.length - 2 * n, mag.length - n))
      return new BigInt2(signum, newMag)
    }
  }

  /** Barrett division */
  private def divideBarrett(that: BigInt2): BigInt2 =
    divideAndRemainderBarrett(that)._1

  /** Barrett division */
  private def remainderBarrett(that: BigInt2): BigInt2 =
    divideAndRemainderBarrett(that)._2

  /**
    * Computes <code>this/val</code> and <code>this%val</code> using Barrett division.
    * @param val the divisor
    * @return an array containing the quotient and remainder
    */
  private def divideAndRemainderBarrett(that: BigInt2): (BigInt2, BigInt2) = {
    var (cdiv, crem) = this.abs.divideAndRemainderBarrettPositive(that.abs)
    if (this.signum * that.signum < 0)
      cdiv = -cdiv
    if (signum < 0)
      crem = -crem
    return (cdiv, crem)
  }

  /**
    * Computes <code>this/val</code> and <code>this%val</code> using Barrett division.
    * <code>val</code> must be positive.
    * @param val the divisor
    * @return an array containing the quotient and remainder
    */
  private def divideAndRemainderBarrettPositive(that: BigInt2): (BigInt2, BigInt2) = {
    val m = this.bitLength
    val n = that.bitLength

    if (m < n)
      return (BigInt2.zero, this)
    else if (m <= 2 * n) {
      // this case is handled by Barrett directly
      val mu = that inverse (m - n)
      return barrettBase(that, mu)
    } else {
      // treat each n-bit piece of a as a digit and do long division by val
      // (which is also n bits), reusing the inverse
      val mu2n = that inverse n
      var startBit: Int = m / n * n; // the bit at which the current n-bit piece starts
      var quotient = BigInt2.zero
      var remainder = this >> startBit
      val mask = (BigInt2.one << n) - BigInt2.one // n ones
      while (startBit > 0) {
        startBit -= n;
        val ai = (this >> startBit) & mask
        remainder = (remainder << n) + ai
        val mu = mu2n.shiftRightRounded(2 * n - remainder.bitLength) // mu = 2^(remainder.length-n)/val
        val (cdiv, crem) = remainder.barrettBase(that, mu)
        quotient = (quotient << n) + cdiv
        remainder = crem
      }
      return (quotient, remainder)
    }
  }

  /**
    * Computes <code>this/b</code> and <code>this%b</code>.
    * The binary representation of <code>b</code> must be at least half as
    * long, and no longer than, the binary representation of <code>a</code>.<br/>
    * This method uses the Barrett algorithm as described in
    * <a href="http://treskal.com/kalle/exjobb/original-report.pdf">
    * Fast Division of Large Integers</a>, pg 17.
    * @param b
    * @param mu 2<sup>n</sup>/b where <code>n</code> is the number of binary digits of <code>this</code>
    * @return an array containing the quotient and remainder
    */
  private def barrettBase(b: BigInt2, mu: BigInt2): (BigInt2, BigInt2) = {
    val m = bitLength
    val n = b.bitLength

    val a1 = this >> (n - 1)
    var q = (a1 * mu) >> (m - n + 1)
    var r = this - (b * q)
    while (r.signum < 0 || r.compareTo(b) >= 0)
      if (r.signum < 0) {
        r = r + b
        q = q - BigInt2.one
      } else {
        r = r - b
        q = q + BigInt2.one
      }
    return (q, r)
  }

  /**
    * Computes 2<sup>bitLength()+n</sup>/this.<br/>
    * Uses the
    * <a href="http://en.wikipedia.org/wiki/Division_%28digital%29#Newton.E2.80.93Raphson_division">
    * Newton algorithm</a> as described in
    * <a href="http://treskal.com/kalle/exjobb/original-report.pdf">
    * Fast Division of Large Integers</a>, pg 23.
    * @param n precision in bits
    * @return <code>1/this</code>, shifted to the left by <code>bitLength()+n</code> bits
    */
  private def inverse(n: Int): BigInt2 = {
    val m = bitLength
    if (n <= NewtonThreshold)
      return (BigInt2.one << (n * 2)) divideKnuth (shiftRightRounded(m - n))

    // let numSteps = ceil(log2(n/NEWTON_THRESHOLD)) and initialize k
    var numSteps = bitLengthOf((n + NewtonThreshold - 1) / NewtonThreshold)
    val k = new Array[Int](numSteps)
    var ki = n
    var i = numSteps - 1
    while (i >= 0) {
      ki = (ki + 1) / 2
      k(i) = if (ki < NewtonThreshold) NewtonThreshold else ki
      i -= 1
    }

    // calculate 1/this truncated to k0 fraction digits
    var z = (BigInt2.one << (k(0) * 2)).divideKnuth(shiftRightRounded(m - k(0))) // exp=k0 because exp(this)=m

    var j = 0
    while (j < numSteps) {
      ki = k(j)
      // the following BigIntegers represent numbers of the form a*2^(-exponent)
      val s = z.square // exponent = 2ki
      val t = shiftRightRounded(m - 2 * ki - 3) // exponent = 2ki+3
      val u = t * s // exponent = 4ki+3 > 2ki+1
      var w = z + z // exponent = ki
      w = w << (3 * ki + 3) // increase #fraction digits to 4ki+3 to match u
      z = w - u // exponent = 4ki+3
      if (j < numSteps - 1)
        z = z.shiftRightRounded(4 * ki + 3 - k(j + 1)) // reduce #fraction digits to k[i+1]
      else
        z = z.shiftRightRounded(4 * ki + 3 - n) // final step: reduce #fraction digits to n
      j += 1
    }
    return z;
  }

  /**
    * Same as {@link BigInteger#shiftRight(int)} but rounds to the
    * nearest integer.
    * @param n shift distance, in bits.
    * @return round(this*2<sup>-n</sup>)
    */
  private def shiftRightRounded(n: Int): BigInt2 = {
    var b = this >> n
    if (n > 0 && testBit(n - 1))
      b = b + BigInt2.one
    return b;
  }

  /**
    * Shifts a number to the left by a multiple of 32. Used by Burnikel-Ziegler division.
    * @param n a non-negative number
    * @return <code>this.shiftLeft(32*n)</code>
    */
  private[bignum] def shiftLeftInts(n: Int): BigInt2 = {
    val newMag = removeLeadingZeroes(java.util.Arrays.copyOf(mag, mag.length + n))
    new BigInt2(signum, newMag)
  }

  /**
    * Shifts a number to the right by a multiple of 32. Used by Burnikel-Ziegler division.
    * @param n a non-negative number
    * @return <code>this.shiftRight(32*n)</code>
    */
  private[bignum] def shiftRightInts(n: Int): BigInt2 = {
    if (n >= mag.length)
      BigInt2.zero
    else
      new BigInt2(signum, java.util.Arrays.copyOf(mag, mag.length - n))
  }

  def %(that: BigInt2): BigInt2 = ???

  def /%(that: BigInt2): (BigInt2, BigInt2) = {
    if (that == null)
      throw new NullPointerException
    if (mag.length < BurnikelZieglerThreshold || that.mag.length < BurnikelZieglerThreshold)
      return divideAndRemainderKnuth(that)
    else if (!shouldDivideBarrett(mag.length * 32) || !shouldDivideBarrett(that.mag.length * 32))
      return divideAndRemainderBurnikelZiegler(that)
    else
      return divideAndRemainderBarrett(that)
  }

  /**
    * Returns a BigInt2 whose value is (this mod m).
    * This method differs from `%` in that it always returns a non-negative BigInt2.
    */
  def mod(that: BigInt2): BigInt2 = ???

  /** Returns the greatest common divisor of `this.abs` and `that.abs`. */
  def gcd(that: BigInt2): BigInt2 = {
    if (this.isZero)
      that.abs
    else if (that.isZero)
      this.abs
    else if (this.digits.length == 1 && this.digits(0) == 1 || that.digits.length == 1 && that.digits(0) == 1)
      BigInt2.one
    else
      ???
  }

  /**
    * Returns a new BigInteger representing n lower ints of the number.
    * This is used by Karatsuba multiplication, Karatsuba squaring,
    * and Burnikel-Ziegler division.
    */
  final def lowerInts(n: Int): BigInt2 = {
    val len = mag.length;

    if (len <= n)
      return this

    val lowerInts = new Array[Int](n)
    scala.compat.Platform.arraycopy(mag, len - n, lowerInts, 0, n);

    return new BigInt2(1, removeLeadingZeroes(lowerInts))
  }

  /**
    * Returns a new BigInteger representing mag.length-n upper
    * ints of the number. This is used by Karatsuba multiplication,
    * Karatsuba squaring, and Burnikel-Ziegler division.
    */
  final def upperInts(n: Int): BigInt2 = {
    val len = mag.length;

    if (len <= n)
      return BigInt2.zero

    val upperLen = len - n;
    val upperInts = new Array[Int](upperLen)
    scala.compat.Platform.arraycopy(mag, 0, upperInts, 0, upperLen);

    return new BigInt2(1, removeLeadingZeroes(upperInts));
  }

  /**
    * Standard Serialization would work, but we have to make sure
    * that we sanitize the array to verify our invariant of no leading
    * “zeroes” in our magnitude.
    *
    * Otherwise all methods depending on it will be broken.
    */
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.lang.ClassNotFoundException])
  private def readObject(in: java.io.ObjectInputStream): Unit = {
    @inline def setField(name: String, value: Any): Unit = {
      val field = this.getClass.getDeclaredField(name)
      field.setAccessible(true)
      field.set(this, value)
    }

    var inArr = in.readObject.asInstanceOf[Array[Int]]
    setField("arr", removeLeadingZeroes(inArr))

    if (inArr.length != 0) {
      val sign = if (in.readBoolean) 1 else -1
      setField("signum", sign)
    }
  }

  @throws(classOf[java.io.ObjectStreamException])
  private def readReplace(): Object = {
    if (signum == 0) return BigInt2.zero
    else this
  }

  @throws(classOf[java.io.IOException])
  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.writeObject(digits)
    if (signum != 0)
      out.writeBoolean(signum > 0)
    out.close()
  }

  /**
    * Returns a slice of a BigInteger for use in Toom-Cook multiplication.
    * @param lowerSize The size of the lower-order bit slices.
    * @param upperSize The size of the higher-order bit slices.
    * @param slice The index of which slice is requested, which must be a
    * number from 0 to size-1. Slice 0 is the highest-order bits,
    * and slice size-1 are the lowest-order bits.
    * Slice 0 may be of different size than the other slices.
    * @param fullsize The size of the larger integer array, used to align
    * slices to the appropriate position when multiplying different-sized
    * numbers.
    */
  def toomSlice(lowerSize: Int, upperSize: Int, slice: Int, fullsize: Int): BigInt2 = {
    //int start, end, sliceSize, len, offset;

    val len = mag.length;
    val offset = fullsize - len;
    var start = 0
    var end = 0

    if (slice == 0) {
      start = 0 - offset;
      end = upperSize - 1 - offset;
    } else {
      start = upperSize + (slice - 1) * lowerSize - offset;
      end = start + lowerSize - 1;
    }

    if (start < 0)
      start = 0;
    if (end < 0)
      return BigInt2.zero;

    val sliceSize = (end - start) + 1;

    if (sliceSize <= 0)
      return BigInt2.zero;

    // While performing Toom-Cook, all slices are positive and
    // the sign is adjusted when the final number is composed.
    if (start == 0 && sliceSize >= len)
      return this.abs

    val intSlice: Array[Int] = new Array[Int](sliceSize)
    scala.compat.Platform.arraycopy(mag, start, intSlice, 0, sliceSize)

    return new BigInt2(1, removeLeadingZeroes(intSlice))
  }

  /**
  * Does an exact division (that is, the remainder is known to be zero)
  * of the specified number by 3. This is used in Toom-Cook
  * multiplication. This is an efficient algorithm that runs in linear
  * time. If the argument is not exactly divisible by 3, results are
  * undefined. Note that this is expected to be called with positive
  * arguments only.
  */
  private[bignum] def exactDivideBy3: BigInt2 = {
    val len: Int = mag.length;
    var result: Array[Int] = new Array[Int](len)

    var x: Long = 0L
    var w: Long = 0L
    var q: Long = 0L
    var borrow: Long = 0L

    var i = len - 1

    while (i >= 0) {

      x = mag(i) & 0xFFFFFFFFL
      w = x - borrow;
      if (borrow > x) // Did we make the number go negative?
        borrow = 1L;
      else
        borrow = 0L;

      // 0xAAAAAAAB is the modular inverse of 3 (mod 2^32). Thus,
      // the effect of this is to divide by 3 (mod 2^32).
      // This is much faster than division on most architectures.
      q = (w * 0xAAAAAAABL) & 0xFFFFFFFFL
      result(i) = q.toInt

      // Now check the borrow. The second check can of course be
      // eliminated if the first fails.
      if (q >= 0x55555556L) {
        borrow += 1
        if (q >= 0xAAAAAAABL)
          borrow += 1;
      }
      i -= 1
    }
    result = removeLeadingZeroes(result)
    return new BigInt2(signum, result)
  }

  def mag = digits

  override def toString = BigInt2.toDecimalScaledString(this, 0)
  //override def toString: String = toString(10)

  def toString(radix: Int): String = {
    if (radix < Character.MIN_RADIX)
      throw new UnsupportedOperationException(s"Radix $radix is too small. Minimum radix is ${Character.MIN_RADIX}.")
    if (radix > Character.MAX_RADIX)
      throw new UnsupportedOperationException(s"Radix $radix is too large. Maximum radix is ${Character.MAX_RADIX}.")

    if (signum == 0)
      return "0";

    // If it's small enough, use smallToString.
    if (mag.length <= SchönhageBaseConversionThreshold)
      return smallToString(radix)

    // Otherwise use recursive toString, which requires positive arguments.
    // The results will be concatenated into this StringBuilder
    val sb = new java.lang.StringBuilder
    if (signum < 0) {
      BigInt2.toString(-this, sb, radix, 0)
      sb.insert(0, '-');
    } else
      BigInt2.toString(this, sb, radix, 0)

    return sb.toString()
  }

  /** This method is used to perform toString when arguments are small. */
  private def smallToString(radix: Int): String = {
    if (signum == 0)
      return "0"

    ???
  }

  def toBinaryString: String = signToString + (if (isZero) "0" else binStr)

  private def binStr: String = {
    val buf = new StringBuilder

    // Don't pad the first element, so we don't end up with
    // 00000000000000000000000011111111 instead of 11111111.
    buf ++= digits(0).toBinaryString

    var i = 1
    while (i < digits.length) {
      paddedBinaryString(buf, digits(i))
      i += 1
    }
    buf.toString
  }

  private def paddedBinaryString(sb: StringBuilder, i: Int): Unit = {
    val str = i.toBinaryString
    val missingChars = 32 - str.length
    if (missingChars > 0) {
      sb ++= ("0" * missingChars)
    }
    sb ++= str
  }

  def toDebugString: String = signToString + binDebStr

  private def binDebStr = {
    val buf = new StringBuilder
    var i = 0
    while (i < digits.length) {
      val str = digits(i).toBinaryString
      val zeroes = "0" * (32 - str.length)
      buf ++= i + ":" ++= zeroes ++= str += '|'
      i += 1
    }
    buf.toString
  }

  private def signToString = if (signum == -1) "-" else ""

  override def hashCode: Int =
    if (isValidLong)
      unifiedPrimitiveHashcode
    else {
      var hash = 0

      val len = digits.length
      var i = 0
      while (i < len) {
        hash = (31 * hash + (digits(i) & 0xFFFFFFFFL)).toInt
        i += 1
      }

      hash * signum
    }

}

object BigInt2 {
  /**
    * 0 is a bit special: It is the only value which is allowed to have
    * a 0 sign and an empty array as the magnitude.
    *
    * `Zero` is the only instance of value `0`.
    */
  final val zero = new BigInt2(0, Array(0))

  /**
    * `One` is a pre-defined instance of value `1`.
    * Unlike `Zero`, multiple instances with the value of `One` can exist.
    */
  final val one = new BigInt2(1, Array(1))
  final val minusOne = new BigInt2(-1, Array(1))

  private val LogTwo: Double = math.log(2.0)

  lazy val digitFitInInt = Array(-1, -1, 30, 19, 15, 13, 11, 11, 10, 9, 9, 8, 8, 8, 8, 7, 7,
    7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5)
  lazy val bigRadices = Array(-2147483648, 1162261467,1073741824, 1220703125, 362797056,
    1977326743, 1073741824, 387420489, 1000000000, 214358881, 429981696, 815730721,
    1475789056, 170859375, 268435456, 410338673, 612220032, 893871739, 1280000000,
    1801088541, 113379904, 148035889, 191102976, 244140625, 308915776, 387420489,
    481890304, 594823321, 729000000, 887503681, 1073741824, 1291467969, 1544804416,
    1838265625, 60466176)

  /** Stores the maximum exponent for each radix from 0 - 36,
    * so that `radix.pow(DigitsPerLong)` fits into a positive Long value.
    *
    * Note that radices of 0 and 1 are not supported.
    */
  private[bignum] val DigitsPerLong: Array[Int] = Array(
      -1, -1, 62, 39, 31, 27, 24, 22, 20, 19, 18, 18, 17, 17, 16, 16, 15, 15, 15,
      14, 14, 14, 14, 13, 13, 13, 13, 13, 13, 12, 12, 12, 12, 12, 12, 12, 12)

  private var logCache: Array[Double] = null

  /** The cache of powers of each radix.  This allows us to not have to
    * recalculate powers of `radix^(2^n)` more than once.  This speeds
    * Schoenhage recursive base conversion significantly.
    */
  private var powerCache: Array[ArrayList[BigInt2]] = null

  /** Initialize the cache of radix^(2^x) values used for base conversion
    * with just the very first value.  Additional values will be created
    * on demand.
    */
  private def initCaches(): Unit = {
    powerCache = new Array[ArrayList[BigInt2]](Character.MAX_RADIX+1)
    logCache = new Array[Double](Character.MAX_RADIX+1)
    var i = Character.MIN_RADIX

    while (i <= Character.MAX_RADIX) {
      powerCache(i) = new ArrayList[BigInt2](1)
      powerCache(i).add(BigInt2(i))
      logCache(i) = scala.math.log(i)
      i += 1
    }
  }

  initCaches()

  /** Converts the specified BigInteger to a string and appends to
    * `sb`. This implements the recursive Schoenhage algorithm
    * for base conversions.
    *
    * See Knuth, Donald, _The Art of Computer Programming_, Vol. 2,
    * Answers to Exercises (4.4) Question 14.
    *
    * @param u The number to convert to a string.
    * @param sb The StringBuilder that will be appended to in place.
    * @param radix The base to convert to.
    * @param digits The minimum number of digits to pad to.
    */
  private def toString(u: BigInt2, sb: java.lang.StringBuilder, radix: Int, digits: Int): Unit = {
    /* If we're smaller than a certain threshold, use the smallToString
     * method, padding with leading zeroes when necessary. */
    if (u.mag.length <= SchönhageBaseConversionThreshold) {
      val s = u smallToString radix

      // Pad with internal zeros if necessary.
      // Don't pad if we're at the beginning of the string.
      if ((s.length < digits) && (sb.length > 0)) {
        var i = s.length
        while (i < digits) {
          sb.append('0') // do this?
          i += 1
        }
      }

      sb.append(s);
      return ;
    }

    val b = u.bitLength

    // Calculate a value for n in the equation radix^(2^n) = u
    // and subtract 1 from that value. This is used to find the
    // cache index that contains the best value to divide u.
    val n: Int = math.round(math.log(b * BigInt2.LogTwo / (BigInt2 logCache radix)) / BigInt2.LogTwo - 1.0).toInt
    val v = BigInt2.radixConversionCache(radix, n);
    val (quot, rem) = u /% v

    val expectedDigits = 1 << n;

    // Now recursively build the two halves of each number.
    toString(quot, sb, radix, digits - expectedDigits)
    toString(rem, sb, radix, expectedDigits)
  }

  /**
    * Returns the value radix^(2^exponent) from the cache.
    * If this value doesn't already exist in the cache, it is added.
    * <p/>
    * This could be changed to a more complicated caching method using
    * <code>Future</code>.
    */
  private[bignum] def radixConversionCache(radix: Int, exponent: Int): BigInt2 = synchronized {
    var retVal: BigInt2 = null
    val cacheLine: ArrayList[BigInt2] = powerCache(radix)
    val oldSize: Int = cacheLine.size
    if (exponent >= oldSize) {
      cacheLine.ensureCapacity(exponent+1)
      var i = oldSize
      while (i <= exponent){
        retVal = cacheLine.get(i-1).square
        cacheLine.add(i, retVal)
        i += 1
      }
    } else {
      retVal = cacheLine.get(exponent)
    }
    return retVal;
  }

  def apply(a: String): BigInt2 = BigInt2(a, 10)
  def apply(a1: String, radix: Int): BigInt2 = {
    if (a1.length == 0)
      throw new NumberFormatException("Zero length BigInteger")
    val a = a1.dropWhile(_ == '0')
    if (a != "") {
      // Doesn't Handle +123
      val sign = if (a.length > 0 && a(0) == '-') -1 else 1
      val startChar = if (sign == -1) 1 else 0
      val stringLength = a.length + (if (sign == -1) -1 else 0)

      val charsPerInt = BigInt2.digitFitInInt(radix)
      val topChars = stringLength % charsPerInt
      val bigRadixDigitsLength = (stringLength / charsPerInt) + (if (topChars != 0) 1 else 0)
      val digits = new Array[Int](bigRadixDigitsLength)
      val bigRadix = BigInt2.bigRadices(radix - 2)

      var ind = digits.length - 1
      var substrStart = startChar
      var substrEnd = startChar + (if (topChars == 0) charsPerInt else topChars)
      while (substrStart < a.length) {
        val bigRadixDigit = Integer.parseInt(a.substring(substrStart, substrEnd), radix)
        if (bigRadixDigit < 0)
          throw new NumberFormatException("Illegal Digit")
        // digits * bigRadix + bigRadixDigit
        // Mix these two
        val len = digits.length - 1 - ind
        val newDigit = inplaceMultiplyByInt(digits, len, bigRadix)
        digits(ind) = newDigit + inplaceAdd(digits, len, bigRadixDigit)
        ind -= 1
        substrStart = substrEnd
        substrEnd += charsPerInt
      }
      new BigInt2(sign, removeLeadingZeroes(digits))
    }
    else zero
  }
  private[bignum] def apply(sign: Int, value: Int) = new BigInt2(sign, Array(value))
  private[bignum] def apply(sign: Int, value: Array[Int]) = new BigInt2(sign, value)
  /**
    * Creates an instance with the value of the given signed `Int` value.
    *
    * @param num the signed value from which the BigInt is created
    */
  def apply(num: Int): BigInt2 = {
    if (num == 0) return BigInt2.zero
    // Invariant: `num` not Zero
    var newNum = 0
    var newSign = 0

    if (num < 0) {
      // Make num positive again ...
      newNum = -num
      // and make the sign negative.
      newSign = -1
    } else {
      newNum = num
      newSign = 1
    }

    assume(newSign != 0)
    assume(newNum != 0)

    new BigInt2(newSign, Array(newNum))
  }
  /**
    * Creates an instance with the value of the given signed `Long` value.
    *
    * @param num the signed value from which the BigInt is created
    */
  def apply(num: Long): BigInt2 = {
    if (num == 0) return BigInt2.zero
    // Invariant: `num` not Zero
    var newNum = 0L
    var newSign = 0

    if (num < 0) {
      // Make num positive again ...
      newNum = -num
      // and make the sign negative.
      newSign = -1
    } else {
      newNum = num
      newSign = 1
    }

    assume(newSign != 0)
    assume(newNum != 0)

    // Get the upper 32 bits to figure out how large the array needs to be
    val upperInt: Int = highBitsToInt(newNum)
    if (upperInt != 0)
      // We need an array with 2 elements
      new BigInt2(newSign, Array(upperInt, newNum.toInt))
    else
      // We need an array with 1 element
      new BigInt2(newSign, Array(newNum.toInt))
  }

  def random(num: Int, rnd: java.util.Random) =
    if (num < 0)
      throw new IllegalArgumentException("Number of Bits must be non-negative")
    else if (num == 0)
      zero
    else {
      val digits = new Array[Int]((num + 31) >> 5)
      for (i <- 0 until digits.length)
        digits(i) = rnd.nextInt()
      digits(digits.length - 1) >>>= (-num) & 31
      BigInt2(1, removeLeadingZeroes(digits))
    }

  private[bignum] def toDecimalScaledString(bi: BigInt2, scale: Int): String = {
    val digits = bi.digits
    val result = new Array[Char](digits.length * 10 + 9)
    var x = BigInt(0)
    for (i <- digits.length - 1 to 0 by -1)
      x += BigInt(digits(i) & 0xFFFFFFFFL) << (32 * (digits.length - 1 - i))
    if (bi.sign == -1) "-" + x.toString
    else x.toString
  }

  // Could be broken into divideLongByInt
  private[bignum] def divideArrayByInt(dest: Array[Int], src: Array[Int], divisor: Int): Int = {
    var b = divisor & 0xFFFFFFFFL
    @tailrec def compute(pos: Int, trem: Long): Int = {
      if (pos >= 0) {
        var rem = trem
        val temp = (rem << 32) | (src(pos) & 0xFFFFFFFFL)
        val quot =
          if (temp >= 0) {
            rem = (temp % b)
            (temp / b)
          }
          else {
            val aPos = temp >>> 1
            val bPos = divisor >>> 1
            var quot = aPos / bPos
            rem = ((aPos % bPos) << 1) + (temp & 1)
            if ((divisor & 1) != 0) {
              if (quot <= rem)
                rem = -quot
              else if (quot - rem <= b) {
                rem += b - quot
                quot -= 1
              }
              else {
                rem += (b << 1) - quot
                quot -= 2
              }
            }
            quot
          }
        dest(pos) = quot.toInt
        compute(pos - 1, rem)
      }
      else trem.toInt
    }
    compute(src.length - 1, 0L)
  }

  private[bignum] def divide(quot: Array[Int], a: Array[Int], b: Array[Int]): Array[Int] = ???

}

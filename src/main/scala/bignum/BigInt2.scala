package bignum

import scala.annotation.tailrec
import scala.math.{ ScalaNumber, ScalaNumericConversions }

import UtilCommon._
import UtilBigEndian._
import UtilLittleEndian._

@SerialVersionUID(1L)
class BigInt2 private[bignum](sign0: Int, digits0: Array[Int])
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
      new BigInt2(this.signum, removeLeadingZeroes(resDigits))
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
        BigInt2(resSign, removeLeadingZeroes(resDigits))
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
      new BigInt2(this.signum, removeLeadingZeroes(resultArr))
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
        new BigInt2(resSign, removeLeadingZeroes(resArr))
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
      val resLength = this.digits.size + that.digits.size
      val resSign = if (this.sign != that.sign) -1 else 1
      val resDigits = new Array[Int](resLength)
      inplaceMultArrays(resDigits, this.digits.reverse, that.digits.reverse)
      BigInt2(resSign, removeLeadingZeroes(resDigits.reverse))
    }
    else if ((xlen < ToomCookThreshold) && (ylen < ToomCookThreshold))
      return multiplyKaratsuba(this, that);
    else if (!shouldMultiplySchoenhageStrassen(xlen) || !shouldMultiplySchoenhageStrassen(ylen))
      return multiplyToomCook3(this, that);
    else
      return multiplySchoenhageStrassen(this, that)
  }

  def /(divisor: BigInt2): BigInt2 = {
    if (divisor.sign == 0)
      throw new ArithmeticException("Divide by Zero")
    if (divisor.isOne)
      return (if (divisor.sign > 0) this else -this)
    val divisorSign = divisor.sign
    val thisSign = sign
    val thisLen = digits.size
    val divisorLen = divisor.digits.size
    if (thisLen + divisorLen == 2) {
      val value = (if (thisSign != divisorSign) -1 else 1) *
        (digits(0) & 0xFFFFFFFFL) / (divisor.digits(0) & 0xFFFFFFFFL)
      return BigInt2(value)
    }
    val cmp = compareArrays(digits, divisor.digits)
    if (cmp == 0){
      if (thisSign == divisorSign) BigInt2.one
      else BigInt2.minusOne
    }
    else if (cmp < 0)
      BigInt2.zero
    else {
      val resLength = thisLen - divisorLen + 1
      val resDigits = new Array[Int](resLength)
      val resSign = if (thisSign == divisorSign) 1 else -1
      if (divisorLen == 1)
        BigInt2.divideArrayByInt(resDigits, digits, divisor.digits(0))
      else
        BigInt2.divide(resDigits, digits, divisor.digits)
      BigInt2(resSign, removeLeadingZeroes(resDigits))
    }
  }

  def abs = if (sign < 0) -this else this
  def unary_- : BigInt2 = BigInt2(-sign, digits)
  def signum = sign

  def min(a: BigInt2): BigInt2 = if (this.compare(a) < 0) this else a
  def max(a: BigInt2): BigInt2 = if (this.compare(a) > 0) this else a
  def compare(that: BigInt2): Int =
    if (sign > that.sign) 1
    else if (sign < that.sign) -1
    else sign * compareArrays(digits, that.digits)

  def intValue: Int = if (digits.length < 2) sign * digits(0) else -1
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

  def <<(n: Int): BigInt2 =
    if ((n == 0) || (sign == 0))
      this
    else if (n > 0)
      shiftLeft(new BigInt2(this.signum, this.digits.reverse), n)
    else
      shiftRight(new BigInt2(this.signum, this.digits.reverse), -n)

  def >>(n: Int): BigInt2 =
    if ((n == 0) || (sign == 0))
      this
    else if (n > 0)
      shiftRight(new BigInt2(this.signum, this.digits.reverse), n)
    else
      shiftLeft(new BigInt2(this.signum, this.digits.reverse), -n)

  def testBit(n: Int): Boolean = {
    if (n < 0)
      throw new IllegalArgumentException("Negative Bit for Testing")
    else if (this.isZero) false
    else if (n == 0)
      if (this.digits.size > 0) (this.digits(0).unsignedToLong & 1L) != 0
      else false
    // For Positive
    else if (this.signum > 0) {
      checkBit(digits, n)
    }
    // Negative 2's Compliment
    else {
      // Just one problem is all are one's in binary then diff :P
      // of form 2^n -1 should be handled separate
      val res = this.abs + BigInt2.minusOne
      !checkBit(res.digits, n)
    }
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
    if (a.size != digits.size)
      false
    else {
      var i = digits.size - 1
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
  def bitLength: Int = ???

  /**
    * Returns the number of bits in the two's complement representation of this BigInt
    * that differ from its sign bit.
    */
  def bitCount: Int = {
    var i = 0
    var bits = 0
    val len = digits.length
    while (i < len) {
      bits += Integer bitCount digits(i)
      i += 1
    }
    if (isNegative) {
      // TODO?
    }
    bits
  }

  def lowestSetBit: Int = ???

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

      x = mag(i).unsignedToLong
      w = x - borrow;
      if (borrow > x) // Did we make the number go negative?
        borrow = 1L;
      else
        borrow = 0L;

      // 0xAAAAAAAB is the modular inverse of 3 (mod 2^32). Thus,
      // the effect of this is to divide by 3 (mod 2^32).
      // This is much faster than division on most architectures.
      q = (w * 0xAAAAAAABL) & UnsignedIntMask
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

}

object BigInt2 {
  final val minusOne = new BigInt2(-1, Array(1))
  final val zero = new BigInt2(0, Array(0))
  final val one = new BigInt2(1, Array(1))

  lazy val digitFitInInt = Array(-1, -1, 30, 19, 15, 13, 11, 11, 10, 9, 9, 8, 8, 8, 8, 7, 7,
    7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5)
  lazy val bigRadices = Array(-2147483648, 1162261467,1073741824, 1220703125, 362797056,
    1977326743, 1073741824, 387420489, 1000000000, 214358881, 429981696, 815730721,
    1475789056, 170859375, 268435456, 410338673, 612220032, 893871739, 1280000000,
    1801088541, 113379904, 148035889, 191102976, 244140625, 308915776, 387420489,
    481890304, 594823321, 729000000, 887503681, 1073741824, 1291467969, 1544804416,
    1838265625, 60466176)

  def apply(a: String): BigInt2 = BigInt2(a, 10)
  def apply(a1: String, radix: Int): BigInt2 = {
    if (a1.length == 0)
      throw new NumberFormatException("Zero length BigInteger")
    val a = a1.dropWhile(_ == '0')
    if (a != "") {
      // Doesn't Handle +123
      val sign = if (a.size > 0 && a(0) == '-') -1 else 1
      val startChar = if (sign == -1) 1 else 0
      val stringLength = a.size + (if (sign == -1) -1 else 0)

      val charsPerInt = BigInt2.digitFitInInt(radix)
      val topChars = stringLength % charsPerInt
      val bigRadixDigitsLength = (stringLength / charsPerInt) + (if (topChars != 0) 1 else 0)
      val digits = new Array[Int](bigRadixDigitsLength)
      val bigRadix = BigInt2.bigRadices(radix - 2)

      def init(ind: Int, substrStart: Int, substrEnd: Int): Int = {
        if (substrStart < a.size) {
          val bigRadixDigit = Integer.parseInt(a.substring(substrStart, substrEnd), radix)
          if (bigRadixDigit < 0)
            throw new NumberFormatException("Illegal Digit")
          // digits * bigRadix + bigRadixDigit
          val newDigit = multiplyByInt(digits, ind, bigRadix)
          digits(ind) = newDigit + inplaceAdd(digits, ind, bigRadixDigit)
          init(ind + 1, substrEnd, substrEnd + charsPerInt)
        }
        else
          ind
      }
      val substrEnd = startChar + (if (topChars == 0) charsPerInt else topChars)
      init(0, startChar, substrEnd)
      new BigInt2(sign, removeLeadingZeroes(digits.reverse))
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
      new BigInt2(newSign, Array(newNum.toInt, upperInt))
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
      for (i <- 0 until digits.size)
        digits(i) = rnd.nextInt()
      digits(digits.size - 1) >>>= (-num) & 31
      BigInt2(1, removeLeadingZeroes(digits))
    }

  private[bignum] def toDecimalScaledString(bi: BigInt2, scale: Int): String = {
    val digits = bi.digits
    val result = new Array[Char](digits.size * 10 + 9)
    var x = BigInt(0)
    for (i <- digits.size - 1 to 0 by -1)
      x += BigInt(digits(i) & 0xFFFFFFFFL) << (32 * (digits.size - 1 - i))
    if (bi.sign == -1) "-" + x.toString
    else x.toString
  }

  // Could be broken into divideLongByInt
  private[bignum] def divideArrayByInt(dest: Array[Int], src: Array[Int], divisor: Int): Int = {
    var b = divisor & 0xFFFFFFFFL
    @tailrec def compute(pos: Int, trem: Long): Int = {
      if (pos >= 0) {
        var rem = trem
        val temp = (rem << 32) | (src(pos) & 0xffffffffL)
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
        dest(pos) = (quot & 0xFFFFFFFFL).toInt
        compute(pos - 1, rem)
      }
      else trem.toInt
    }
    compute(src.size - 1, 0L)
  }

  private[bignum] def divide(quot: Array[Int], a: Array[Int], b: Array[Int]): Array[Int] = {
    val aLen = a.size
    val bLen = b.size
    val normA = new Array[Int](aLen + 1)
    val normB = new Array[Int](bLen + 1)
    val divisorShift = Integer.numberOfLeadingZeros(b(bLen - 1))
    if (divisorShift != 0) {
      //shiftLeft(normB, b, 0, divisorShift)
      //shiftLeft(normA, a, 0, divisorShift)
    }
    else {
      scala.compat.Platform.arraycopy(a, 0, normA, 0, aLen)
      scala.compat.Platform.arraycopy(b, 0, normB, 0, bLen)
    }
    val firstDivisorDigit = normB(bLen - 1)
    var i = quot.size -1
    var j = aLen
    while (i >= 0) {
      var guessDigit = 0
      if (normA(j) == firstDivisorDigit)
        guessDigit = -1
      else {
        val product = (((normA(j) & 0xFFFFFFFFL) << 32) + (normA(j - 1) & 0xFFFFFFFFL))
        val res = divideLongByInt(product, firstDivisorDigit)
        guessDigit = res.toInt
        var rem = (res >> 32).toInt
        if (guessDigit != 0) {
          guessDigit += 1
          @tailrec def compute(rOverflowed: Boolean, leftHand: Long, rightHand: Long) {
            guessDigit -= 1
            if (!rOverflowed){
              val left = (guessDigit & 0xFFFFFFFFL) * (normB(bLen - 2) & 0xFFFFFFFFL)
              val right = ((rem.toLong << 32) + (normA(j - 2) & 0xFFFFFFFFL))
              val longR = (rem & 0xFFFFFFFFL) + (firstDivisorDigit & 0xFFFFFFFFL)
              if (Integer.numberOfLeadingZeros((longR >>> 32).toInt) < 32)
                if ((leftHand ^ 0x8000000000000000L) > (rightHand ^ 0x8000000000000000L))
                  compute(true, left, right)
              else{
                rem = longR.toInt
                if ((leftHand ^ 0x8000000000000000L) > (rightHand ^ 0x8000000000000000L))
                  compute(false, left, right)
              }
            }
          }
          compute(false, 0L, 0L)
        }
        if (guessDigit != 0) {
          val borrow = multiplyAndSubtract(normA, j - bLen, normB, guessDigit)
          if (borrow != 0) {
            guessDigit -= 1
            var carry = 0L
            for (k <- 0 until bLen) {
              carry += (normA(j - bLen + k) & 0xFFFFFFFFL) + (normB(k) & 0xFFFFFFFFL)
              normA(j - bLen + k) = carry.toInt
              carry >>>= 32
            }
          }
        }
        if (quot != null)
          quot(i) = guessDigit
        j -= 1
        i -= 1
      }
    }
    if (divisorShift != 0) {
      shiftRight(normB, bLen, normA, 0, divisorShift)
      normB
    }
    else {
      scala.compat.Platform.arraycopy(normA, 0, normB, 0, bLen)
      normA
    }
  }

  private[this] def divideLongByInt(a: Long, bInt: Int): Long = {
    val b = bInt & 0xFFFFFFFFL
    if (a >= 0)
      ((a % b) << 32) | ((a / b) & 0xffffffffL)
    else {
      val aPos = a >>> 1
      val bPos = b >>> 1
      var quot: Long = aPos / bPos
      var rem: Long = ((aPos % bPos) << 1) + (a & 1)
      if ((b & 1) != 0) {
        if (quot <= rem)
          rem -= quot
        else if (quot - rem <= b) {
          rem += b - quot
          quot -= 1
        }
        else {
          rem += (b << 1) - quot
          quot -= 2
        }
      }
      (rem << 32) | (quot & 0xffffffffL)
    }
  }

  private[this] def multiplyAndSubtract(a: Array[Int], start: Int, b: Array[Int], c: Int) = {
    val bLen = b.size - 1
    @tailrec def compute(pos: Int, carry0: Long, carry1: Long): Long = {
      if (pos < bLen) {
        val tcarry0 = unsignedMultAddAdd(b(pos), c, carry0.toInt, 0)
        val tcarry1 = carry1 + (a(start + pos) & 0xFFFFFFFFL) - (tcarry0 & 0xFFFFFFFFL)
        a(start + pos) = tcarry1.toInt
        compute(pos + 1, tcarry0 >>> 32, tcarry1 >> 32)
      }
      else carry1 - carry0
    }
    val res = compute(0, 0L, 0L)
    val carry = (a(start + bLen) & 0xFFFFFFFFL) + res
    a(start + bLen) = carry.toInt
    (carry >> 32).toInt
  }

}

package bignum

import scala.annotation.tailrec

class BigInt2 private[bignum](sign0: Int, digits0: Array[Int]) extends Ordered[BigInt2]{
  if (sign0 < -1 || sign0 > 1)
    throw new IllegalArgumentException(sign0 + "signum Should be either -1, +1, 0")
  private[bignum] var sign: Int = sign0
  private[bignum] var digits: Array[Int] = digits0

  //Constructor's
  private[bignum] def this(a1: String, radix: Int) {
    this(0, Array(0))
    if (a1.length == 0)
      throw new NumberFormatException("Zero length BigInteger")
    val a = a1.dropWhile(_ == '0')
    if (a != "") {
      // Doesn't Handle +123
      val sign1 = if (a.size > 0 && a(0) == '-') -1 else 1
      val startChar = if (sign1 == -1) 1 else 0
      val stringLength = a.size + (if (sign1 == -1) -1 else 0)

      val charsPerInt = BigInt2.digitFitInInt(radix)
      val topChars = stringLength % charsPerInt
      val bigRadixDigitsLength = (stringLength / charsPerInt) + (if (topChars != 0) 1 else 0)
      var digits1 = new Array[Int](bigRadixDigitsLength)
      val bigRadix = BigInt2.bigRadices(radix - 2)

      def init(ind: Int, substrStart: Int, substrEnd: Int): Int = {
        if (substrStart < a.size) {
          val bigRadixDigit = Integer.parseInt(a.substring(substrStart, substrEnd), radix)
          if (bigRadixDigit < 0)
            throw new NumberFormatException("Illegal Digit")
          // digits * bigRadix + bigRadixDigit
          val newDigit = BigInt2.multiplyByInt(digits1, ind, bigRadix)
          digits1(ind) = newDigit + BigInt2.inplaceAdd(digits1, ind, bigRadixDigit)
          init(ind + 1, substrEnd, substrEnd + charsPerInt)
        }
        else
          ind
      }
      val substrEnd = startChar + (if (topChars == 0) charsPerInt else topChars)
      init(0, startChar, substrEnd)
      sign = sign1
      digits = digits1
      cutOffLeadingZeroes
    }
  }

  private[bignum] def cutOffLeadingZeroes {
    // should be replaced by dropWhile
    def counter(pos: Int): Int = {
      if (pos >= 0 && digits(pos) == 0)
        counter(pos - 1)
      else pos
    }
    val pos = counter(digits.size - 1)
    // Check if pos != digits last elem
    if (pos != digits.size - 1) {
      digits = digits.dropRight(digits.size - pos - 1)
      // If all are zero only then digits = Array(0) and sign
      if (digits.size == 0)
        digits = Array(0)
    }
  }

  def +(a: BigInt2) = BigInt2.add(this, a)

  def abs = if (sign < 0) -this else this

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
      return BigInt2.valueOf(value)
    }
    val cmp = BigInt2.compareArrays(digits, divisor.digits)
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
      val res = BigInt2(resSign, resDigits)
      res.cutOffLeadingZeroes
      res
    }
  }

  def min(a: BigInt2): BigInt2 = if (this.compare(a) < 0) this else a

  def max(a: BigInt2): BigInt2 = if (this.compare(a) > 0) this else a

  def compare(that: BigInt2): Int =
    if (sign > that.sign) 1
    else if (sign < that.sign) -1
    else sign * BigInt2.compareArrays(digits, that.digits)

  def intValue: Int = if (digits.length < 2) sign * digits(0) else -1

  def longValue: Long = {
    val value =
      if (digits.length > 1) ((digits(1).toLong) << 32) | (digits(0) & 0xFFFFFFFFL)
      else (digits(0) & 0xFFFFFFFFL)
    if (digits.length < 3) sign * value else -1L
  }

  def signum = sign

  def -(a: BigInt2) = this + (-a)

  def *(a: BigInt2) = BigInt2.multiply(this, a)

  def unary_- : BigInt2 = if (sign == 0) this else BigInt2(-sign, digits)

  def <<(n: Int): BigInt2 =
    if ((n == 0) || (sign == 0))
      this
    else if (n > 0)
      BigInt2.shiftLeft(this, n)
    else
      BigInt2.shiftRight(this, -n)

  def >>(n: Int): BigInt2 =
    if ((n == 0) || (sign == 0))
      this
    else if (n > 0)
      BigInt2.shiftRight(this, n)
    else
      BigInt2.shiftLeft(this, -n)

  private[this] def equalsArrays(a: Array[Int]): Boolean =
    if (a.size != digits.size)
      false
    else {
      var i = digits.size - 1
      while ((i >= 0) && (digits(i) == a(i)))
        i -= 1
      (i < 0)
    }

  private[bignum] def isOne = this equals BigInt2.one

  override def equals(that: Any): Boolean = that match {
    case a: BigInt2 =>
      compare(a) == 0
    case b: BigInt =>
      toString == b.toString
    case _ => false
  }

  override def toString = BigInt2.toDecimalScaledString(this, 0)

}

object BigInt2 {
  lazy val minusOne = BigInt2("-1")
  lazy val zero = BigInt2("0")
  lazy val one = BigInt2("1")
  lazy val ten = BigInt2("10")
  lazy val smallValues = Array[BigInt2](zero, one, BigInt2(1, 2), BigInt2(1, 3),
    BigInt2(1, 4), BigInt2(1, 5), BigInt2(1, 6), BigInt2(1, 7),
    BigInt2(1, 8), BigInt2(1, 9), ten )

  lazy val digitFitInInt = Array(-1, -1, 30, 19, 15, 13, 11, 11, 10, 9, 9, 8, 8, 8, 8, 7, 7,
    7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5)
  lazy val bigRadices = Array(-2147483648, 1162261467,1073741824, 1220703125, 362797056,
    1977326743, 1073741824, 387420489, 1000000000, 214358881, 429981696, 815730721,
    1475789056, 170859375, 268435456, 410338673, 612220032, 893871739, 1280000000,
    1801088541, 113379904, 148035889, 191102976, 244140625, 308915776, 387420489,
    481890304, 594823321, 729000000, 887503681, 1073741824, 1291467969, 1544804416,
    1838265625, 60466176)

  def apply(a: String) = new BigInt2(a, 10)
  def apply(a: String, radix: Int) = new BigInt2(a, radix)
  def apply(sign: Int, value: Int) = new BigInt2(sign, Array(value))
  private[bignum] def apply(sign: Int, value: Array[Int]) = new BigInt2(sign, value)
  def apply(a: Int) = valueOf(a)
  def apply(a: Long) = valueOf(a)

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
      val ret = BigInt2(1, digits)
      ret.cutOffLeadingZeroes
      ret
    }

  @inline private[bignum] def valueOf(a: Long): BigInt2 =
    if (a == Long.MinValue)
      BigInt2("-9223372036854775808")
    else if (a < 0)
      -valueOf(-a)
    else {
      val Lo = a.toInt
      val Hi = (a >>> 32).toInt
      if (Hi == 0) BigInt2(a.signum, Lo)
      else BigInt2(a.signum, Array[Int](Lo, Hi))
    }

  @inline private[bignum] def valueOf(a: Int): BigInt2 =
    if (a < 0) {
      if (a != -1) BigInt2(-1, -a)
      else minusOne
    }
    else if (a <= 10)
      smallValues(a)
    else
      BigInt2(1, a)

  private[bignum] def toDecimalScaledString(bi: BigInt2, scale: Int): String = {
    var digits = bi.digits
    var resLengthInChars = digits.size * 10 + 1 + 7
    var result = new Array[Char](resLengthInChars + 1)
    var currentChar = resLengthInChars
    var x = BigInt(0)
    for (i <- digits.size - 1 to 0 by -1)
      x += BigInt(digits(i) & 0xFFFFFFFFL) << (32 * i)
    val ret = x.toString
    if (bi.sign == -1) "-" + ret
    else if (ret == "") "0"
    else ret
  }

  private[bignum] def multiplyByInt(a: Array[Int], aSize: Int, factor: Int): Int =
    multiplyByInt(a, a, aSize, factor)

  private[this] def multiplyByInt(res: Array[Int], a: Array[Int], aSize: Int, factor: Int): Int = {
    @tailrec def compute(pos: Int, carry: Long): Int = {
      if (pos < aSize) {
        val tcarry = unsignedMultAddAdd(a(pos), factor, carry.toInt, 0)
        res(pos) = tcarry.toInt
        compute(pos + 1, tcarry >>> 32)
      }
      else carry.toInt
    }
    compute(0, 0L)
  }

  private[bignum] def unsignedMultAddAdd(a: Int, b: Int, c: Int, d: Int): Long =
    (a & 0xFFFFFFFFL) * (b & 0xFFFFFFFFL) + (c & 0xFFFFFFFFL) + (d & 0xFFFFFFFFL)

  private[bignum] def inplaceAdd(a: Array[Int], aSize: Int, addend: Int): Int = {
    @tailrec def compute(pos: Int, carry: Long): Int = {
      if (pos < aSize && carry != 0) {
        val tcarry = carry + (a(pos) & 0xFFFFFFFFL)
        a(pos) = tcarry.toInt
        compute(pos + 1, tcarry >>> 32)
      }
      else carry.toInt
    }
    compute(0, addend & 0xFFFFFFFFL)
  }

  private[bignum] def compareArrays(a: Array[Int], b: Array[Int]): Int = {
    if (a.size > b.size) 1
    else if (a.size < b.size) -1
    else {
      @tailrec def rec(pos: Int): Int = {
        if (pos >= 0 && a(pos) == b(pos))
          rec(pos - 1)
        else pos
      }
      val pos = rec(a.size - 1)
      if (pos < 0) 0
      else if ((a(pos) & 0xFFFFFFFFL) < (b(pos) & 0xFFFFFFFFL)) -1
      else 1
    }
  }

  private[bignum] def add(a: BigInt2, b: BigInt2): BigInt2 = {
    val aSign = a.sign
    val bSign = b.sign
    var resSign = 0
    var resDigits = Array[Int]()
    if (aSign == 0) return b
    if (bSign == 0) return a
    val aLen = a.digits.size
    val bLen = b.digits.size
    if (aLen + bLen == 2) {
      val a1 = a.digits(0) & 0xFFFFFFFFL
      val b1 = b.digits(0) & 0xFFFFFFFFL
      if (aSign == bSign) return BigInt2.valueOf(aSign * (a1 + b1))
      else return BigInt2.valueOf(if (aSign < 0) b1 - a1 else a1 - b1)
    }
    else if (aSign == bSign) {
      resSign = aSign
      resDigits =
        if (aLen >= bLen) add(a.digits, b.digits)
        else add(b.digits, a.digits)
    }
    else {
      val cmp = compareArrays(a.digits, b.digits)
      if (cmp == 0)
        return zero
      else if (cmp > 0) {
        resSign = aSign
        resDigits = subtract(a.digits, b.digits)
      }
      else {
        resSign = bSign
        resDigits = subtract(b.digits, a.digits)
      }
    }
    var res = BigInt2(resSign, resDigits)
    res.cutOffLeadingZeroes
    res
  }

  private[this] def add(a: Array[Int], b: Array[Int]): Array[Int] = {
    val res = new Array[Int](a.size + 1)
    var carry = (a(0) & 0xFFFFFFFFL) + (b(0) & 0xFFFFFFFFL)
    res(0) = carry.toInt
    @tailrec def compute(pos: Int, carry: Long) {
      if (pos < b.size) {
        val tcarry = carry + (a(pos) & 0xFFFFFFFFL) + (b(pos) & 0xFFFFFFFFL)
        res(pos) = tcarry.toInt
        compute(pos + 1, tcarry >> 32)
      }
      else if (pos < a.size) {
        val tcarry = carry + (a(pos) & 0xFFFFFFFFL)
        res(pos) = tcarry.toInt
        compute(pos + 1, tcarry >> 32)
      }
      else if (carry != 0)
        res(pos) = carry.toInt
    }
    compute(1, carry >> 32)
    res
  }

  private[this] def subtract(a: Array[Int], b: Array[Int]): Array[Int] = {
    val res = new Array[Int](a.size)
    @tailrec def compute(pos: Int, borrow: Long) {
      if (pos < b.size) {
        val tborrow = borrow + (a(pos) & 0xFFFFFFFFL) - (b(pos) & 0xFFFFFFFFL)
        res(pos) = tborrow.toInt
        compute(pos + 1, tborrow >> 32)
      }
      else if (pos < a.size) {
        val tborrow = borrow + (a(pos) & 0xFFFFFFFFL)
        res(pos) = tborrow.toInt
        compute(pos + 1, tborrow >> 32)
      }
    }
    compute(0, 0L)
    return res
  }

  private[bignum] def multiply(a: BigInt2, b: BigInt2): BigInt2 = {
    val resLength = a.digits.size + b.digits.size
    val resSign =
      if (0 == a.sign || 0 == b.sign) 0
      else if (a.sign != b.sign) -1
      else 1
    if (resLength == 2) {
      val value = unsignedMultAddAdd(a.digits(0), b.digits(0), 0, 0)
      BigInt2.valueOf(resSign * value)
    }
    else {
      val resDigits = new Array[Int](resLength)
      multArraysInplace(a.digits, b.digits, resDigits)
      val result = BigInt2(resSign, resDigits)
      result.cutOffLeadingZeroes
      result
    }
  }

  private[this] def multArraysInplace(a: Array[Int], b: Array[Int], res: Array[Int]) {
    val aLen = a.size
    val bLen = b.size
    if (!(aLen == 0 || bLen == 0))
      if (aLen == 1) res(bLen) = multiplyByInt(res, b, bLen, a(0))
      else if (bLen == 1) res(aLen) = multiplyByInt(res, a, aLen, b(0))
      else multInplace(a, b, res)
  }

  private[this] def multInplace(a: Array[Int], b: Array[Int], t: Array[Int]) {
    @tailrec def loop(pos: Int) {
      if (pos < a.size) {
        @tailrec def loopj(posj: Int, carry: Long): Int = {
          if (posj < b.size) {
            var tcarry = unsignedMultAddAdd(a(pos), b(posj), t(pos + posj), carry.toInt)
            t(pos + posj) = tcarry.toInt
            loopj(posj + 1, tcarry >>> 32)
          }
          else carry.toInt
        }
        t(pos + b.size) = loopj(0, 0L)
        loop(pos + 1)
      }
    }
    loop(0)
  }

  private[this] def shiftLeftOnce(result: Array[Int], source: Array[Int], srcLen: Int) {
    @tailrec def compute(pos: Int, carry: Int): Int = {
      if (pos < srcLen) {
        val value = source(pos)
        result(pos) = (value << 1) | carry
        compute(pos + 1, value >>> 31)
      }
      else carry
    }
    val carry = compute(0, 0)
    if (carry != 0)
      result(srcLen) = carry
  }

  private[bignum] def shiftRight(source: BigInt2, count1: Int): BigInt2 = {
    val intCount = count1 >> 5
    val count = count1 & 31
    if (intCount >= source.digits.size)
      if (source.sign < 0) minusOne else zero
    else {
      var resLen = source.digits.size - intCount
      val res = new Array[Int](resLen + 1)
      shiftRight(res, resLen, source.digits, intCount, count)
      if (source.sign < 0) {
        @tailrec def rec(pos: Int): Int = {
          if (pos < intCount && source.digits(pos) == 0)
            rec(pos + 1)
          else pos
        }
        val pos = rec(0)
        if ((pos < intCount) || ((count >0) && ((source.digits(pos) << (32 - count)) != 0))) {
          @tailrec def rec1(pos: Int): Int = {
            if (pos < intCount && source.digits(pos) == -1){
              res(pos + 1) = 0
              rec1(pos + 1)
            }
            else pos
          }
          val i = rec1(0)
          if (i == resLen) resLen += 1
          res(i) += 1
        }
      }
      val result = BigInt2(source.sign, res)
      result.cutOffLeadingZeroes
      result
    }
  }

  private[this] def shiftRight(res: Array[Int], resLen: Int, source: Array[Int],
    intCount: Int, count: Int): Boolean = {
    @tailrec def compute(pos: Int): Boolean = {
      if (pos < intCount)
        if (source(pos) == 0)
          compute(pos + 1)
        else
          false
      else
        true
    }
    if (count == 0) {
      System.arraycopy(source, intCount, res, 0, resLen)
      compute(0)
    }
    else {
      val leftShiftCount = 32 - count
      @tailrec def compute1(pos: Int): Int = {
        if (pos < resLen -1){
          res(pos) = (source(pos + intCount) >>> count) |
            (source(pos + intCount + 1) << leftShiftCount)
          compute1(pos + 1)
        }
        else pos
      }
      val i = compute1(0)
      res(i) = (source(i + intCount) >>> count)
      compute(0) & (source(intCount) << leftShiftCount) == 0
    }
  }

  private[bignum] def shiftLeft(source: BigInt2, count: Int): BigInt2 = {
    val intCount = count >> 5
    val count1 = count & 31
    val resLength = source.digits.size + intCount + (if (count1 == 0) 0 else 1)
    val res = new Array[Int](resLength)
    shiftLeft(res, source.digits, intCount, count1)
    val result = BigInt2(source.sign, res)
    result.cutOffLeadingZeroes
    return result
  }

  private[bignum] def shiftLeft(res: Array[Int], source: Array[Int], intCount: Int, count: Int) {
    if (count == 0)
      System.arraycopy(source, 0, res, intCount, res.size - intCount)
    else {
      val rightShiftCount = 32 - count
      val start = res.size - 1
      res(start) == 0
      @tailrec def compute(pos: Int) {
        if (pos > intCount) {
          res(pos) |= (source(pos - intCount - 1) >>> rightShiftCount)
          res(pos - 1) = source(pos - intCount - 1) << count
          compute(pos - 1)
        }
      }
      compute(start)
    }
    for (i <- 0 until intCount)
      res(i) = 0
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
      BigInt2.shiftLeft(normB, b, 0, divisorShift)
      BigInt2.shiftLeft(normA, a, 0, divisorShift)
    }
    else {
      System.arraycopy(a, 0, normA, 0, aLen)
      System.arraycopy(b, 0, normB, 0, bLen)
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
      System.arraycopy(normA, 0, normB, 0, bLen)
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
package me.techaddict.bignum

import scala.annotation.tailrec

class BigInt2 extends java.lang.Number{
  var sign = 0
  var numberLength = 0
  var digits = Array[Int]()

  //Constructor's
  def this(a1: java.lang.String, radix: scala.Int) {
    this()
    if (a1.length == 0)
      throw new NumberFormatException("Zero length BigInteger")
    val a = removeLeadingZeroes(a1)
    if (a == "") {
      sign = 0
      digits = Array[Int](0)
    }
    else {
      val digitFitInInt = Array(0, 0, 30, 19, 15, 13, 11, 11, 10, 9, 9, 8, 8, 8, 8, 7, 7,
        7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5)
      val bigRadices = Array(-2147483648, 1162261467,1073741824, 1220703125, 362797056,
        1977326743, 1073741824, 387420489, 1000000000, 214358881, 429981696, 815730721,
        1475789056, 170859375, 268435456, 410338673, 612220032, 893871739, 1280000000,
        1801088541, 113379904, 148035889, 191102976, 244140625, 308915776, 387420489,
        481890304, 594823321, 729000000, 887503681, 1073741824, 1291467969, 1544804416,
        1838265625, 60466176)

      // Doesn't Handle +123
      val sign = if (a.size > 0 && a(0) == '-') -1 else 1
      val startChar = if (sign == -1) 1 else 0
      val stringLength = a.length + (if (sign == -1) -1 else 0)

      val charsPerInt = digitFitInInt(radix)
      val topChars = stringLength % charsPerInt
      val bigRadixDigitsLength = (stringLength / charsPerInt) + (if (topChars != 0) 1 else 0)
      var digits = new Array[Int](bigRadixDigitsLength)
      val bigRadix = bigRadices(radix - 2)

      def init(ind: Int, substrStart: Int, substrEnd: Int): Int = {
        if (substrStart < a.length) {
          val bigRadixDigit = Integer.parseInt(a.substring(substrStart, substrEnd), radix)
          if (bigRadixDigit < 0)
            throw new NumberFormatException("Illegal Digit")
          // digits * bigRadix + bigRadixDigit
          val newDigit = BigInt2.multiplyByInt(digits, ind, bigRadix)
          digits(ind) = newDigit + BigInt2.inplaceAdd(digits, ind, bigRadixDigit)
          init(ind + 1, substrEnd, substrEnd + charsPerInt)
        }
        else
          ind
      }
      val substrEnd = startChar + (if (topChars == 0) charsPerInt else topChars)
      val numberLength = init(0, startChar, substrEnd)
      this.sign = sign
      this.digits = digits
      this.numberLength = numberLength
      this.cutOffLeadingZeroes()
    }
  }

  def this(a: java.lang.String) = this(a, 10)

  def this(num: scala.Int, rnd: java.util.Random) {
    this()
    if (num < 0)
      throw new IllegalArgumentException("Number of Bits must be non-negative")
    else if (num == 0) {
      this.sign = 0
      this.numberLength = 1
      this.digits = Array[Int](0)
    }
    else {
      this.sign = 1
      val numberLength = (num + 31) >> 5
      this.numberLength = (num + 31) >> 5
      this.digits = new Array[Int](numberLength)
      for (i <- 0 until numberLength)
        digits(i) = rnd.nextInt()
      digits(numberLength - 1) >>>= (-num) & 31
      cutOffLeadingZeroes()
    }
  }

  def this(sign: Int, numberLength: Int, digits: Array[Int]) {
    this()
    this.sign = sign
    this.numberLength = numberLength
    this.digits = digits
  }

  def this(sign: Int, value: Int) =
    this(sign, 1, Array(value))

  private def cutOffLeadingZeroes() {
    def counter(pos: Int): Int = {
      if (pos >= 0 && digits(pos) == 0)
        counter(pos - 1)
      else pos
    }
    val pos = counter(digits.length - 1)
    if (pos != digits.length - 1) {
      digits = digits.dropRight(digits.length - pos - 1)
      numberLength = digits.length
    }
  }

  private def removeLeadingZeroes(s: String): String = {
    def counter(pos: Int): Int = {
      if (pos < s.length && s(pos) == '0')
        counter(pos + 1)
      else pos
    }
    val pos = counter(0)
    s.drop(pos)
  }

  def +(a: BigInt2) =
    BigInt2.add(this, a)

  def abs(): BigInt2 = {
    if (sign < 0)
      new BigInt2(1, numberLength, digits)
    else
      this
  }

  def /(divisor: BigInt2): BigInt2 = {
    if (divisor.sign == 0)
      throw new ArithmeticException("Divide by Zero")
    if (divisor.isOne)
      return (if (divisor.sign > 0) this else this.negate)
    val divisorSign = divisor.sign
    val thisSign = sign
    val thisLen = numberLength
    val divisorLen = divisor.numberLength
    if (thisLen + divisorLen == 2) {
      val value = (if (thisSign != divisorSign) -1 else 1) *(digits(0) & 0xFFFFFFFFL) / (divisor.digits(0) & 0xFFFFFFFFL)
      return BigInt2.valueOf(value)
    }
    val cmp =
      if (thisLen != divisorLen) {if (thisLen > divisorLen) 1 else -1}
      else BigInt2.compareArrays(digits, divisor.digits, thisLen)
    if (cmp == 0){
      if (thisSign == divisorSign) BigInt2.ONE
      else BigInt2.MINUS_ONE
    }
    else if (cmp == -1)
      BigInt2.ZERO
    else {
      val resLength = thisLen - divisorLen + 1
      val resDigits = new Array[Int](resLength)
      val resSign = if (thisSign == divisorSign) 1 else -1
      if (divisorLen == 1)
        BigInt2.divideArrayByInt(resDigits, digits, thisLen, divisor.digits(0))
      else
        BigInt2.divide(resDigits, resLength, digits, thisLen, divisor.digits, divisorLen)
      val res = new BigInt2(resSign, resLength, resDigits)
      res.cutOffLeadingZeroes()
      res
    }
  }

  def min(a: BigInt2): BigInt2 =
    if (this.compareTo(a) == -1) this
    else a

  def max(a: BigInt2): BigInt2 =
    if (this.compareTo(a) == 1) this
    else a

  def compareTo(a: BigInt2): Int = {
    if (sign > a.sign) 1
    else if (sign < a.sign) -1
    else if (numberLength > a.numberLength) sign
    else if (numberLength < a.numberLength) -a.sign
    else sign * BigInt2.compareArrays(digits, a.digits, numberLength)
  }

  override def intValue(): Int =
    (sign * (digits(0)))

  override def longValue(): Long = {
    val value =
      if (numberLength > 1)
        ((digits(1).toLong) << 32) | (digits(0) & 0xFFFFFFFFL)
      else
        (digits(0) & 0xFFFFFFFFL)
    return sign * value
  }

  override def floatValue(): Float =
    doubleValue.toFloat

  override def doubleValue(): Double = {
    if ((numberLength < 2) || ((numberLength == 2) && (digits(1) > 0)))
      return longValue.toDouble
    if (numberLength > 32)
      return (
        if (sign > 0) Double.PositiveInfinity
        else Double.NegativeInfinity)
    1.0
  }

  def signum() = sign

  def -(a: BigInt2) =
    BigInt2.subtract(this, a)

  def *(a: BigInt2) =
    BigInt2.multiply(this, a)

  def negate(): BigInt2 = {
    if (sign == 0)
      this
    else{
      new BigInt2(-sign, numberLength, digits)
    }
  }

  def <<(n: Int): BigInt2 = {
    if ((n == 0) || (sign == 0))
      this
    else if (n > 0)
      BigInt2.shiftLeft(this, n)
    else
      BigInt2.shiftRight(this, -n)
  }

  def >>(n: Int): BigInt2 = {
    if ((n == 0) || (sign == 0))
      this
    else if (n > 0)
      BigInt2.shiftRight(this, n)
    else
      BigInt2.shiftLeft(this, -n)
  }

  def shiftLeftOneBit(): BigInt2 =
    if (sign == 0) this
    else BigInt2.shiftLeft(this, 1)

  def equalsArrays(a: Array[Int]): Boolean = {
    var i = numberLength - 1
    while ((i >= 0) && (digits(i) == a(i)))
      i -= 1
    (i < 0)
  }

  def isOne() = ((numberLength == 1) && (digits(0) == 1))

  override def toString = BigInt2.toDecimalScaledString(this, 0)

}

object BigInt2 {
  val MINUS_ONE = new BigInt2("-1")
  val ZERO = new BigInt2("0")
  val ONE = new BigInt2("1")
  val TEN = new BigInt2("10")
  val SMALL_VALUES = Array[BigInt2](ZERO, ONE, new BigInt2(1, 2), new BigInt2(1, 3),
    new BigInt2(1, 4), new BigInt2(1, 5), new BigInt2(1, 6), new BigInt2(1, 7),
    new BigInt2(1, 8), new BigInt2(1, 9), TEN )

  def valueOf(a: Long): BigInt2 = {
    if (a < 0) {
      if (a != -1)
        new BigInt2(-1, -a.toInt)
      else
        MINUS_ONE
    }
    else if (a <= 10)
      SMALL_VALUES(a.toInt)
    else
      new BigInt2(1, a.toInt)
  }

  def toDecimalScaledString(bi: BigInt2, scale: Int): String = {
    var digits = bi.digits
    val numberLength = bi.numberLength
    var resLengthInChars = numberLength * 10 + 1 + 7
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

  def multiplyByInt(a: Array[Int], aSize: Int, factor: Int): Int =
    multiplyByInt(a, a, aSize, factor)

  def multiplyByInt(res: Array[Int], a: Array[Int], aSize: Int, factor: Int): Int = {
    @tailrec
    def compute(pos: Int, carry: Long): Int = {
      if (pos < aSize) {
        val tcarry = unsignedMultAddAdd(a(pos), factor, carry.toInt, 0)
        res(pos) = tcarry.toInt
        compute(pos + 1, tcarry >>> 32)
      }
      else carry.toInt
    }
    compute(0, 0L)
  }

  def unsignedMultAddAdd(a: Int, b: Int, c: Int, d: Int): Long =
    (a & 0xFFFFFFFFL) * (b & 0xFFFFFFFFL) + (c & 0xFFFFFFFFL) + (d & 0xFFFFFFFFL)

  def inplaceAdd(a: Array[Int], aSize: Int, addend: Int): Int = {
    var carry = addend & 0xFFFFFFFFL
    var i = 0
    /*@tailrec
    def compute(pos: Int, carry: Long): Int = {
      if (pos < aSize && carry != 0) {
        val tcarry = carry + a(pos) & 0xFFFFFFFFL
        val prev = a(pos)
        a(pos) = tcarry.toInt
        compute(pos + 1, carry >>> 32)
      }
      else carry.toInt
    }*/
    while (i < aSize && carry != 0){
      carry += a(i) & 0xFFFFFFFFL
      a(i) = carry.toInt
      carry >>>= 32
      i += 1
    }
    carry.toInt
    //compute(i, carry)
  }

  def compareArrays(a: Array[Int], b: Array[Int], size: Int): Int = {
    var i = size - 1
    while((i >= 0) && (a(i) == b(i)))
      i -= 1
    if (i < 0) 0
    else if ((a(i) & 0xFFFFFFFFL) < (b(i) & 0xFFFFFFFFL)) -1
    else 1
  }

  def add(a: BigInt2, b: BigInt2): BigInt2 = {
    val aSign = a.sign
    val bSign = b.sign
    var resSign = 0
    var resDigits = Array[Int]()
    if (aSign == 0)
      return b
    if (bSign == 0)
      return a
    val aLen = a.numberLength
    val bLen = b.numberLength
    if (aLen + bLen == 2) {
      val a1 = a.digits(0) & 0xFFFFFFFFL
      val b1 = b.digits(0) & 0xFFFFFFFFL
      if (aSign == bSign) {
        val res = a1 + b1
        val valueLo = res.toInt
        val valueHi = (res >>> 32).toInt
        return (
          if (valueHi == 0) new BigInt2(aSign, valueLo)
          else new BigInt2(aSign, 2, Array[Int](valueLo, valueHi))
        )
      }
      return BigInt2.valueOf(if (aSign < 0) b1 - a1 else a1 - b1)
    }
    else if (aSign == bSign) {
      resSign = aSign
      resDigits =
        if (aLen >= bLen) add(a.digits, aLen, b.digits, bLen)
        else add(b.digits, bLen, a.digits, aLen)
    }
    else {
      val cmp = (
        if (aLen != bLen) {
          if (aLen > bLen) 1
          else -1
        }
        else compareArrays(a.digits, b.digits, aLen))
      if (cmp == 0)
        return ZERO
      else if (cmp == 1) {
        resSign = aSign
        resDigits = subtract(a.digits, aLen, b.digits, bLen)
      }
      else {
        resSign = bSign
        //resDigits = subtract(a.digits, bLen, b.digits, aLen)
        resDigits = subtract(b.digits, bLen, a.digits, aLen)
      }
    }
    var res = new BigInt2(resSign, resDigits.length, resDigits)
    res.cutOffLeadingZeroes()
    res
  }

  def add(res: Array[Int], a: Array[Int], aSize: Int, b: Array[Int], bSize: Int) {
    var carry = (a(0) & 0xFFFFFFFFL) + (b(0) & 0xFFFFFFFFL)
    res(0) = carry.toInt
    carry >>= 32
    var i = 1
    while (i < bSize) {
      carry += (a(i) & 0xFFFFFFFFL) + (b(i) & 0xFFFFFFFFL)
      res(i) = carry.toInt
      carry >>= 32
      i += 1
    }
    while(i < aSize) {
      carry += a(i) & 0xFFFFFFFFL
      res(i) = carry.toInt
      carry >>= 32
      i += 1
    }
    if (carry != 0)
      res(i) = carry.toInt
  }

  def add(a: Array[Int], aSize: Int, b: Array[Int], bSize: Int): Array[Int] = {
    var res = new Array[Int](aSize + 1)
    if (aSize >= bSize)
      add(res, a, aSize, b, bSize)
    else
      add(res, b, bSize, a, aSize)
    return res
  }

  def subtract(a: BigInt2, b: BigInt2): BigInt2 =
    add(a, b.negate)

  def subtract(res: Array[Int], a: Array[Int], aSize: Int, b: Array[Int], bSize: Int) {
    var i = 0
    var borrow = 0L
    while (i < bSize) {
      borrow += (a(i) & 0xFFFFFFFFL) - (b(i) & 0xFFFFFFFFL)
      res(i) = borrow.toInt
      borrow >>= 32
      i += 1
    }
    while(i < aSize) {
      borrow += a(i) & 0xFFFFFFFFL
      res(i) = borrow.toInt
      borrow >>= 32
      i += 1
    }
  }

  def subtract(a: Array[Int], aSize: Int, b: Array[Int], bSize: Int): Array[Int] = {
    var res = new Array[Int](aSize)
    subtract(res, a, aSize, b, bSize)
    return res
  }

  def multiply(a: BigInt2, b: BigInt2): BigInt2 = {
    val aLen = a.numberLength
    val bLen = b.numberLength
    val resLength = aLen + bLen
    val resSign = if (a.sign != b.sign) -1 else 1
    if (resLength == 2) {
      val value = unsignedMultAddAdd(a.digits(0), b.digits(0), 0, 0)
      val valueLo = value.toInt
      val valueHi = (value >>> 32).toInt
      return (
        if (valueHi == 0) new BigInt2(resSign, valueLo)
        else new BigInt2(resSign, 2, Array(valueLo, valueHi)))
    }
    else {
      val aDigits = a.digits
      val bDigits = b.digits
      val resDigits = new Array[Int](resLength)
      multArraysPAP(aDigits, aLen, bDigits, bLen, resDigits)
      val result = new BigInt2(resSign, resLength, resDigits)
      result.cutOffLeadingZeroes()
      result
    }
  }

  def multArraysPAP(aDigits: Array[Int], aLen: Int, bDigits: Array[Int], bLen: Int, resDigits: Array[Int]) {
    if (!(aLen == 0 || bLen == 0)) {
      if (aLen == 1)
        resDigits(bLen) = multiplyByInt(resDigits, bDigits, bLen, aDigits(0))
      else if (bLen == 1)
        resDigits(aLen) = multiplyByInt(resDigits, aDigits, aLen, bDigits(0))
      else
        multPAP(aDigits, bDigits, resDigits, aLen, bLen)
    }
  }

  def multPAP(a: Array[Int], b: Array[Int], t: Array[Int], aLen: Int, bLen: Int) {
    if (a == b && aLen == bLen)
      square(a, aLen, t)
    else {
      for (i <- 0 until aLen) {
        var carry = 0L
        var aI = a(i)
        for (j <- 0 until bLen) {
          carry = unsignedMultAddAdd(aI, b(j), t(i + j), carry.toInt)
          t(i + j) = carry.toInt
          carry >>>= 32
        }
        t(i + bLen) = carry.toInt
      }
    }
  }

  def square(a: Array[Int], aLen: Int, res: Array[Int]): Array[Int] = {
    var carry = 0L
    for (i <- 0 until aLen) {
      carry = 0L
      for (j <- i+1 until aLen) {
        carry = unsignedMultAddAdd(a(i), a(j), res(i + j), carry.toInt)
        res(i + j) = carry.toInt
        carry >>>= 32
      }
      res(i + aLen) = carry.toInt
    }
    shiftLeftOneBit(res, res, aLen << 1)
    carry = 0L
    var index = 0
    for (i <- 0 until aLen) {
      carry = unsignedMultAddAdd(a(i), a(i), res(index), carry.toInt)
      res(index) = carry.toInt
      carry >>>= 32
      index += 1
      carry += res(index) & 0xFFFFFFFL
      res(index) = carry.toInt
      carry >>>= 32
      index += 1
    }
    res
  }

  def shiftLeftOneBit(result: Array[Int], source: Array[Int], srcLen: Int) {
    var carry = 0
    for (i <- 0 until srcLen) {
      val value = source(i)
      result(i) = (value << 1) | carry
      carry = value >>> 31
    }
    if (carry != 0)
      result(srcLen) = carry
  }

  def shiftRight(source: BigInt2, count1: Int): BigInt2 = {
    val intCount = count1 >> 5
    val count = count1 & 31
    if (intCount >= source.numberLength)
      return (if (source.sign < 0) MINUS_ONE else ZERO)
    var resLength = source.numberLength - intCount
    var resDigits = new Array[Int](resLength + 1)
    shiftRight(resDigits, resLength, source.digits, intCount, count)
    if (source.sign < 0) {
      var i = 0
      while (i < intCount && source.digits(i) == 0)
        i += 1
      if ((i < intCount) || ((count >0) && ((source.digits(i) << (32 - count)) != 0))) {
        i = 0
        while (i < resLength && resDigits(i) == -1) {
          i += 1
          resDigits(i) = 0
        }
        if (i == resLength)
          resLength += 1
        resDigits(i) += 1
      }
    }
    val result = new BigInt2(source.sign, resLength, resDigits)
    result.cutOffLeadingZeroes
    return result
  }

  def shiftRight(res: Array[Int], resLen: Int, source: Array[Int], intCount: Int, count: Int): Boolean = {
    @tailrec
    def compute(pos: Int): Boolean = {
      if (pos < intCount)
        if(source(pos) == 0)
          compute(pos + 1)
        else
          false
      else
        true
    }
    var allZero = compute(0)
    if (count == 0) {
      System.arraycopy(source, intCount, res, 0, resLen)
    }
    else {
      val leftShiftCount = 32 - count
      allZero &= (source(intCount) << leftShiftCount) == 0
      @tailrec
      def compute(pos: Int): Int = {
        if (pos < resLen -1){
          res(pos) = (source(pos + intCount) >>> count) | (source(pos + intCount + 1) << leftShiftCount)
          compute(pos + 1)
        }
        else pos
      }
      val i = compute(0)
      res(i) = (source(i + intCount) >>> count)
    }
    allZero
  }

  def shiftLeft(source: BigInt2, count: Int): BigInt2 = {
    val intCount = count >> 5
    val count1 = count & 31
    var resLength = source.numberLength + intCount + (if (count1 == 0) 0 else 1)
    var resDigits = new Array[Int](resLength)
    shiftLeft(resDigits, source.digits, intCount, count1)
    val result = new BigInt2(source.sign, resLength, resDigits)
    result.cutOffLeadingZeroes
    return result
  }

  def shiftLeft(res: Array[Int], source: Array[Int], intCount: Int, count: Int) {
    if (count == 0)
      System.arraycopy(source, 0, res, intCount, res.length - intCount)
    else {
      val rightShiftCount = 32 - count
      var i = res.length - 1
      res(i) == 0
      while (i > intCount){
        res(i) |= (source(i - intCount - 1) >>> rightShiftCount)
        res(i - 1) = source(i - intCount - 1) << count
        i -= 1
      }
    }
    for (i <- 0 until intCount)
      res(i) = 0
  }

  def divideArrayByInt(dest: Array[Int], src: Array[Int], srcLen: Int, divisor: Int): Int = {
    var rem = 0L
    var bLong = divisor & 0xFFFFFFFFL
    for (i <- srcLen - 1 to 0 by -1) {
      val temp = (rem << 32) | (src(i) & 0xffffffffL)
      val quot =
        if (temp >= 0) {
          rem = (temp % bLong)
          (temp / bLong)
        }
        else {
          val aPos = temp >>> 1
          val bPos = divisor >>> 1
          var quot = aPos / bPos
          rem = aPos % bPos
          rem = (rem << 1 + temp & 1)
          if ((divisor & 1) != 0) {
            if (quot <= rem)
              rem = -quot
            else if (quot - rem <= bLong) {
              rem += bLong - quot
              quot -= 1
            }
            else {
              rem += (bLong << 1) - quot
              quot -= 2
            }
          }
          quot
        }
      dest(i) = (quot & 0xFFFFFFFFL).toInt
    }
    rem.toInt
  }

  def divide(quot: Array[Int], quotLen: Int, a: Array[Int], aLen: Int, b: Array[Int], bLen: Int): Array[Int] = {
    val normA = new Array[Int](aLen + 1)
    val normB = new Array[Int](bLen + 1)
    val normBLen = bLen
    val divisorShift = Integer.numberOfLeadingZeros(b(bLen - 1))
    if (divisorShift != 0) {
      BigInt2.shiftLeft(normB, b, 0, divisorShift)
      BigInt2.shiftLeft(normA, a, 0, divisorShift)
    }
    else {
      System.arraycopy(a, 0, normA, 0, aLen)
      System.arraycopy(b, 0, normB, 0, bLen)
    }
    val firstDivisorDigit = normB(normBLen - 1)
    var i = quotLen -1
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
          @tailrec
          def compute(rOverflowed: Boolean, leftHand: Long, rightHand: Long) {
            guessDigit -= 1
            if (!rOverflowed){
              val left = (guessDigit & 0xFFFFFFFFL) * (normB(normBLen - 2) & 0xFFFFFFFFL)
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
          val borrow = multiplyAndSubtract(normA, j - normBLen, normB, normBLen, guessDigit)
          if (borrow != 0) {
            guessDigit -= 1
            var carry = 0L
            for (k <- 0 until normBLen) {
              carry += (normA(j - normBLen + k) & 0xFFFFFFFFL) + (normB(k) & 0xFFFFFFFFL)
              normA(j - normBLen + k) = carry.toInt
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
      shiftRight(normB, normBLen, normA, 0, divisorShift)
      normB
    }
    else {
      System.arraycopy(normA, 0, normB, 0, bLen)
      normA
    }
  }

  def divideLongByInt(a: Long, b: Int): Long = {
    val bLong = b & 0xFFFFFFFFL
    if (a >= 0) {
      val quot = (a / bLong)
      val rem = (a % bLong)
      (rem << 32) | (quot & 0xffffffffL)
    }
    else {
      val aPos = a >>> 1
      val bPos = b >>> 1
      var quot: Long = aPos / bPos
      var rem: Long = aPos % bPos
      rem = (rem << 1) + (a & 1)
      if ((b & 1) != 0) {
        if (quot <= rem)
          rem -= quot
        else if (quot - rem <= bLong) {
          rem += bLong - quot
          quot -= 1
        }
        else {
          rem += (bLong << 1) - quot
          quot -= 2
        }
      }
      (rem << 32) | (quot & 0xffffffffL)
    }
  }

  def multiplyAndSubtract(a: Array[Int], start: Int, b: Array[Int], bLen: Int, c: Int) = {
    @tailrec
    def compute(pos: Int, carry0: Long, carry1: Long): (Long, Long) = {
      if (pos < bLen) {
        val tcarry0 = unsignedMultAddAdd(b(pos), c, carry0.toInt, 0)
        val tcarry1 = carry1 + (a(start + pos) & 0xFFFFFFFFL) - (tcarry0 & 0xFFFFFFFFL)
        a(start + pos) = tcarry1.toInt
        compute(pos + 1, tcarry0 >>> 32, tcarry1 >> 32)
      }
      else (carry0, carry1)
    }
    val res = compute(0, 0L, 0L)
    val carry1 = (a(start + bLen) & 0xFFFFFFFFL) - res._1 + res._2
    a(start + bLen) = carry1.toInt
    (carry1 >> 32).toInt
  }

}

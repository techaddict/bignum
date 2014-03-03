package bignum

import scala.annotation.tailrec
import UtilCommon._
import UtilBigEndian._
import BigInt2._

object UtilLittleEndian {

  def shiftRight(source: BigInt2, count1: Int): BigInt2 = {
    val intCount = count1 >> 5
    val count = count1 & 31
    if (intCount >= source.digits.length)
      if (source.sign < 0) minusOne else zero
    else {
      var resLen = source.digits.length - intCount
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
      BigInt2(source.sign, removeLeadingZeroes(res.reverse))
    }
  }

  def shiftRight(res: Array[Int], resLen: Int, source: Array[Int],
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
      scala.compat.Platform.arraycopy(source, intCount, res, 0, resLen)
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

  def shiftLeft(sign: Int, digits: Array[Int], count1: Int): BigInt2 = {
    // We don't to shift one by one :)
    // First Shift in Multiples of 32, then in last
    val intCount = count1 >> 5
    // the remaining Part
    val count = count1 & 31
    // if the remaining Part is not 0 reslength + 1
    val resLength = digits.length + intCount + (if (count == 0) 0 else 1)
    val res = new Array[Int](resLength)
    // All the work is done here
    if (count == 0) {
      scala.compat.Platform.arraycopy(digits, 0, res, intCount, res.length - intCount)
      new BigInt2(sign, removeLeadingZeroes(res.reverse))
    }
    else {
      @tailrec def compute(pos: Int) {
        if (pos > intCount) {
          res(res.length - 1 - pos) |= (digits(pos - intCount - 1) >>> (32 - count))
          res(res.length - pos) = digits(pos - intCount - 1) << count
          compute(pos - 1)
        }
      }
      compute(res.length - 1)
      new BigInt2(sign, removeLeadingZeroes(res.reverse))
    }
  }

}

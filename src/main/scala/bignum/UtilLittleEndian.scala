package bignum

import scala.annotation.tailrec
import UtilCommon._
import UtilBigEndian._
import BigInt2._

object UtilLittleEndian {

  def shiftRight(source: BigInt2, count1: Int): BigInt2 = {
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

  def shiftLeft(source: BigInt2, count: Int): BigInt2 = {
    val intCount = count >> 5
    val count1 = count & 31
    val resLength = source.digits.size + intCount + (if (count1 == 0) 0 else 1)
    val res = new Array[Int](resLength)
    shiftLeft(res, source.digits, intCount, count1)
    BigInt2(source.sign, removeLeadingZeroes(res.reverse))
  }

  def shiftLeft(res: Array[Int], source: Array[Int], intCount: Int, count: Int) {
    if (count == 0)
      scala.compat.Platform.arraycopy(source, 0, res, intCount, res.size - intCount)
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

}

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
        if ((pos < intCount) || ((count >0) && ((source.digits(pos) << (32 - count)) != 0)))
          res(0) += 1
      }
      BigInt2(source.sign, removeLeadingZeroes(res.reverse))
    }
  }

  def shiftRight(res: Array[Int], resLen: Int, source: Array[Int],
    intCount: Int, count: Int) {
    if (count == 0) {
      scala.compat.Platform.arraycopy(source, intCount, res, 0, resLen)
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
    }
  }

}

package bignum

import scala.annotation.tailrec

object Trashy {

  def shiftRight1(res: Array[Int], resLen: Int, source: Array[Int],
    intCount: Int, count: Int) {
    if (count == 0) {
      scala.compat.Platform.arraycopy(source, intCount, res, 0, resLen)
    }
    else {
      val leftShiftCount = 32 - count
      for (i <- 0 until resLen - 1) {
        res(i) = ( source(i + intCount) >>> count ) |
          ( source(i + intCount + 1) << leftShiftCount )
      }
      res(resLen - 1) = ( source(resLen - 1 + intCount) >>> count )
    }
  }

  def shiftLeft1(res: Array[Int], source: Array[Int], intCount: Int, count: Int) {
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

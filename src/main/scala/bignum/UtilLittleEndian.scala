package bignum

import scala.annotation.tailrec
import UtilCommon._
import UtilBigEndian._
import BigInt2._

object UtilLittleEndian {
  /**
    * Get the element at pos Position, in the a, Little Endian
    */
  def getLittleEndianElem(a: BigInt2, pos: Int): Int = {
    if (pos < 0) 0
    else if (pos >= a.digits.length)
      if (a.signum < 0) -1 else 0
    else {
      val value = a.digits(a.digits.length - 1 - pos)
      if (a.signum > 0) value
      else if (pos <= a.firstNonZeroElem) -value
      else ~value
    }
  }

}

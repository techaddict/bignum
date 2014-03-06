package bignum

import scala.annotation.tailrec
import UtilCommon._
import UtilBigEndian._
import BigInt2._

object UtilLittleEndian {

  // Caller has to make sure Positive Only
  final def checkBit(digits: Array[Int], pos: Int): Boolean = {
    if (pos > digits.length * 32) false
    else {
      // Position in Array
      val index = (pos-1) / 32
      // Position in the Integer
      val intPos = pos % 32
      print("ind =" + index + " "+ intPos)
      println("val ="+((digits(index) & 0xFFFFFFFFL) & (1L << (intPos-1))))
      if (((digits(index) & 0xFFFFFFFFL) & (1L << (intPos-1))) != 0) true
      else false
    }
  }

}

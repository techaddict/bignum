package bignum

import scala.annotation.tailrec
import UtilCommon._
import UtilBigEndian._
import BigInt2._

object UtilLittleEndian {
  final def inplaceAdd(a: Array[Int], aSize: Int, addend: Int): Int = {
    @tailrec def compute6(pos: Int, carry: Long): Int = {
      if (pos < aSize && carry != 0) {
        val tcarry = carry + (a(pos).unsignedToLong)
        a(pos) = tcarry.toInt
        compute6(pos + 1, tcarry >>> 32)
      }
      else carry.toInt
    }
    compute6(0, addend.unsignedToLong)
  }

  final def multiplyByInt(a: Array[Int], aSize: Int, factor: Int): Int = {
    @tailrec def compute5(pos: Int, carry: Long): Int = {
      if (pos < aSize) {
        val tcarry = unsignedMultAddAdd(a(pos), factor, carry.toInt, 0)
        a(pos) = tcarry.toInt
        compute5(pos + 1, tcarry >>> 32)
      }
      else carry.toInt
    }
    compute5(0, 0L)
  }

  // Caller has to make sure Positive Only
  final def checkBit(digits: Array[Int], pos: Int): Boolean = {
    if (pos > digits.length * 32) false
    else {
      // Position in Array
      val index = (pos-1) / 32
      // Position in the Integer
      val intPos = pos % 32
      print("ind =" + index + " "+ intPos)
      println("val ="+((digits(index).unsignedToLong) & (1L << (intPos-1))))
      if (((digits(index).unsignedToLong) & (1L << (intPos-1))) != 0) true
      else false
    }
  }

}

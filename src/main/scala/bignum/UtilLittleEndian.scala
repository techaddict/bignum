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

  final def inplaceMultArrays(res: Array[Int], a: Array[Int], b: Array[Int]) {
    val aLen = a.length
    val bLen = b.length
    if (!(aLen == 0 || bLen == 0))
      if (aLen == 1) res(bLen) = multiplyByInt(res, b, bLen, a(0))
      else if (bLen == 1) res(aLen) = multiplyByInt(res, a, aLen, b(0))
      else {
        @tailrec def loop(pos: Int) {
          if (pos < a.length) {
            @tailrec def loopj(posj: Int, carry: Long): Int = {
              if (posj < b.length) {
                var tcarry = unsignedMultAddAdd(a(pos), b(posj), res(pos + posj), carry.toInt)
                res(pos + posj) = tcarry.toInt
                loopj(posj + 1, tcarry >>> 32)
              }
              else carry.toInt
            }
            res(pos + b.length) = loopj(0, 0L)
            loop(pos + 1)
          }
        }
        loop(0)
      }
  }

  final def multiplyByInt(a: Array[Int], aSize: Int, factor: Int): Int =
    multiplyByInt(a, a, aSize, factor)

  final def multiplyByInt(res: Array[Int], a: Array[Int], aSize: Int, factor: Int): Int = {
    @tailrec def compute5(pos: Int, carry: Long): Int = {
      if (pos < aSize) {
        val tcarry = unsignedMultAddAdd(a(pos), factor, carry.toInt, 0)
        res(pos) = tcarry.toInt
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

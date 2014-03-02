package bignum

import scala.annotation.tailrec
import UtilCommon._

object UtilLittleEndian {
  /** Returns the original array if unchanged. */
  final def removeLeadingZeroes(arr: Array[Int]): Array[Int] = {
    @tailrec def counter(pos: Int): Int = {
      if (pos >= 0 && arr(pos) == 0)
        counter(pos - 1)
      else pos + 1
    }
    //New size of the array
    val pos = counter(arr.size - 1)

    // If not Changed return the same
    if (pos == arr.size)
      arr
    // If all are zero only then digits = Array(0) for handling Zero
    else if (pos == 0)
      Array(0)
    else {
      val newArr = new Array[Int](pos)
      scala.compat.Platform.arraycopy(arr, 0, newArr, 0, pos)
      newArr
    }
  }

  final def compareArrays(a: Array[Int], b: Array[Int]): Int = {
    if (a.size > b.size) 1
    else if (a.size < b.size) -1
    else {
      @tailrec def rec(pos: Int): Int = {
        if (pos >= 0) {
          if (a(pos) == b(pos))
            rec(pos - 1)
          else if ((a(pos).unsignedToLong) < (b(pos).unsignedToLong)) -1
          else 1
        }
        else 0
      }
      rec(a.length - 1)
    }
  }

  /** Callers of this method have to make sure that the size of `large` is equal or greater than `small`. */
  final def arrayPlusArray(large: Array[Int], small: Array[Int]): Array[Int] = {
    //Could be optimzed, as it doesn't overflow very often
    val res = new Array[Int](large.size + 1)
    var carry = large(0).unsignedToLong + small(0).unsignedToLong
    res(0) = carry.toInt
    @tailrec def compute(pos: Int, carry: Long) {
      if (pos < small.size) {
        val tcarry = carry + large(pos).unsignedToLong + small(pos).unsignedToLong
        res(pos) = tcarry.toInt
        compute(pos + 1, tcarry >> 32)
      }
      else if (pos < large.size) {
        val tcarry = carry + large(pos).unsignedToLong
        res(pos) = tcarry.toInt
        compute(pos + 1, tcarry >> 32)
      }
      else if (carry != 0)
        res(pos) = carry.toInt
    }
    compute(1, carry >> 32)
    res
  }
  /** Callers of this method have to make sure that the size of `large` is equal or greater than `small`. */
  final def arrayMinusArray(large: Array[Int], small: Array[Int]): Array[Int] = {
    val res = new Array[Int](large.size)
    @tailrec def compute(pos: Int, borrow: Long) {
      if (pos < small.size) {
        val tborrow = borrow + large(pos).unsignedToLong - small(pos).unsignedToLong
        res(pos) = tborrow.toInt
        compute(pos + 1, tborrow >> 32)
      }
      else if (pos < large.size) {
        val tborrow = borrow + large(pos).unsignedToLong
        res(pos) = tborrow.toInt
        compute(pos + 1, tborrow >> 32)
      }
    }
    compute(0, 0L)
    res
  }

  final def inplaceMultArrays(res: Array[Int], a: Array[Int], b: Array[Int]) {
    val aLen = a.size
    val bLen = b.size
    if (!(aLen == 0 || bLen == 0))
      if (aLen == 1) res(bLen) = multiplyByInt(res, b, bLen, a(0))
      else if (bLen == 1) res(aLen) = multiplyByInt(res, a, aLen, b(0))
      else {
        @tailrec def loop(pos: Int) {
          if (pos < a.size) {
            @tailrec def loopj(posj: Int, carry: Long): Int = {
              if (posj < b.size) {
                var tcarry = unsignedMultAddAdd(a(pos), b(posj), res(pos + posj), carry.toInt)
                res(pos + posj) = tcarry.toInt
                loopj(posj + 1, tcarry >>> 32)
              }
              else carry.toInt
            }
            res(pos + b.size) = loopj(0, 0L)
            loop(pos + 1)
          }
        }
        loop(0)
      }
  }

  final def multiplyByInt(a: Array[Int], aSize: Int, factor: Int): Int =
    multiplyByInt(a, a, aSize, factor)

  final def multiplyByInt(res: Array[Int], a: Array[Int], aSize: Int, factor: Int): Int = {
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

  @inline final def unsignedMultAddAdd(a: Int, b: Int, c: Int, d: Int): Long =
    (a & 0xFFFFFFFFL) * (b & 0xFFFFFFFFL) + (c & 0xFFFFFFFFL) + (d & 0xFFFFFFFFL)

}

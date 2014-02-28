package bignum

import scala.annotation.tailrec
import UtilCommon._

object UtilLittleEndian {
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

}

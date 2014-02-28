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

}

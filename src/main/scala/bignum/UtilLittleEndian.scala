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

}

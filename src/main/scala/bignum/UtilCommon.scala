package bignum

object UtilCommon {
  implicit class RicherInt(val uint: Int) extends AnyVal {
    def unsignedToLong: Long = unsignedIntToLong(uint)
  }

  implicit class RicherLong(val long: Long) extends AnyVal {
    def toUnsignedInt: Int = longToUnsignedInt(long)
  }

  @inline
  final def unsignedIntToLong(unsignedInt: Int): Long =
    unsignedInt & UnsignedIntMask

  final def longToUnsignedInt(long: Long): Int = {
    if (long < 0L || long > UnsignedIntMask)
      throw new NumberFormatException("Value is out of range: " + long)
    long.toInt
  }

  @inline final def highBitsToInt(long: Long) = (long >>> 32).toInt

  /** While addition, subtraction and multiplication work the same on signed
    * and unsigned numbers, this mask is used to convert an signed to an unsigned value.
    */
  final val UnsignedIntMask: Long = 0xFFFFFFFFL
}

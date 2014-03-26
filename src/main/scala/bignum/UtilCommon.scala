package bignum

object UtilCommon {
  implicit class RicherLong(val long: Long) extends AnyVal {
    def toUnsignedInt: Int = longToUnsignedInt(long)
  }

  final def longToUnsignedInt(long: Long): Int = {
    if (long < 0L || long > UnsignedIntMask)
      throw new NumberFormatException("Value is out of range: " + long)
    long.toInt
  }

  @inline final def highBitsToInt(long: Long) = (long >>> 32).toInt

  /**
    * Returns the bit length of the given integer.
    */
  final def bitLengthOf(int: Int) = 32 - (java.lang.Integer numberOfLeadingZeros int)

  final val IS64BIT = "64".equals(System.getProperty("sun.arch.data.model"))

  /**
    * Estimates whether SS will be more efficient than the other methods when multiplying two numbers
    * of a given length in bits.
    * @param length the number of ints in each of the two factors
    * @return <code>true</code> if SS is more efficient, <code>false</code> if Toom-Cook is more efficient
    */
  def shouldMultiplySchoenhageStrassen(length: Int): Boolean = {
    if (IS64BIT) {
      // The following values were determined experimentally on a 64-bit JVM.
      // SS is slower than Toom-Cook below ~15,500 ints (~149,000 decimal digits)
      // and faster above ~73,200 ints (~705,000 decimal digits).
      // Between those values, it changes several times.
      if (length < 15500)
        return false
      if (length < 16384) // 2^14
        return true
      if (length < 26300)
        return false
      if (length < 32768) // 2^15
        return true
      if (length < 44000)
        return false
      if (length < 65536) // 2^16
        return true
      if (length < 73200)
        return false
      return true
    } else {
      // The following values were determined experimentally on a 32-bit JVM.
      // SS is slower than Toom-Cook below ~6,300 ints (~60,700 decimal digits)
      // and faster above ~34,000 ints (~327,500 decimal digits).
      // Between those values, it changes several times.
      if (length < 6300)
        return false
      if (length < 16384) // 2^14
        return true
      if (length < 19300)
        return false
      if (length < 32768) // 2^15
        return true
      if (length < 34000)
        return false
      return true
    }
  }

  /**
    * Estimates whether SS will be more efficient than the other methods when squaring a number
    * of a given length in bits.
    * @param bitLength the number of ints in the number to be squared
    * @return <code>true</code> if SS is more efficient, <code>false</code> if Toom-Cook is more efficient
    * @see #shouldMultiplySchoenhageStrassen(int)
    */
  def shouldSquareSchoenhageStrassen(length: Int): Boolean = {
    if (IS64BIT) {
      if (length < 15000)
        return false
      if (length < 16384) // 2^14
        return true
      if (length < 27100)
        return false
      if (length < 32768) // 2^15
        return true
      if (length < 43600)
        return false
      if (length < 65536) // 2^16
        return true
      if (length < 76300)
        return false
      if (length < 131072) // 2^17
        return true
      if (length < 133800)
        return false
      return true
    } else {
      if (length < 7100)
        return false
      if (length < 8192) // 2^13
        return true
      if (length < 14200)
        return false
      if (length < 16384) // 2^14
        return true
      if (length < 24100)
        return false
      if (length < 32768) // 2^15
        return true
      if (length < 42800)
        return false
      if (length < 65536) // 2^16
        return true
      if (length < 73000)
        return false
      return true
    }
  }

  /** While addition, subtraction and multiplication work the same on signed
    * and unsigned numbers, this mask is used to convert an signed to an unsigned value.
    */
  final val UnsignedIntMask: Long = 0xFFFFFFFFL

  /** The threshold value for using Karatsuba multiplication. If the number
    * of ints in both mag arrays are greater than this number, then
    * Karatsuba multiplication will be used. This value is found
    * experimentally to work well.
    */
  final val KaratsubaThreshold = 50

  /** The threshold value for using 3-way Toom-Cook multiplication.
    * If the number of ints in both mag arrays are greater than this number,
    * then Toom-Cook multiplication will be used. This value is found
    * experimentally to work well.
    */
  final val ToomCookThreshold = 75

  /** The threshold value for using Karatsuba squaring. If the number
    * of ints in the number are larger than this value,
    * Karatsuba squaring will be used. This value is found
    * experimentally to work well.
    */
  final val KaratsubaSquareThreshold = 90

  /** The threshold value for using Toom-Cook squaring. If the number
    * of ints in the number are larger than this value,
    * Toom-Cook squaring will be used. This value is found
    * experimentally to work well.
    */
  final val ToomCookSquareThreshold = 140

  /** The threshold value for using Burnikel-Ziegler division. If the number
    * of ints in the number are larger than this value,
    * Burnikel-Ziegler division will be used. This value is found
    * experimentally to work well.
    */
  final val BurnikelZieglerThreshold = 50

  /** The threshold value, in bits, for using Newton iteration when
    * computing the reciprocal of a number.
    */
  final val NewtonThreshold = 100

  /** The threshold value for using Schoenhage recursive base conversion. If
    * the number of ints in the number are larger than this value,
    * the Schoenhage algorithm will be used. In practice, it appears that the
    * Schoenhage routine is faster for any threshold down to 2, and is
    * relatively flat for thresholds between 2-25, so this choice may be
    * varied within this range for very small effect.
    */
  final val SchoenhageBaseConversionThreshold = 8

}

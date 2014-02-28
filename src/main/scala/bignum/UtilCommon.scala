package bignum

object UtilCommon {

  @inline final def highBitsToInt(long: Long) = (long >>> 32).toInt

}

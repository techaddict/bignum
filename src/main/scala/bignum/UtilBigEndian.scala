package bignum

import scala.annotation.tailrec
import UtilCommon._
import BigInt2._
import Trashy._

object UtilBigEndian {

  def shiftRight(a: BigInt2, count1: Int): BigInt2 = {
    val intCount = count1 >> 5
    val count = count1 & 31
    if (intCount >= a.digits.length)
      if (a.sign < 0) minusOne else zero
    else {
      var resLen = a.digits.length - intCount
      val res =
        if (count == 0) {
          val res = new Array[Int](resLen)
          scala.compat.Platform.arraycopy(a.digits, 0, res, 0, resLen)
          res
        }
        else {
          val res = new Array[Int](resLen + 1)
          val leftShiftCount = 32 - count
          var pos = res.length - 1
          while (pos > 1) {
            res(pos) = (a.digits(pos - 1) >>> count) | (a.digits(pos - 2) << leftShiftCount)
            pos -= 1
          }
          res(pos) = (a.digits(pos - 1) >>> count)
          res
        }
      if (a.sign < 0) {
        val value = resLen - 1
        var pos = a.digits.length - 1
        while (pos > value && a.digits(pos) == 0)
          pos -= 1
        if ((pos > value) || ((count >0) && ((a.digits(pos) << (32 - count)) != 0)))
          res(res.length - 1) += 1
      }
      BigInt2(a.sign, removeLeadingZeroes(res))
    }
  }

  /** Shift Left the BigInteger a by count1 bits */
  def shiftLeft(a: BigInt2, count1: Int): BigInt2 = {
    // We don't to shift one by one :)
    // First Shift in Multiples of 32, then in last
    val intCount = count1 >> 5
    // the remaining Part
    val count = count1 & 31
    // All the work is done here
    if (count == 0) {
      val res = new Array[Int](a.digits.length + intCount)
      scala.compat.Platform.arraycopy(a.digits, 0, res, 0, res.length - intCount)
      new BigInt2(a.signum, res)
    }
    else {
      val res = new Array[Int](a.digits.length + intCount + 1)
      var pos = 0
      while(pos < a.digits.length) {
        res(pos) |= a.digits(pos) >>> (32 - count)
        res(pos + 1) = a.digits(pos) << count
        pos += 1
      }
      new BigInt2(a.signum, removeLeadingZeroes(res))
    }
  }

  final def inplaceMultArrays(a: Array[Int], b: Array[Int]): Array[Int] = {
    val aLen = a.length
    val bLen = b.length
    val res = new Array[Int](aLen + bLen)
    if (aLen == 1) multiplyByInt(res, b, a(0))
    else if (bLen == 1) multiplyByInt(res, a, b(0))
    else {
      var pos = a.length - 1
      while (pos >= 0) {
        var posj = b.length - 1
        var carry = 0L
        while (posj >= 0) {
          carry += (a(pos) & 0xFFFFFFFFL) * (b(posj) & 0xFFFFFFFFL) +
            (res(pos + posj + 1) & 0xFFFFFFFFL)
          res(pos + posj + 1) = carry.toInt
          posj -= 1
          carry >>>= 32
        }
        res(pos) = carry.toInt
        pos -= 1
      }
    }
    res
  }

  final def multiplyByInt(res: Array[Int], a: Array[Int], factor: Int) {
    var pos = a.length - 1
    var carry = 0L
    while (pos >= 0) {
      carry += (a(pos) & 0xFFFFFFFFL) * (factor & 0xFFFFFFFFL)
      res(pos + 1) = carry.toInt
      pos -= 1
      carry >>>= 32
    }
    res(0) = carry.toInt
  }

  final def inplaceAdd(a: Array[Int], aSize: Int, addend: Int): Int = {
    var pos = a.length - 1
    var carry = addend & 0xFFFFFFFFL
    while (pos > a.length - 1 - aSize && carry != 0) {
      carry += a(pos) & 0xFFFFFFFFL
      a(pos) = carry.toInt
      pos -= 1
      carry >>>= 32
    }
    carry.toInt
  }

  final def inplaceMultiplyByInt(a: Array[Int], aSize: Int, factor: Int): Int = {
    var pos = a.length - 1
    var carry = 0L
    var cond = a.length - 1 - aSize
    while (pos > cond) {
      carry = (a(pos) & 0xFFFFFFFFL) * (factor & 0xFFFFFFFFL) + carry
      a(pos) = carry.toInt
      pos -= 1
      carry >>>= 32
    }
    carry.toInt
  }

  /** Returns the original array if unchanged. */
  final def removeLeadingZeroes(arr: Array[Int]): Array[Int] = {
    var i = 0
    var empty = -1
    var stop = false
    while (i < arr.length && stop == false) {
      if (arr(i) == 0)
        empty = i
      else
        stop = true
      i += 1
    }
    if (empty == -1) {
      arr
    } else {
      val newLen = arr.length - empty - 1
      val newArr = new Array[Int](newLen)
      scala.compat.Platform.arraycopy(arr, empty + 1, newArr, 0, newLen)
      newArr
    }
  }

  /** Returns a new array. */
  final def removeLeadingZeroesAndCopy(arr: Array[Int]): Array[Int] = {
    var i = 0
    var empty = -1
    var stop = false
    while (i < arr.length && stop == false) {
      if (arr(i) == 0)
        empty = i
      else
        stop = true
      i += 1
    }
    if (empty == -1) {
      arr.clone
    } else {
      val newLen = arr.length - empty - 1
      val newArr = new Array[Int](newLen)
      scala.compat.Platform.arraycopy(arr, empty + 1, newArr, 0, newLen)
      newArr
    }
  }

  final def compareArrays(an: Array[Int], bn: Array[Int]): Int = {
    val al = an.length
    val bl = bn.length
    if (al < bl)
      return -1
    if (al > bl)
      return 1

    var i = 0
    while (i < al) {
      val av = an(i)
      val bv = bn(i)
      if (av != bv)
        return if ((av & 0xFFFFFFFFL) < (bv & 0xFFFFFFFFL)) -1 else 1
      i += 1
    }

    return 0
  }

  /*final def inplaceAdd(a: Array[Int], aSize: Int, addend: Int) = {
    var carry: Long = addend & 0xFFFFFFFFL

    var i = a.length - 1
    val aStop = a.length - aSize // ???
    while ((carry != 0) && (i >= aStop)) { // ???
      carry += a(i) & 0xFFFFFFFFL
      a(i) = carry.toInt
      carry >>= 32

      i += 1
    }

    carry.toInt
  }*/

  /** Callers of this method have to make sure that the size of `large` is equal or greater than `small`. */
  final def arrayPlusArray(large: Array[Int], small: Array[Int]): Array[Int] = {
    val llen = large.length
    val slen = small.length
    val diff = llen - slen
    /* We could probably check under the condition that b < a if b[high] is "smaller"
      * than Int.MinValue (unsigned) because then b.length would be enough under all
      * circumstances for the new array.
      * Because it doesn't overflow that often, we just assume that it doesn't
      * overflow and reallocate only if it really does later.
      */
    var result = new Array[Int](llen)
    var sum = 0L

    var i = slen - 1
    while (i >= 0) {
      val idx = diff + i
      sum = (large(idx) & 0xFFFFFFFFL) + (small(i) & 0xFFFFFFFFL) + sum
      result(idx) = sum.toInt
      sum = sum >>> 32
      i -= 1
    }

    var j = diff - 1
    while (sum != 0 && j >= 0) {
      sum = (large(j) & 0xFFFFFFFFL) + sum
      result(j) = sum.toInt
      sum = sum >>> 32
      j -= 1
    }

    if (sum == 0 && j >= 0) { // sum == 0 necessary?
      scala.compat.Platform.arraycopy(large, 0, result, 0, j + 1)
      result
    } else if (sum != 0) {
      val oldResult = result
      result = new Array[Int](llen + 1)
      scala.compat.Platform.arraycopy(oldResult, 0, result, 1, oldResult.length)
      result(0) = 1
      result
    } else
      result
  }

  /** Callers of this method have to make sure that the size of `large` is equal or greater than `small`. */
  final def arrayMinusArray(large: Array[Int], small: Array[Int]): Array[Int] = {
    val llen = large.length
    val slen = small.length

    var lidx = llen
    var sidx = slen

    val result = new Array[Int](lidx)

    var diff: Long = 0
    while (sidx > 0) {
      lidx -= 1
      sidx -= 1
      diff = (large(lidx) & 0xFFFFFFFFL) - (small(sidx) & 0xFFFFFFFFL) + (diff >> 32)
      result(lidx) = diff.toInt
    }

    var borrow: Boolean = (diff >> 32 != 0)
    while (lidx > 0 && borrow) {
      lidx -= 1
      val tmp = large(lidx) - 1
      result(lidx) = tmp
      borrow = tmp == -1
    }

    while (lidx > 0) {
      lidx -= 1
      result(lidx) = large(lidx)
    }

    removeLeadingZeroes(result)
  }

/*  final def multiplyByInt(x: Array[Int], y: Int, sign: Int): BigInt2 = ???

  final def multiplyToLength(x: Array[Int], xlen: Int, y: Array[Int], ylen: Int, _z: Array[Int]) = ???
*/
  /**
    * Multiplies two BigIntegers using the Karatsuba multiplication
    * algorithm. This is a recursive divide-and-conquer algorithm which is
    * more efficient for large numbers than what is commonly called the
    * "grade-school" algorithm used in multiplyToLen. If the numbers to be
    * multiplied have length n, the "grade-school" algorithm has an
    * asymptotic complexity of O(n^2). In contrast, the Karatsuba algorithm
    * has complexity of O(n^(log2(3))), or O(n^1.585). It achieves this
    * increased performance by doing 3 multiplies instead of 4 when
    * evaluating the product. As it has some overhead, should be used when
    * both numbers are larger than a certain threshold (found
    * experimentally).
    *
    * See: http://en.wikipedia.org/wiki/Karatsuba_algorithm
    */
  final def multiplyKaratsuba(x: BigInt2, y: BigInt2): BigInt2 = {
    val xlen: Int = x.digits.length
    val ylen: Int = y.digits.length

    // The number of ints in each half of the number.
    val half: Int = (math.max(xlen, ylen) + 1) / 2

    // xl and yl are the lower halves of x and y respectively, xh and yh are the upper halves.
    val xl = x lowerInts half
    val xh = x upperInts half
    val yl = y lowerInts half
    val yh = y upperInts half

    val p1 = xh * yh
    val p2 = xl * yl

    val p3 = (xh + xl) * (yh + yl)

    // result = p1 * 2^(32*2*half) + (p3 - p1 - p2) * 2^(32*half) + p2
    val result = p1.<<(32 * half).+(p3.-(p1).-(p2)).<<(32 * half).+(p2);

    if (x.signum != y.signum)
      return -result
    else
      return result
  }

  /**
    * Multiplies two BigIntegers using a 3-way Toom-Cook multiplication
    * algorithm. This is a recursive divide-and-conquer algorithm which is
    * more efficient for large numbers than what is commonly called the
    * "grade-school" algorithm used in multiplyToLen. If the numbers to be
    * multiplied have length n, the "grade-school" algorithm has an
    * asymptotic complexity of O(n^2). In contrast, 3-way Toom-Cook has a
    * complexity of about O(n^1.465). It achieves this increased asymptotic
    * performance by breaking each number into three parts and by doing 5
    * multiplies instead of 9 when evaluating the product. Due to overhead
    * (additions, shifts, and one division) in the Toom-Cook algorithm, it
    * should only be used when both numbers are larger than a certain
    * threshold (found experimentally). This threshold is generally larger
    * than that for Karatsuba multiplication, so this algorithm is generally
    * only used when numbers become significantly larger.
    *
    * The algorithm used is the "optimal" 3-way Toom-Cook algorithm outlined
    * by Marco Bodrato.
    *
    * See: http://bodrato.it/toom-cook/
    * http://bodrato.it/papers/#WAIFI2007
    *
    * "Towards Optimal Toom-Cook Multiplication for Univariate and
    * Multivariate Polynomials in Characteristic 2 and 0." by Marco BODRATO;
    * In C.Carlet and B.Sunar, Eds., "WAIFI'07 proceedings", p. 116-133,
    * LNCS #4547. Springer, Madrid, Spain, June 21-22, 2007.
    *
    */
  final def multiplyToomCook3(a: BigInt2, b: BigInt2): BigInt2 = {
    val alen = a.digits.length
    val blen = b.digits.length

    val largest: Int = math.max(alen, blen)

    // k is the size (in ints) of the lower-order slices.
    val k: Int = (largest + 2) / 3; // Equal to ceil(largest/3)

    // r is the size (in ints) of the highest-order slice.
    val r: Int = largest - 2 * k;

    // Obtain slices of the numbers. a2 and b2 are the most significant
    // bits of the numbers a and b, and a0 and b0 the least significant.
    val a2 = a.toomSlice(k, r, 0, largest)
    val a1 = a.toomSlice(k, r, 1, largest)
    val a0 = a.toomSlice(k, r, 2, largest)
    val b2 = b.toomSlice(k, r, 0, largest)
    val b1 = b.toomSlice(k, r, 1, largest)
    val b0 = b.toomSlice(k, r, 2, largest)

    val v0 = a0 * b0
    var da1 = a2 + a0
    var db1 = b2 + b0
    val vm1 = (da1 - a1) * (db1 - b1)
    da1 += a1
    db1 += b1
    val v1 = da1 * db1
    val v2 = da1.+(a2).<<(1).-(a0).*(db1.+(b2).<<(1).-(b0))
    val vinf = a2 * b2

    /* The algorithm requires two divisions by 2 and one by 3.
     * All divisions are known to be exact, that is, they do not produce
     * remainders, and all results are positive. The divisions by 2 are
     * implemented as right shifts which are relatively efficient, leaving
     * only an exact division by 3, which is done by a specialized
     * linear-time algorithm. */
    var t2 = (v2 - vm1).exactDivideBy3
    var tm1 = (v1 - vm1) >> 1
    var t1 = v1 - v0
    t2 = (t2 - t1) >> 1
    t1 = t1 - tm1 - vinf
    t2 -= (vinf << 1)
    tm1 -= t2

    // Number of bits to shift left.
    val ss: Int = k * 32;

    val result: BigInt2 = vinf.<<(ss).+(t2).<<(ss).+(t1).<<(ss).+(tm1).<<(ss).+(v0)

    if (a.signum != b.signum)
      -result
    else
      result
  }

  /** Multiplies two {@link BigInteger}s using the
    * <a href="http://en.wikipedia.org/wiki/Sch%C3%B6nhage%E2%80%93Strassen_algorithm">
    * Schoenhage-Strassen algorithm</a> algorithm.
    * @param a
    * @param b
    * @return a <code>BigInteger</code> equal to <code>a.multiply(b)</code>
    */
  final def multiplySchoenhageStrassen(_a: BigInt2, _b: BigInt2): BigInt2 = {
    var a = _a
    var b = _b
    val signum: Int = a.signum * b.signum
    if (a.signum < 0) a = -a
    if (b.signum < 0) b = -b
    val cArr: Array[Int] = multiplySchoenhageStrassen(a.digits, b.digits)
    var c: BigInt2 = new BigInt2(1, cArr)
    if (signum < 0) c = -c
    return c
  }

  /** This is the core Schoenhage-Strassen method. It multiplies two <b>positive</b> numbers of length
    * <code>aBitLen</code> and </code>bBitLen</code> that are represented as int arrays, i.e. in base
    * 2<sup>32</sup>.
    * Positive means an int is always interpreted as an unsigned number, regardless of the sign bit.<br/>
    * The arrays must be ordered most significant to least significant, so the most significant digit
    * must be at index 0.<br/>
    * If <code>a==b</code>, the DFT for b is omitted which saves roughly 1/4 of the execution time.
    * <p/>
    * The Schoenhage-Strassen algorithm works as follows:
    * <ol>
    * <li>Given numbers a and b, split both numbers into pieces of length 2<sup>n-1</sup> bits.
    * See the code for how n is calculated.</li>
    * <li>Take the low n+2 bits of each piece of a, zero-pad them to 3n+5 bits,
    * and concatenate them to a new number u.</li>
    * <li>Do the same for b to obtain v.</li>
    * <li>Calculate all pieces of gamma by multiplying u and v (using Schoenhage-Strassen or another
    * algorithm).</li>
    * <li>Split gamma into pieces of 3n+5 bits.</li>
    * <li>Calculate z'<sub>i</sub> = gamma<sub>i</sub> + gamma<sub>i+2*2<sup>n</sup></sub> -
    * gamma<sub>i+2<sup>n</sup></sub> - gamma<sub>i+3*2<sup>n</sup></sub> and reduce modulo
    * 2<sup>n+2</sup>.<br/>
    * z'<sub>i</sub> will be the i-th piece of a*b mod 2<sup>n+2</sup>.</li>
    * <li>Pad the pieces of a and b from step 1 to 2<sup>n</sup>+1 bits.</li>
    * <li>Perform a
    * <a href="http://en.wikipedia.org/wiki/Discrete_Fourier_transform_%28general%29#Number-theoretic_transform">
    * Discrete Fourier Transform</a> (DFT) on the padded pieces.</li>
    * <li>Calculate all pieces of z" by multiplying the i-th piece of a by the i-th piece of b.</li>
    * <li>Perform an Inverse Discrete Fourier Transform (IDFT) on z". z" will contain all pieces of
    * a*b mod F<sub>n</sub> where F<sub>n</sub>=2<sup>2<sup>n</sup></sup>+1.</li>
    * <li>Calculate all pieces of z such that each piece is congruent to z' modulo n+2 and congruent to
    * z" modulo F<sub>n</sub>. This is done using the
    * <a href="http://en.wikipedia.org/wiki/Chinese_remainder_theorem">Chinese remainder theorem</a>.</li>
    * <li>Calculate c by adding z<sub>i</sub> * 2<sup>i*2<sup>n-1</sup></sup> for all i, where z<sub>i</sub> is the
    * i-th piece of z.</li>
    * <li>Return c reduced modulo 2<sup>2<sup>m</sup></sup>+1. See the code for how m is calculated.</li>
    * </ol>
    *
    * References:
    * <ol>
    * <li><a href="http://en.wikipedia.org/wiki/Sch%C3%B6nhage%E2%80%93Strassen_algorithm">
    * Wikipedia article</a>
    * <li><a href="http://www.scribd.com/doc/68857222/Schnelle-Multiplikation-gro%C3%9Fer-Zahlen">
    * Arnold Schoenhage und Volker Strassen: Schnelle Multiplikation grosser Zahlen, Computing 7, 1971,
    * Springer-Verlag, S. 281-292</a></li>
    * <li><a href="http://malte-leip.net/beschreibung_ssa.pdf">Eine verstaendliche Beschreibung des
    * Schoenhage-Strassen-Algorithmus</a></li>
    * </ol>
    * @param a
    * @param b
    * @return a*b
    */
  final def multiplySchoenhageStrassen(a: Array[Int], b: Array[Int]): Array[Int] = {
    val square: Boolean = a eq b
    val M: Int = Math.max(a.length * 32, b.length * 32)
    val m: Int = 32 - Integer.numberOfLeadingZeros(2 * M - 1 - 1)
    val n: Int = m / 2 + 1
    val even: Boolean = m % 2 == 0
    val numPieces: Int = if (even) 1 << n else 1 << (n + 1)
    val pieceSize: Int = 1 << (n - 1 - 5)
    val numPiecesA: Int = (a.length + pieceSize) / pieceSize
    val u: Array[Int] = new Array[Int]((numPiecesA * (3 * n + 5) + 31) / 32)
    var uBitLength: Int = 0

    {
      var i: Int = 0
      while (i < numPiecesA && i * pieceSize < a.length) {
        {
          appendBits(u, uBitLength, a, i * pieceSize, n + 2)
          uBitLength += 3 * n + 5
        }
        ({
          i += 1; i - 1
        })
      }
    }
    var gamma: Array[Int] = null
    if (square) gamma = new BigInt2(1, u).square.digits
    else {
      val numPiecesB: Int = (b.length + pieceSize) / pieceSize
      val v: Array[Int] = new Array[Int]((numPiecesB * (3 * n + 5) + 31) / 32)
      var vBitLength: Int = 0

      {
        var i: Int = 0
        while (i < numPiecesB && i * pieceSize < b.length) {
          {
            appendBits(v, vBitLength, b, i * pieceSize, n + 2)
            vBitLength += 3 * n + 5
          }
          ({
            i += 1; i - 1
          })
        }
      }
      gamma = (new BigInt2(1, u) * new BigInt2(1, v)).digits
    }
    val gammai: Array[Array[Int]] = splitBits(gamma, 3 * n + 5)
    val halfNumPcs: Int = numPieces / 2
    val zi: Array[Array[Int]] = new Array[Array[Int]](gammai.length)

    {
      var i: Int = 0
      while (i < gammai.length) {
        zi(i) = gammai(i)
        ({
          i += 1; i - 1
        })
      }
    }
    {
      var i: Int = 0
      while (i < gammai.length - halfNumPcs) {
        subModPow2(zi(i), gammai(i + halfNumPcs), n + 2)
        ({
          i += 1; i - 1
        })
      }
    }
    {
      var i: Int = 0
      while (i < gammai.length - 2 * halfNumPcs) {
        addModPow2(zi(i), gammai(i + 2 * halfNumPcs), n + 2)
        ({
          i += 1; i - 1
        })
      }
    }
    {
      var i: Int = 0
      while (i < gammai.length - 3 * halfNumPcs) {
        subModPow2(zi(i), gammai(i + 3 * halfNumPcs), n + 2)
        ({
          i += 1; i - 1
        })
      }
    }
    val ai: Array[Array[Int]] = splitInts(a, halfNumPcs, pieceSize, (1 << (n - 5)) + 1)
    var bi: Array[Array[Int]] = null
    if (!square) bi = splitInts(b, halfNumPcs, pieceSize, (1 << (n - 5)) + 1)
    val omega: Int = if (even) 4 else 2
    val c: Array[Array[Int]] = new Array[Array[Int]](halfNumPcs)
    if (square) {
      dft(ai, omega)
      modFn(ai)

      {
        var i: Int = 0
        while (i < c.length) {
          c(i) = squareModFn(ai(i))
          ({
            i += 1; i - 1
          })
        }
      }
    }
    else {
      dft(ai, omega)
      dft(bi, omega)
      modFn(ai)
      modFn(bi)

      {
        var i: Int = 0
        while (i < c.length) {
          c(i) = multModFn(ai(i), bi(i))
          ({
            i += 1; i - 1
          })
        }
      }
    }
    idft(c, omega)
    modFn(c)
    val z: Array[Int] = new Array[Int]((1 << (m - 5)) + 1)

    {
      var i: Int = 0
      while (i < halfNumPcs) {
        {
          val eta: Array[Int] = if (i >= zi.length) new Array[Int]((n + 2 + 31) / 32) else zi(i)
          subModPow2(eta, c(i), n + 2)
          val shift: Int = i * (1 << (n - 1 - 5))
          addShifted(z, c(i), shift)
          addShifted(z, eta, shift)
          addShifted(z, eta, shift + (1 << (n - 5)))
        }
        ({
          i += 1; i - 1
        })
      }
    }
    modFn(z)
    return z
  }

  /**
   * Estimates whether SS will be more efficient than the other methods when multiplying two numbers
   * of a given length in bits.
   * @param length the number of ints in each of the two factors
   * @return <code>true</code> if SS is more efficient, <code>false</code> if Toom-Cook is more efficient
   */
  final def shouldUseSchoenhageStrassen(length: Int): Boolean = {
    if (IS64BIT) {
      if (length <= 3952) return false
      if (length <= 4096) return true
      if (length <= 6256) return false
      if (length <= 8192) return true
      if (length <= 10832) return false
      if (length <= 16384) return true
      if (length <= 17904) return false
      return true
    }
    else {
      if (length <= 2000) return false
      if (length <= 2048) return true
      if (length <= 3216) return false
      if (length <= 4096) return true
      if (length <= 5392) return false
      if (length <= 8192) return true
      if (length <= 9232) return false
      return true
    }
  }

  /**
   * Performs a modified
   * <a href="http://en.wikipedia.org/wiki/Discrete_Fourier_transform_%28general%29#Number-theoretic_transform">
   * Fermat Number Transform</a> on an array whose elements are <code>int</code> arrays.<br/>
   * The modification is that the first step is omitted because only the upper half of the result is needed.<br/>
   * This implementation uses <a href="http://www.nas.nasa.gov/assets/pdf/techreports/1989/rnr-89-004.pdf">
   * Bailey's 4-step algorithm</a>.<br/>
   * <code>A</code> is assumed to be the lower half of the full array and the upper half is assumed to be all zeros.
   * The number of subarrays in <code>A</code> must be 2<sup>n</sup> if m is even and 2<sup>n+1</sup> if m is odd.<br/>
   * Each subarray must be ceil(2<sup>n-1</sup>) bits in length.<br/>
   * n must be equal to m/2-1.
   * @param A
   * @param omega 2 or 4
   */
  final def dft(A: Array[Array[Int]], omega: Int) {
    val rows: Int = 1 << ((31 - Integer.numberOfLeadingZeros(A.length)) / 2)
    val cols: Int = A.length / rows

    {
      var i: Int = 0
      while (i < cols) {
        dftBailey1(A, omega, rows, cols, i)
        ({
          i += 1; i - 1
        })
      }
    }
    applyDftWeights(A, omega, rows, cols)

    {
      var i: Int = 0
      while (i < rows) {
        dftBailey2(A, omega, rows, cols, i)
        ({
          i += 1; i - 1
        })
      }
    }
  }

  /**
   * Performs an DFT on column {@code colIdx}.<br/>
   * <code>A</code> is assumed to be the lower half of the full array.
   * @param A an array of length rows*cols
   * @param omega root of unity
   * @param rows number of rows in A
   * @param cols number of columns in A
   * @param colIdx index of the column to transform
   */
  final def dftBailey1(A: Array[Array[Int]], omega: Int, rows: Int, cols: Int, colIdx: Int) {
    dftDirect(A, omega, rows, cols, rows, colIdx, cols)
  }

  /**
   * Performs an DFT on row {@code rowIdx}.<br/>
   * <code>A</code> is assumed to be the lower half of the full array.
   * @param A an array of length rows*cols
   * @param omega root of unity
   * @param rows number of rows in A
   * @param cols number of columns in A
   * @param rowIdx index of the row to transform
   */
  final def dftBailey2(A: Array[Array[Int]], omega: Int, rows: Int, cols: Int, rowIdx: Int) {
    dftDirect(A, omega, 0, rows, cols, rowIdx * cols, 1)
  }

  /** This implementation uses the radix-4 technique which combines two levels of butterflies. */
  final def dftDirect(A: Array[Array[Int]], omega: Int, expOffset: Int, expScale: Int, len: Int, idxOffset: Int, stride: Int) {
    val n: Int = 31 - Integer.numberOfLeadingZeros(2 * len)
    var v: Int = 1
    val intLen: Int = A(0).length
    val d: Array[Int] = new Array[Int](intLen)
    var slen: Int = len / 2
    while (slen > 1) {
      {
        var j: Int = 0
        while (j < len) {
          {
            val x1: Int = getDftExponent(n, v + 1, j + expOffset, omega) * expScale
            val x2: Int = getDftExponent(n, v, j + expOffset, omega) * expScale
            val x3: Int = getDftExponent(n, v + 1, j + slen + expOffset, omega) * expScale
            var idx0: Int = stride * j + idxOffset
            var idx1: Int = stride * j + stride * slen / 2 + idxOffset
            var idx2: Int = idx0 + stride * slen
            var idx3: Int = idx1 + stride * slen

            {
              var k: Int = slen - 1
              while (k >= 0) {
                {
                  shiftLeftModFn(A(idx2), x2, d)
                  System.arraycopy(A(idx0), 0, A(idx2), 0, intLen)
                  addModFn(A(idx0), d)
                  subModFn(A(idx2), d)
                  shiftLeftModFn(A(idx3), x2, d)
                  System.arraycopy(A(idx1), 0, A(idx3), 0, intLen)
                  addModFn(A(idx1), d)
                  subModFn(A(idx3), d)
                  shiftLeftModFn(A(idx1), x1, d)
                  System.arraycopy(A(idx0), 0, A(idx1), 0, intLen)
                  addModFn(A(idx0), d)
                  subModFn(A(idx1), d)
                  shiftLeftModFn(A(idx3), x3, d)
                  System.arraycopy(A(idx2), 0, A(idx3), 0, intLen)
                  addModFn(A(idx2), d)
                  subModFn(A(idx3), d)
                  idx0 += stride
                  idx1 += stride
                  idx2 += stride
                  idx3 += stride
                }
                k -= 2
              }
            }
          }
          j += 2 * slen
        }
      }
      v += 2
      slen /= 4
    }
    if (slen > 0) {
      var j: Int = 0
      while (j < len) {
        {
          val x: Int = getDftExponent(n, v, j + expOffset, omega) * expScale
          var idx: Int = stride * j + idxOffset
          var idx2: Int = idx + stride * slen

          {
            var k: Int = slen - 1
            while (k >= 0) {
              {
                shiftLeftModFn(A(idx2), x, d)
                System.arraycopy(A(idx), 0, A(idx2), 0, intLen)
                addModFn(A(idx), d)
                subModFn(A(idx2), d)
                idx += stride
                idx2 += stride
              }
              ({
                k -= 1; k + 1
              })
            }
          }
        }
        j += 2 * slen
      }
    }
  }

  /**
   * Returns the power to which to raise omega in a DFT.<br/>
   * When <code>omega</code>=4, this method doubles the exponent so
   * <code>omega</code> can be assumed always to be 2 in {@link #dft(int[][], int)}.
   * @param n the log of the DFT length
   * @param v butterfly depth
   * @param idx index of the array element to be computed
   * @param omega 2 or 4
   * @return
   */
  final def getDftExponent(n: Int, v: Int, idx: Int, omega: Int): Int = {
    var x: Int = Integer.reverse(idx >>> (n - v)) >>> (32 - v)
    x <<= n - v - 1
    if (omega == 4) x *= 2
    return x
  }

  /** Multiplies vector elements by powers of omega (aka twiddle factors) */
  final def applyDftWeights(A: Array[Array[Int]], omega: Int, rows: Int, cols: Int) {
    val v: Int = 31 - Integer.numberOfLeadingZeros(rows) + 1

    {
      var i: Int = 0
      while (i < rows) {
        {
          var j: Int = 0
          while (j < cols) {
            {
              val idx: Int = i * cols + j
              val temp: Array[Int] = new Array[Int](A(idx).length)
              var shiftAmt: Int = getBaileyShiftAmount(i, j, rows, v)
              if (omega == 4) shiftAmt *= 2
              shiftLeftModFn(A(idx), shiftAmt, temp)
              System.arraycopy(temp, 0, A(idx), 0, temp.length)
            }
            ({
              j += 1; j - 1
            })
          }
        }
        ({
          i += 1; i - 1
        })
      }
    }
  }

  final def getBaileyShiftAmount(i: Int, j: Int, rows: Int, v: Int): Int = {
    val iRev: Int = Integer.reverse(i + rows) >>> (32 - v)
    return iRev * j
  }

  /**
   * Performs a modified
   * <a href="http://en.wikipedia.org/wiki/Discrete_Fourier_transform_%28general%29#Number-theoretic_transform">
   * Inverse Fermat Number Transform</a> on an array whose elements are <code>int</code> arrays.
   * The modification is that the last step (the one where the upper half is subtracted from the lower half)
   * is omitted.<br/>
   * This implementation uses <a href="http://www.nas.nasa.gov/assets/pdf/techreports/1989/rnr-89-004.pdf">
   * Bailey's 4-step algorithm</a>.<br/>
   * <code>A</code> is assumed to be the upper half of the full array and the lower half is assumed to be all zeros.
   * The number of subarrays in <code>A</code> must be 2<sup>n</sup> if m is even and 2<sup>n+1</sup> if m is odd.<br/>
   * Each subarray must be ceil(2<sup>n-1</sup>) bits in length.<br/>
   * n must be equal to m/2-1.
   * @param A
   * @param omega 2 or 4
   */
  final def idft(A: Array[Array[Int]], omega: Int) {
    val rows: Int = 1 << ((31 - Integer.numberOfLeadingZeros(A.length)) / 2)
    val cols: Int = A.length / rows

    {
      var i: Int = 0
      while (i < rows) {
        idftBailey2(A, omega, rows, cols, i)
        ({
          i += 1; i - 1
        })
      }
    }
    applyIdftWeights(A, omega, rows, cols)

    {
      var i: Int = 0
      while (i < cols) {
        idftBailey1(A, omega, rows, cols, i)
        ({
          i += 1; i - 1
        })
      }
    }
  }

  /**
   * Performs an IDFT on column {@code colIdx}.<br/>
   * <code>A</code> is assumed to be the upper half of the full array.
   * @param A an array of length rows*cols
   * @param omega root of unity
   * @param rows number of rows in A
   * @param cols number of columns in A
   * @param colIdx index of the column to transform
   */
  final def idftBailey1(A: Array[Array[Int]], omega: Int, rows: Int, cols: Int, colIdx: Int) {
    idftDirect(A, omega, rows, rows, cols, colIdx, cols)
  }

  /**
   * Performs an IDFT on row {@code rowIdx}.<br/>
   * <code>A</code> is assumed to be the upper half of the full array.
   * @param A an array of length rows*cols
   * @param omega root of unity
   * @param rows number of rows in A
   * @param cols number of columns in A
   * @param rowIdx index of the row to transform
   */
  def idftBailey2(A: Array[Array[Int]], omega: Int, rows: Int, cols: Int, rowIdx: Int) {
    idftDirect(A, omega, cols, 0, rows, rowIdx * cols, 1)
  }

  /** This implementation uses the radix-4 technique which combines two levels of butterflies. */
  final def idftDirect(A: Array[Array[Int]], omega: Int, len: Int, expOffset: Int, expScale: Int, idxOffset: Int, stride: Int) {
    val n: Int = 31 - Integer.numberOfLeadingZeros(2 * len)
    var v: Int = 31 - Integer.numberOfLeadingZeros(len)
    val intLen: Int = A(0).length
    val c: Array[Int] = new Array[Int](intLen)
    var slen: Int = 1
    while (slen <= len / 4) {
      {
        var j: Int = 0
        while (j < len) {
          {
            val x1: Int = getDftExponent(n, v, j + expOffset, omega) * expScale + 1
            val x2: Int = getDftExponent(n, v - 1, j + expOffset, omega) * expScale + 1
            val x3: Int = getDftExponent(n, v, j + slen * 2 + expOffset, omega) * expScale + 1
            var idx0: Int = stride * j + idxOffset
            var idx1: Int = stride * j + stride * slen + idxOffset
            var idx2: Int = idx0 + stride * slen * 2
            var idx3: Int = idx1 + stride * slen * 2

            {
              var k: Int = slen - 1
              while (k >= 0) {
                {
                  System.arraycopy(A(idx0), 0, c, 0, intLen)
                  addModFn(A(idx0), A(idx1))
                  shiftRightModFn(A(idx0), 1, A(idx0))
                  subModFn(c, A(idx1))
                  shiftRightModFn(c, x1, A(idx1))
                  System.arraycopy(A(idx2), 0, c, 0, intLen)
                  addModFn(A(idx2), A(idx3))
                  shiftRightModFn(A(idx2), 1, A(idx2))
                  subModFn(c, A(idx3))
                  shiftRightModFn(c, x3, A(idx3))
                  System.arraycopy(A(idx0), 0, c, 0, intLen)
                  addModFn(A(idx0), A(idx2))
                  shiftRightModFn(A(idx0), 1, A(idx0))
                  subModFn(c, A(idx2))
                  shiftRightModFn(c, x2, A(idx2))
                  System.arraycopy(A(idx1), 0, c, 0, intLen)
                  addModFn(A(idx1), A(idx3))
                  shiftRightModFn(A(idx1), 1, A(idx1))
                  subModFn(c, A(idx3))
                  shiftRightModFn(c, x2, A(idx3))
                  idx0 += stride
                  idx1 += stride
                  idx2 += stride
                  idx3 += stride
                }
                ({
                  k -= 1; k + 1
                })
              }
            }
          }
          j += 4 * slen
        }
      }
      v -= 2
      slen *= 4
    }
    if (slen <= len / 2) {
      var j: Int = 0
      while (j < len) {
        {
          val x: Int = getDftExponent(n, v, j + expOffset, omega) * expScale + 1
          var idx: Int = stride * j + idxOffset
          var idx2: Int = idx + stride * slen

          {
            var k: Int = slen - 1
            while (k >= 0) {
              {
                System.arraycopy(A(idx), 0, c, 0, intLen)
                addModFn(A(idx), A(idx2))
                shiftRightModFn(A(idx), 1, A(idx))
                subModFn(c, A(idx2))
                shiftRightModFn(c, x, A(idx2))
                idx += stride
                idx2 += stride
              }
              ({
                k -= 1; k + 1
              })
            }
          }
        }
        j += 2 * slen
      }
    }
  }

  /** Divides vector elements by powers of omega (aka twiddle factors) */
  final def applyIdftWeights(A: Array[Array[Int]], omega: Int, rows: Int, cols: Int) {
    val v: Int = 31 - Integer.numberOfLeadingZeros(rows) + 1

    {
      var i: Int = 0
      while (i < rows) {
        {
          var j: Int = 0
          while (j < cols) {
            {
              val idx: Int = i * cols + j
              val temp: Array[Int] = new Array[Int](A(idx).length)
              var shiftAmt: Int = getBaileyShiftAmount(i, j, rows, v)
              if (omega == 4) shiftAmt *= 2
              shiftRightModFn(A(idx), shiftAmt, temp)
              System.arraycopy(temp, 0, A(idx), 0, temp.length)
            }
            ({
              j += 1; j - 1
            })
          }
        }
        ({
          i += 1; i - 1
        })
      }
    }
  }

  /**
   * Adds two <b>positive</b> numbers (meaning they are interpreted as unsigned) modulo 2<sup>2<sup>n</sup></sup>+1,
   * where n is <code>a.length*32/2</code>; in other words, n is half the number of bits in
   * <code>a</code>.<br/>
   * Both input values are given as <code>int</code> arrays; they must be the same length.
   * The result is returned in the first argument.
   * @param a a number in base 2<sup>32</sup> starting with the highest digit; the array's length must be 2^n+1 for some n
   * @param b a number in base 2<sup>32</sup> starting with the highest digit; must be the same length as a
   */
  final def addModFn(a: Array[Int], b: Array[Int]) {
    var carry: Boolean = false

    {
      var i: Int = a.length - 1
      while (i >= 0) {
        {
          var sum: Int = a(i) + b(i)
          if (carry) ({
            sum += 1; sum - 1
          })
          carry = ((sum >>> 31) < (a(i) >>> 31) + (b(i) >>> 31))
          a(i) = sum
        }
        ({
          i -= 1; i + 1
        })
      }
    }
    var i: Int = a.length - 1
    while (carry && i >= 0) {
      val sum: Int = a(i) + 1
      a(i) = sum
      carry = sum == 0
      i -= 1
    }
    modFn(a)
  }

  /**
   * Subtracts two <b>positive</b> numbers (meaning they are interpreted as unsigned) modulo 2<sup>2<sup>n</sup></sup>+1,
   * where n is <code>a.length*32/2</code>; in other words, n is half the number of bits in
   * <code>a</code>.<br/>
   * Both input values are given as <code>int</code> arrays; they must be the same length.
   * The result is returned in the first argument.
   * @param a a number in base 2<sup>32</sup> starting with the highest digit; the array's length must be 2^n+1 for some n
   * @param b a number in base 2<sup>32</sup> starting with the highest digit; must be the same length as a
   */
  final def subModFn(a: Array[Int], b: Array[Int]) {
    var borrow: Boolean = false

    {
      var i: Int = a.length - 1
      while (i >= 0) {
        {
          var diff: Int = a(i) - b(i)
          if (borrow) ({
            diff -= 1; diff + 1
          })
          borrow = ((diff >>> 31) > (a(i) >>> 31) - (b(i) >>> 31))
          a(i) = diff
        }
        ({
          i -= 1; i + 1
        })
      }
    }
    if (borrow) {
      a(0) += 1
      var i: Int = a.length - 1
      var carry: Boolean = true
      while (carry && i >= 0) {
        val sum: Int = a(i) + 1
        a(i) = sum
        carry = sum == 0
        i -= 1
      }
    }
  }

  /**
   * Multiplies two <b>positive</b> numbers (meaning they are interpreted as unsigned) modulo 2<sup>n</sup>+1,
   * and returns the result in a new array.<br/>
   * <code>a</code> and <code>b</code> are assumed to be reduced mod 2<sup>n</sup>+1, i.e. 0&le;a&lt;2<sup>n</sup>+1
   * and 0&le;b&lt;2<sup>n</sup>+1, where n is <code>a.length*32/2</code>; in other words, n is half the number
   * of bits in <code>a</code>.<br/>
   * Both input values are given as <code>int</code> arrays; they must be the same length.
   * @param a a number in base 2<sup>32</sup> starting with the highest digit; the array's length must be 2^n+1 for some n
   * @param b a number in base 2<sup>32</sup> starting with the highest digit; must be the same length as a
   */
  final def multModFn(a: Array[Int], b: Array[Int]): Array[Int] = {
    if (a(0) == 1 && b(0) == 1) {
      val c: Array[Int] = new Array[Int](a.length)
      c(c.length - 1) = 1
      return c
    }
    else {
      val aBigInt: BigInt2 = new BigInt2(1, a)
      val bBigInt: BigInt2 = new BigInt2(1, b)
      val c: Array[Int] = (aBigInt * bBigInt).digits
      val cpad: Array[Int] = new Array[Int](a.length - 1 + b.length - 1)
      System.arraycopy(c, 0, cpad, cpad.length - c.length, c.length)
      modFnLong(cpad)
      return java.util.Arrays.copyOfRange(cpad, cpad.length / 2 - 1, cpad.length)
    }
  }

  /** @see #multModFn(int[], int[]) */
  final def squareModFn(a: Array[Int]): Array[Int] = {
    if (a(0) == 1) {
      val c: Array[Int] = new Array[Int](a.length)
      c(c.length - 1) = 1
      return c
    }
    else {
      val aBigInt: BigInt2 = new BigInt2(1, a)
      val c: Array[Int] = aBigInt.square.digits
      val cpad: Array[Int] = new Array[Int](2 * a.length - 2)
      System.arraycopy(c, 0, cpad, cpad.length - c.length, c.length)
      modFnLong(cpad)
      return java.util.Arrays.copyOfRange(cpad, cpad.length / 2 - 1, cpad.length)
    }
  }

  /** Like {@link #modFn(int[])} but expects the argument to be 2^(n+1) bits long. */
  final def modFnLong(a: Array[Int]) {
    val len: Int = a.length
    var carry: Boolean = false

    {
      var i: Int = len - 1
      while (i >= len / 2) {
        {
          val bi: Int = a(i - len / 2)
          var diff: Int = a(i) - bi
          if (carry) ({
            diff -= 1; diff + 1
          })
          carry = ((diff >>> 31) > (a(i) >>> 31) - (bi >>> 31))
          a(i) = diff
        }
        ({
          i -= 1; i + 1
        })
      }
    }
    {
      var i: Int = len / 2 - 1
      while (i >= 0) {
        a(i) = 0
        ({
          i -= 1; i + 1
        })
      }
    }
    if (carry) {
      var j: Int = len - 1
      do {
        val sum: Int = a(j) + 1
        a(j) = sum
        carry = sum == 0
        j -= 1
        //if (j <= 0) break //TODO: break is not supported
      } while (carry && j <= 0)
    }
  }

  /**
   * Reduces a number modulo F<sub>n</sub>. The value of n is determined from the array's length.
   * @param a a number in base 2<sup>32</sup> starting with the highest digit; the array's length must be 2^n+1 for some n
   */
  final def modFn(a: Array[Int]) {
    val len: Int = a.length
    val bi: Int = a(0)
    var diff: Int = a(len - 1) - bi
    var borrow: Boolean = ((diff >>> 31) > (a(len - 1) >>> 31) - (bi >>> 31))
    a(len - 1) = diff
    a(0) = 0
    if (borrow) {
      var i: Int = len - 2
      do {
        diff = a(i) - 1
        a(i) = diff
        borrow = diff == -1
        i -= 1
      } while (borrow && i >= 0)
    }
    if (borrow) {
      var i: Int = a.length - 1
      var carry: Boolean = true
      a(0) = 0
      while (carry && i >= 0) {
        val sum: Int = a(i) + 1
        a(i) = sum
        carry = sum == 0
        i -= 1
      }
    }
  }

  final def modFn(a: Array[Array[Int]]) {
    var i: Int = 0
    while (i < a.length) {
      modFn(a(i))
      i += 1
    }
  }

  /**
   * Multiplies a number by 2<sup>-numBits</sup> modulo 2<sup>2<sup>n</sup></sup>+1, where n is
   * <code>a.length*32/2</code>; in other words, n is half the number of bits in <code>a</code>.
   * "Right" means towards the higher array indices and the lower bits<br/>.
   * This is equivalent to extending the number to <code>2*(a.length-1)</code> ints and cyclicly
   * shifting it to the right by <code>numBits</code> bits.<br/>
   * The result is returned in the third argument.
   * @param a a number in base 2<sup>32</sup> starting with the highest digit; the array's length must be 2^n+1 for some n
   * @param numBits the shift amount in bits; must be less than <code>32*2*(len-1))</code>
   * @param b the return value; must be at least as long as <code>a</code>
   */
  final def shiftRightModFn(a: Array[Int], numBits: Int, b: Array[Int]) {
    val len: Int = a.length
    if (numBits >= 32 * (len - 1)) {
      shiftLeftModFn(a, 32 * 2 * (len - 1) - numBits, b)
      return
    }
    val numElements: Int = numBits / 32
    if (numElements > 0) {
      var borrow: Boolean = false

      {
        var i: Int = 1
        while (i < len - numElements) {
          {
            var diff: Int = a(i)
            if (borrow) ({
              diff -= 1; diff + 1
            })
            b(numElements + i) = diff
            borrow = diff == -1 && borrow
          }
          ({
            i += 1; i - 1
          })
        }
      }
      var diff: Int = 0
      if (numElements < len - 1) diff = a(0) - a(len - 1)
      else diff = 0
      if (borrow) {
        diff -= 1
        borrow = diff == -1
      }
      else borrow = a(0) == 0 && a(len - 1) != 0
      b(numElements) = diff

      {
        var i: Int = 1
        while (i < numElements) {
          {
            b(numElements - i) = -a(len - 1 - i)
            if (borrow) ({
              b(numElements - i) -= 1; b(numElements - i) + 1
            })
            borrow = b(numElements - i) != 0 || borrow
          }
          ({
            i += 1; i - 1
          })
        }
      }
      var carry: Boolean = borrow
      if (carry) {
        b(0) = 0
        var i: Int = len - 1
        do {
          val sum: Int = b(i) + 1
          b(i) = sum
          carry = sum == 0
          i -= 1
        } while (carry && i >= 0)
      }
      else b(0) = 0
    }
    else System.arraycopy(a, 0, b, 0, len)
    val numBitsFrac: Int = numBits % 32
    if (numBitsFrac != 0) {
      val bhi: Int = b(len - 1) << (32 - numBitsFrac)
      b(len - 1) >>>= numBitsFrac

      {
        var i: Int = len - 1
        while (i > 0) {
          {
            b(i) |= b(i - 1) << (32 - numBitsFrac)
            b(i - 1) >>>= numBitsFrac
          }
          ({
            i -= 1; i + 1
          })
        }
      }
      val diff: Int = b(1) - bhi
      val borrow: Boolean = ((diff >>> 31) > (b(1) >>> 31) - (bhi >>> 31))
      b(1) = diff
      var carry: Boolean = borrow
      if (carry) {
        b(0) = 0
        var i: Int = len - 1
        do {
          val sum: Int = b(i) + 1
          b(i) = sum
          carry = sum == 0
          i -= 1
        } while (carry && i >= 0)
      }
      else b(0) = 0
    }
  }

  /**
   * Multiplies a number by 2<sup>numBits</sup> modulo 2<sup>2<sup>n</sup></sup>+1, where n is
   * <code>a.length*32/2</code>; in other words, n is half the number of bits in <code>a</code>.
   * "Left" means towards the higher array indices and the lower bits<br/>.
   * This is equivalent to extending the number to <code>2*(a.length-1)</code> ints and cyclicly
   * shifting it to the left by <code>numBits</code> bits.<br/>
   * The result is returned in the third argument.
   * @param a a number in base 2<sup>32</sup> starting with the highest digit; the array's length must be 2^n+1 for some n
   * @param numBits the shift amount in bits
   * @param b the return value; must be at least as long as <code>a</code>
   */
  final def shiftLeftModFn(a: Array[Int], numBits: Int, b: Array[Int]) {
    val len: Int = a.length
    if (numBits > 32 * (len - 1)) {
      shiftRightModFn(a, 32 * 2 * (len - 1) - numBits, b)
      return
    }
    val numElements: Int = numBits / 32
    if (numElements > 0) {
      var borrow: Boolean = false

      {
        var i: Int = 0
        while (i < numElements) {
          {
            b(len - 1 - i) = -a(numElements - i)
            if (borrow) ({
              b(len - 1 - i) -= 1; b(len - 1 - i) + 1
            })
            borrow = b(len - 1 - i) != 0 || borrow
          }
          ({
            i += 1; i - 1
          })
        }
      }
      var diff: Int = 0
      if (numElements < len - 1) diff = a(len - 1) - a(0)
      else diff = -a(0)
      if (borrow) {
        diff -= 1
        borrow = diff == -1
      }
      else borrow = a(0) == 1 && diff == -1
      b(len - 1 - numElements) = diff

      {
        var i: Int = 1
        while (i < len - numElements - 1) {
          {
            diff = a(len - 1 - i)
            if (borrow) ({
              diff -= 1; diff + 1
            })
            b(len - 1 - numElements - i) = diff
            borrow = diff == -1 && borrow
          }
          ({
            i += 1; i - 1
          })
        }
      }
      var carry: Boolean = borrow
      if (carry) {
        b(0) = 0
        var i: Int = len - 1
        do {
          val sum: Int = b(i) + 1
          b(i) = sum
          carry = sum == 0
          i -= 1
        } while (carry && i >= 0)
      }
      else b(0) = 0
    }
    else System.arraycopy(a, 0, b, 0, len)
    val numBitsFrac: Int = numBits % 32
    if (numBitsFrac != 0) {
      b(0) <<= numBitsFrac

      {
        var i: Int = 1
        while (i < len) {
          b(i - 1) |= b(i) >>> (32 - numBitsFrac)
          b(i) <<= numBitsFrac
          i += 1
        }
      }
    }
    modFn(b)
  }

  /**
   * Adds two numbers, <code>a</code> and <code>b</code>, after shifting <code>b</code> by
   * <code>numElements</code> elements.<br/>
   * Both numbers are given as <code>int</code> arrays and must be <b>positive</b> numbers
   * (meaning they are interpreted as unsigned).<br/>
   * The result is returned in the first argument.
   * If any elements of b are shifted outside the valid range for <code>a</code>, they are dropped.
   * @param a a number in base 2<sup>32</sup> starting with the highest digit
   * @param b a number in base 2<sup>32</sup> starting with the highest digit
   * @param numElements
   */
  final def addShifted(a: Array[Int], b: Array[Int], numElements: Int) {
    var carry: Boolean = false
    var aIdx: Int = a.length - 1 - numElements
    var bIdx: Int = b.length - 1
    var i: Int = Math.min(aIdx, bIdx)
    while (i >= 0) {
      val ai: Int = a(aIdx)
      var sum: Int = ai + b(bIdx)
      if (carry) ({
        sum += 1; sum - 1
      })
      carry = ((sum >>> 31) < (ai >>> 31) + (b(bIdx) >>> 31)) // carry if signBit(sum) < signBit(a)+signBit(b)
      a(aIdx) = sum
      i -= 1
      aIdx -= 1
      bIdx -= 1
    }
    while (carry && aIdx >= 0) {
      a(aIdx) += 1
      carry = a(aIdx) == 0
      aIdx -= 1
    }
  }

  /**
   * Adds two <b>positive</b> numbers (meaning they are interpreted as unsigned) modulo 2<sup>numBits</sup>.
   * Both input values are given as <code>int</code> arrays.
   * The result is returned in the first argument.
   * @param a a number in base 2<sup>32</sup> starting with the highest digit
   * @param b a number in base 2<sup>32</sup> starting with the highest digit
   */
  final def addModPow2(a: Array[Int], b: Array[Int], numBits: Int) {
    val numElements: Int = (numBits + 31) / 32
    var carry: Boolean = false
    var i: Int = 0
    var aIdx: Int = a.length - 1
    var bIdx: Int = b.length - 1

    {
      i = numElements - 1
      while (i >= 0) {
        var sum: Int = a(aIdx) + b(bIdx)
        if (carry) ({
          sum += 1; sum - 1
        })
        carry = ((sum >>> 31) < (a(aIdx) >>> 31) + (b(bIdx) >>> 31)) // carry if signBit(sum) < signBit(a)+signBit(b)
        a(aIdx) = sum
        aIdx -= 1
        bIdx -= 1
        i -= 1
      }
    }
    if (numElements > 0) a(aIdx + 1) &= -1 >>> (32 - (numBits % 32))
    while (aIdx >= 0) {
      a(aIdx) = 0
      aIdx -= 1
    }
  }

  /**
   * Subtracts two <b>positive</b> numbers (meaning they are interpreted as unsigned) modulo 2<sup>numBits</sup>.
   * Both input values are given as <code>int</code> arrays.
   * The result is returned in the first argument.
   * @param a a number in base 2<sup>32</sup> starting with the highest digit
   * @param b a number in base 2<sup>32</sup> starting with the highest digit
   */
  final def subModPow2(a: Array[Int], b: Array[Int], numBits: Int) {
    val numElements: Int = (numBits + 31) / 32
    var carry: Boolean = false
    var i: Int = 0
    var aIdx: Int = a.length - 1
    var bIdx: Int = b.length - 1

    {
      i = numElements - 1
      while (i >= 0) {
        var diff: Int = a(aIdx) - b(bIdx)
        if (carry) ({
          diff -= 1; diff + 1
        })
        carry = ((diff >>> 31) > (a(aIdx) >>> 31) - (b(bIdx) >>> 31)) // carry if signBit(diff) > signBit(a)-signBit(b)
        a(aIdx) = diff
        aIdx -= 1
        bIdx -= 1
        i -= 1
      }
    }
    if (numElements > 0) a(aIdx + 1) &= -1 >>> (32 - (numBits % 32))
    while (aIdx >= 0) {
      a(aIdx) = 0
      aIdx -= 1
    }
  }

  /**
    * Cyclicly shifts a number to the right modulo 2<sup>2<sup>n+1</sup></sup>, where n is
    * <code>a.length*32/2</code>; in other words, n is half the number of bits in <code>a</code>.<br/>
    * "Right" means towards the lower array indices and the lower bits; this is equivalent to
    * a multiplication by 2<sup>-numBits</sup> modulo 2<sup>2<sup>n+1</sup></sup>.<br/>
    * The result is returned in the third argument.
    * @param a a number in base 2<sup>32</sup> starting with the highest digit; the length must be a power of 2
    * @param numBits the shift amount in bits
    * @param b the return value; must be at least as long as <code>a</code>
    */
  final def cyclicShiftRight(a: Array[Int], _numBits: Int, b: Array[Int]): Unit = {
    var numBits = _numBits
    val numElements = numBits / 32;
    scala.compat.Platform.arraycopy(a, 0, b, numElements, a.length - numElements);
    scala.compat.Platform.arraycopy(a, a.length - numElements, b, 0, numElements);

    numBits = numBits % 32;
    if (numBits != 0) {
      val bhi = b(b.length - 1);
      b(b.length - 1) = b(b.length - 1) >>> numBits

      var i = b.length - 1
      while (i > 0) {
        b(i) |= b(i - 1) << (32 - numBits)
        b(i - 1) = b(i - 1) >>> numBits
        i -= 1
      }
      b(0) |= bhi << (32 - numBits)
    }
  }

  /**
    * Cyclicly shifts a number to the left modulo 2<sup>2<sup>n+1</sup></sup>, where n is
    * <code>a.length*32/2</code>; in other words, n is half the number of bits in <code>a</code>.<br/>
    * "Left" means towards the lower array indices and the lower bits; this is equivalent to
    * a multiplication by 2<sup>numBits</sup> modulo 2<sup>2<sup>n+1</sup></sup>.<br/>
    * The result is returned in the third argument.
    * @param a a number in base 2<sup>32</sup> starting with the highest digit; the length must be a power of 2
    * @param numBits the shift amount in bits
    * @param b the return value; must be at least as long as <code>a</code>
    */
  final def cyclicShiftLeftBits(a: Array[Int], _numBits: Int, b: Array[Int]): Unit = {
    var numBits = _numBits
    val numElements = numBits / 32;
    scala.compat.Platform.arraycopy(a, numElements, b, 0, a.length - numElements);
    scala.compat.Platform.arraycopy(a, 0, b, a.length - numElements, numElements);

    numBits = numBits % 32;
    if (numBits != 0) {
      val b0 = b(0)
      b(0) <<= numBits

      var i = 1
      while (i < b.length) {
        b(i - 1) |= b(i) >>> (32 - numBits);
        b(i) <<= numBits
        i += 1
      }
      b(b.length - 1) |= b0 >>> (32 - numBits);
    }
  }

  /**
    * Reads <code>bBitLength</code> bits from <code>b</code>, starting at array index
    * <code>bStart</code>, and copies them into <code>a</code>, starting at bit
    * <code>aBitLength</code>. The result is returned in <code>a</code>.
    * @param a
    * @param aBitLength
    * @param b
    * @param bStart
    * @param bBitLength
    */
  final def appendBits(a: Array[Int], aBitLength: Int, b: Array[Int], bStart: Int, bBitLength: Int): Unit = {
    var aIdx: Int = a.length - 1 - aBitLength / 32
    val bit32: Int = aBitLength % 32

    var i = bStart + bBitLength / 32 - 1
    while (i >= bStart) {
      if (bit32 > 0) {
        a(aIdx) |= b(i) << bit32
        aIdx -= 1
        a(aIdx) = b(i) >>> (32 - bit32)
      } else {
        a(aIdx) = b(i)
        aIdx -= 1
      }
      i -= 1
    }

    if (bBitLength % 32 > 0) {
      aIdx = a.length - 1 - (aBitLength / 32 + bBitLength / 32)
      val bIdx = bBitLength / 32
      var bi = b(b.length - 1 - bStart + bIdx)
      bi &= -1 >>> (32 - bBitLength)
      a(aIdx) |= bi << bit32
      if (bit32 + (bBitLength % 32) > 32)
        a(aIdx - 1) = bi >>> (32 - bit32)
    }
  }

  /**
    * Divides an <code>int</code> array into pieces <code>bitLength</code> bits long.
    * @param a
    * @param bitLength
    * @return a new array containing <code>bitLength</code> bits from <code>a</code> in each subarray
    */
  final def splitBits(a: Array[Int], bitLength: Int): Array[Array[Int]] = {
    var aIntIdx: Int = a.length - 1
    var aBitIdx: Int = 0
    val numPieces: Int = (a.length * 32 + bitLength - 1) / bitLength
    val pieceLength: Int = (bitLength + 31) / 32 // in ints
    val b: Array[Array[Int]] = Array.ofDim[Int](numPieces, pieceLength)

    var i = 0
    while (i < b.length) {
      var bitsRemaining: Int = math.min(bitLength, a.length * 32 - i * bitLength);
      var bIntIdx: Int = bitLength / 32;
      if (bitLength % 32 == 0)
        bIntIdx -= 1
      var bBitIdx = 0;
      while (bitsRemaining > 0) {
        var bitsToCopy: Int = Math.min(32 - aBitIdx, 32 - bBitIdx);
        bitsToCopy = math.min(bitsRemaining, bitsToCopy);
        var mask: Int = a(aIntIdx) >>> aBitIdx;
        mask &= -1 >>> (32 - bitsToCopy);
        mask <<= bBitIdx;
        b(i)(bIntIdx) |= mask;
        bitsRemaining -= bitsToCopy;
        aBitIdx += bitsToCopy;
        if (aBitIdx >= 32) {
          aBitIdx -= 32;
          aIntIdx -= 1
        }
        bBitIdx += bitsToCopy;
        if (bBitIdx >= 32) {
          bBitIdx -= 32;
          bIntIdx -= 1
        }
      }
      i += 1
    }
    return b;
  }

  /**
    * Splits an <code>int</code> array into pieces of <code>pieceSize ints</code> each, and
    * pads each piece to <code>targetPieceSize ints</code>.
    * @param a the input array
    * @param numPieces the number of pieces to split the array into
    * @param pieceSize the size of each piece in the input array in <code>ints</code>
    * @param targetPieceSize the size of each piece in the output array in <code>ints</code>
    * @return an array of length <code>numPieces</code> containing subarrays of length <code>targetPieceSize</code>
    */
  final def splitInts(a: Array[Int], numPieces: Int, pieceSize: Int, targetPieceSize: Int): Array[Array[Int]] = {
    val ai: Array[Array[Int]] = Array.ofDim[Int](numPieces, targetPieceSize)
    var i: Int = 0
    while (i < a.length / pieceSize) {
      scala.compat.Platform.arraycopy(a, a.length - i * pieceSize - pieceSize, ai(i), targetPieceSize - pieceSize, pieceSize)
      i += 1
    }
    scala.compat.Platform.arraycopy(a, a.length - a.length / pieceSize * pieceSize - (a.length % pieceSize), ai(a.length / pieceSize), targetPieceSize - (a.length % pieceSize), a.length % pieceSize)
    return ai
  }

  def divideAndRemainderByInteger(a: BigInt2, d: Int, sign: Int) = {
    (zero, zero)
  }

  def divideAndRemainderKnuthPositive(_a: BigInt2, _b: BigInt2): (BigInt2, BigInt2) = {
    var a = _a.digits.reverse
    var b = _b.digits.reverse
    val aLength = a.length
    val bLength = b.length
    val quotLength = aLength - bLength + 1
    var quot = new Array[Int](quotLength)

    var normA = new Array[Int](aLength + 1)
    var normB = new Array[Int](bLength + 1)
    val normBLength = bLength
    val divisorShift = Integer.numberOfLeadingZeros(b(bLength - 1));
    if (divisorShift != 0) {
      shiftLeft1(normB, b, 0, divisorShift)
      shiftLeft1(normA, a, 0, divisorShift)
    } else {
      System.arraycopy(a, 0, normA, 0, aLength)
      System.arraycopy(b, 0, normB, 0, bLength)
    }
    var firstDivisorDigit = normB(normBLength - 1)
    var i = quotLength - 1
    var j = aLength

    while (i >= 0) {
      var guessDigit = 0;
      if (normA(j) == firstDivisorDigit) {
          guessDigit = -1
      } else {
        var product = (((normA(j) & 0xffffffffL) << 32) + (normA(j - 1) & 0xffffffffL))
        var res = divideLongByInt(product, firstDivisorDigit)
        guessDigit = res.toInt
        var rem = (res >> 32).toInt
        if (guessDigit != 0) {
          var leftHand = 0L
          var rightHand = 0L
          var rOverflowed = false
          var break = false
          guessDigit += 1
          do {
            guessDigit -= 1
            if (rOverflowed) {
              break = true
            }
            else {
              leftHand = (guessDigit & 0xffffffffL) *
                (normB(normBLength - 2) & 0xffffffffL)
              rightHand = (rem.toLong << 32) +
                (normA(j - 2) & 0xffffffffL)
              var longR = (rem & 0xffffffffL) +
                (firstDivisorDigit & 0xffffffffL);
              if (Integer.numberOfLeadingZeros((longR >>> 32).toInt) < 32) {
                rOverflowed = true;
              } else {
                rem = longR.toInt
              }
            }
          } while (((leftHand ^ 0x8000000000000000L) > (rightHand ^ 0x8000000000000000L)) && break == false)
        }
      }
      if (guessDigit != 0) {
        var borrow = multiplyAndSubtract(normA, j -
          normBLength, normB, guessDigit)
        if (borrow != 0) {
          guessDigit -= 1
          var carry = 0L
          for (k <- 0 until normBLength) {
            carry += (normA(j - normBLength + k) & 0xffffffffL)
                    + (normB(k) & 0xffffffffL);
            normA(j - normBLength + k) = carry.toInt
            carry >>>= 32;
          }
        }
      }
      if (quot != null) {
        quot(i) = guessDigit;
      }
      j -= 1
      i -= 1
    }
    val quoSign = if (_a.sign == _b.sign) 1 else -1
    if (divisorShift != 0) {
      shiftRight1(normB, normBLength, normA, 0, divisorShift);
      (new BigInt2(quoSign, removeLeadingZeroes(quot.reverse)), new BigInt2(_a.sign, removeLeadingZeroes(normB.reverse)))
    }
    else {
      System.arraycopy(normA, 0, normB, 0, bLength)
      (new BigInt2(quoSign, removeLeadingZeroes(quot.reverse)), new BigInt2(_b.sign, removeLeadingZeroes(normA.reverse)))
    }
  }

  final def divideLongByInt(a: Long, bInt: Int): Long = {
    val b = bInt & 0xFFFFFFFFL
    if (a >= 0)
      ((a % b) << 32) | (a / b)
    else {
      val aPos = a >>> 1
      val bPos = b >>> 1
      var quot: Long = aPos / bPos
      var rem: Long = ((aPos % bPos) << 1) + (a & 1)
      if ((b & 1) != 0) {
        if (quot <= rem)
          rem -= quot
        else if (quot - rem <= b) {
          rem += b - quot
          quot -= 1
        }
        else {
          rem += (b << 1) - quot
          quot -= 2
        }
      }
      (rem << 32) | quot
    }
  }

  final def multiplyAndSubtract(a: Array[Int], start: Int, b: Array[Int], c: Int) = {
    var carry0 = 0L
    var carry1 = 0L
    var bLen = b.length - 1
    for (i <- 0 until bLen) {
      carry0 = ( b(i) & 0xFFFFFFFFL) * (c & 0xFFFFFFFFL) + (carry0 & 0xFFFFFFFFL)
      carry1 = (a(start+i) & 0xffffffffL) - (carry0 & 0xffffffffL) + carry1;
      a(start+i) = carry1.toInt
      carry1 >>=  32; // -1 or 0
      carry0 >>>= 32;
    }
    carry1 = (a(start + bLen) & 0xffffffffL) - carry0 + carry1;
    a(start + bLen) = carry1.toInt
    (carry1 >> 32).toInt // -1 or 0
  }

  /**
    * Computes <code>a/b</code> and <code>a%b</code> using the
    * <a href="http://cr.yp.to/bib/1998/burnikel.ps"> Burnikel-Ziegler algorithm</a>.
    * This method implements algorithm 3 from pg. 9 of the Burnikel-Ziegler paper.
    * The parameter beta is 2<sup>32</sup> so all shifts are multiples of 32 bits.<br/>
    * <code>a</code> and <code>b</code> must be nonnegative.
    * @param a the dividend
    * @param b the divisor
    * @return an array containing the quotient and remainder
    */
  final def divideAndRemainderBurnikelZieglerPositive(_a: BigInt2, _b: BigInt2): (BigInt2, BigInt2) = {
    var a = _a
    var b = _b
    val r: Int = a.digits.length
    val s: Int = b.digits.length

    if (r < s)
      return (BigInt2.zero, a)
    else {
      // let m = min{2^k | (2^k)*BURNIKEL_ZIEGLER_THRESHOLD > s}
      val m: Int = 1 << (32 - Integer.numberOfLeadingZeros(s / BurnikelZieglerThreshold))

      val j: Int = (s + m - 1) / m // j = ceil(s/m)
      val n: Int = j * m // block length in 32-bit units
      val n32: Int = 32 * n // block length in bits
      val sigma: Int = math.max(0, n32 - b.bitLength)
      b = b << sigma // shift b so its length is a multiple of n
      a = a << sigma // shift a by the same amount

      // t is the number of blocks needed to accommodate 'a' plus one additional bit
      var t: Int = (a.bitLength + n32) / n32
      if (t < 2)
        t = 2
      val a1 = a getBlock (t - 1, t, n) // the most significant block of a
      val a2 = a getBlock (t - 2, t, n) // the second to most significant block

      // do schoolbook division on blocks, dividing 2-block numbers by 1-block numbers
      var z = (a1 shiftLeftInts n) + a2 // Z[t-2]
      var quotient = BigInt2.zero
      var c: (BigInt2, BigInt2) = null
      var i = t - 2
      while (i > 0) {
        c = divide2n1n(z, b)
        z = a getBlock (i - 1, t, n)
        z = z + (c._2 shiftLeftInts n)
        quotient = (quotient + c._1) shiftLeftInts n
        i -= 1
      }
      // do the loop one more time for i=0 but leave z unchanged
      c = divide2n1n(z, b)
      quotient = quotient + c._1

      val remainder = c._2 >> sigma // a and b were shifted, so shift back
      return (quotient, remainder)
    }
  }

  /**
    * This method implements algorithm 1 from pg. 4 of the Burnikel-Ziegler paper.
    * It divides a 2n-digit number by a n-digit number.<br/>
    * The parameter beta is 2<sup>32</sup> so all shifts are multiples of 32 bits.
    * @param a a nonnegative number such that <code>a.bitLength() <= 2*b.bitLength()</code>
    * @param b a positive number such that <code>b.bitLength()</code> is even
    * @return <code>a/b</code> and <code>a%b</code>
    */
  final def divide2n1n(a: BigInt2, b: BigInt2): (BigInt2, BigInt2) = {
    val n = b.digits.length;
    if (n % 2 != 0 || n < BurnikelZieglerThreshold)
      return (a, b)//a divideAndRemainderKnuth b

    // view a as [a1,a2,a3,a4] and divide [a1,a2,a3] by b
    val (c1div, c1rem) = divide3n2n(a shiftRightInts (n / 2), b)

    // divide the concatenation of c1[1] and a4 by b
    val a4 = a lowerInts (n / 2)
    val (c2div, c2rem) = divide3n2n((c1rem shiftLeftInts (n / 2)) + a4, b)

    // quotient = the concatentation of the two above quotients
    ((c1div shiftLeftInts (n / 2)) + c2div, c2rem)
  }

  /**
    * This method implements algorithm 2 from pg. 5 of the Burnikel-Ziegler paper.
    * It divides a 3n-digit number by a 2n-digit number.<br/>
    * The parameter beta is 2<sup>32</sup> so all shifts are multiples of 32 bits.<br/>
    * @param a a nonnegative number such that <code>2*a.bitLength() <= 3*b.bitLength()</code>
    * @param b a positive number such that <code>b.bitLength()</code> is even
    * @return <code>a/b</code> and <code>a%b</code>
    */
  final def divide3n2n(a: BigInt2, b: BigInt2): (BigInt2, BigInt2) = {
    val n: Int = b.digits.length / 2; // half the length of b in ints

    // split a in 3 parts of length n or less
    val a1 = a shiftRightInts (2 * n)
    val a2 = a shiftAndTruncate n
    val a3 = a lowerInts n

    // split a in 2 parts of length n or less
    val b1 = b shiftRightInts n
    val b2 = b lowerInts n

    var q: BigInt2 = null
    var r1: BigInt2 = null
    val a12 = (a1 shiftLeftInts n) + a2 // concatenation of a1 and a2
    if (a1.compareTo(b1) < 0) {
      // q=a12/b1, r=a12%b1
      val (div, rem) = divide2n1n(a12, b1)
      q = div
      r1 = rem
    } else {
      // q=beta^n-1, r=a12-b1*2^n+b1
      q = ones(n)
      r1 = (a12 - (b1 shiftLeftInts n)) + b1
    }

    val d = q * b2
    var r = (r1 shiftLeftInts n) + a3 - d // r = r1*beta^n + a3 - d (paper says a4)

    // add b until r>=0
    while (r.signum < 0) {
      r = r + b
      q = q - BigInt2.zero
    }

    return (q, r)
  }

  /**
    * Returns an <code>n</code>-int number all of whose bits are ones
    * @param n number of ints in the <code>mag</code> array
    * @return a number equal to <code>ONE.shiftLeft(32*n).subtract(ONE)</code>
    */
  final def ones(n: Int): BigInt2 = {
    val mag: Array[Int] = new Array[Int](n)
    java.util.Arrays.fill(mag, -1);
    new BigInt2(1, mag);
  }

}

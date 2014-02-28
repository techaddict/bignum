bignum
======

Big Numbers Library Written purely in Scala
[![Build Status](https://travis-ci.org/techaddict/bignum.png)](https://travis-ci.org/techaddict/bignum)

* *Constructors*
```scala
BigInt2(str: String)
BigInt2(str: String, radix: Int)
BigInt2(value: Int)
BigInt2(value: Long)
```

* *Methods*
  + Addition: `a + b`
  + Subtraction: `a - b`
  + Multiplication: `a * b`
  + Division: `a / b`
  + Absolute: `a.abs`
  + Min: `a min b`
  + Max: `a max b`
  + compare: `a compare b`
  + intValue: `a.intValue`
  + longValue: `a.longValue`
  + signum: `a.signum`
  + Negate: `- a`
  + Left Shift: `a << Int`
  + Right Shift: `a >> Int`

Benchmark
---------
For Benchmarking, Length of Bignum in Base 2

Oracle JDK7, Scala 2.10.2
* *Addition*
```
length  benchmark    ns linear runtime
    32     BigInt   281 =
    32    BigInt2   229 =
    32 BigInteger   176 =
    64     BigInt   273 =
    64    BigInt2   246 =
    64 BigInteger   188 =
   200     BigInt   278 =
   200    BigInt2   274 =
   200 BigInteger   214 =
   500     BigInt   350 =
   500    BigInt2   337 =
   500 BigInteger   274 =
  1000     BigInt   448 =
  1000    BigInt2   453 =
  1000 BigInteger   369 =
  5000     BigInt  1430 ==
  5000    BigInt2  1895 ===
  5000 BigInteger  1357 ==
 10000     BigInt  3010 =====
 10000    BigInt2  4452 =======
 10000 BigInteger  3639 ======
 50000     BigInt 13822 ========================
 50000    BigInt2 17260 ==============================
 50000 BigInteger 12777 ======================
```
* *Multiplication*
```
 length  benchmark         ns linear runtime
     32     BigInt        910 =
     32    BigInt2        840 =
     32 BigInteger        879 =
     64     BigInt       2391 =
     64    BigInt2       2650 =
     64 BigInteger       2359 =
    200     BigInt      21930 =
    200    BigInt2      24863 =
    200 BigInteger      21436 =
    500     BigInt     130576 =
    500    BigInt2     150619 =
    500 BigInteger     136897 =
   1000     BigInt     536052 =
   1000    BigInt2     595522 =
   1000 BigInteger     512093 =
   5000     BigInt   12382673 =
   5000    BigInt2   14090820 =
   5000 BigInteger   12690067 =
  10000     BigInt   49432553 =
  10000    BigInt2   56337811 =
  10000 BigInteger   49443600 =
  50000     BigInt 1208393500 =========================
  50000    BigInt2 1398873000 ==============================
  50000 BigInteger 1237180500 ==========================
```
Find the Benchmarking Code [here](https://github.com/techaddict/bignum/tree/master/benchmark)

To run the code just enter `sbt 'project benchmark' run`

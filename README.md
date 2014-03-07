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
  + Absolute: `a.abs`
  + Pow `a.pow(Int)`
  + Min: `a min b`
  + Max: `a max b`
  + compare: `a compare b`
  + intValue: `a.intValue`
  + longValue: `a.longValue`
  + signum: `a.signum`
  + Negate: `- a`
  + Left Shift: `a << Int`
  + Right Shift: `a >> Int`
  + Bit Length: `a.bitLength`
  + TestBit: `a.testBit(Int)`
  + Lowest Set Bit: `a.lowestSetBit`

**Benchmark**

[Addition](https://github.com/techaddict/bignum/wiki/Benchmark-Results#wiki-addition)

[Multiplication](https://github.com/techaddict/bignum/wiki/Benchmark-Results#wiki-multiplication)

[Left Shift](https://github.com/techaddict/bignum/wiki/Benchmark-Results#wiki-multiplication)

Find the Benchmarking Code [here](https://github.com/techaddict/bignum/tree/master/benchmark)

To run the code just enter `sbt 'project benchmark' run`

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

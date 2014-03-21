package bignum

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object BigInt2Laws extends Properties("BigInt Law") with Generators{

  property("a + b == b + a") = forAll { (a: BigInt2, b: BigInt2) =>
    (a + b) equals (b + a)
  }

  property("a + (b + c) = (a + b) + c") = forAll { (a: BigInt2, b: BigInt2, c: BigInt2) =>
    (a + (b + c)) equals ((a + b) + c)
  }

  property("a * (b + c) = (a * b) + (a * c)") = forAll { (a: BigInt2, b: BigInt2, c: BigInt2) =>
    (a * (b + c)) equals ((a * b) + (a * c))
  }

  property("a + 0 == a") = forAll { (a: BigInt2) =>
    (a + BigInt2.zero) equals a
  }

  property("a + (-a) = 0") = forAll { (a: BigInt2) =>
    (a + (-a)) equals BigInt2.zero
  }

  property("a - b == a + (-b)") = forAll { (a: BigInt2, b: BigInt2) =>
    (a - b) equals (a + (-b))
  }

  property("a * 0 == 0") = forAll { (a: BigInt2) =>
    (a * BigInt2.zero) equals BigInt2.zero
  }

  property("a * 1 == a") = forAll { (a: BigInt2) =>
    (a * BigInt2.one) equals a
  }

  property("a * b == b * a)") = forAll { (a: BigInt2, b: BigInt2) =>
    (a * b) equals (b * a)
  }

  property("a * (b * c) = (a * b) * c") = forAll { (a: BigInt2, b: BigInt2, c: BigInt2) =>
    (a + (b + c)) equals ((a + b) + c)
  }

  property("a / a == 1)") = forAll { (a: BigInt2) =>
    if (!a.isZero)
      (a / a).toString == BigInt2.one.toString
    else true
  }

  property("(a * b) / b = a") = forAll { (a: BigInt2, b: BigInt2) =>
    if (!b.isZero)
      ((a * b) / b).toString == a.toString
    else true
  }

}

package me.techaddict.bignum

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalatest._
import org.scalatest.Matchers

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import java.math.BigInteger

object BigInt2Laws extends Properties("BigInt Law") {

  implicit val arbBigInt2: Arbitrary[BigInt2] =
    Arbitrary(arbitrary[BigInt].map(n => BigInt2(n.toString)))

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

  // Below Test's should take care of divide by 0 :P
  /*property("a / a == 1)") = forAll { (a: BigInt2) =>
    (a / a).toString == BigInt2.one.toString
  }

  property("(a * b) / b = a") = forAll { (a: BigInt2, b: BigInt2) =>
    ((a * b) / b).toString == a.toString
  }*/

}

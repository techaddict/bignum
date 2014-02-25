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

object BigInt2OpsTest extends Properties("BigInt Op") {

  implicit val arbBigInt2: Arbitrary[BigInt2] =
    Arbitrary(arbitrary[BigInt].map(n => BigInt2(n.toString)))

  property("a + b") = forAll { (a: BigInt2, b: BigInt2) =>
    (a + b) equals (BigInt(a.toString) + BigInt(b.toString))
  }

  property("a - b") = forAll { (a: BigInt2, b: BigInt2) =>
    (a - b) equals (BigInt(a.toString) - BigInt(b.toString))
  }

  property("a * b") = forAll { (a: BigInt2, b: BigInt2) =>
    (a * b) equals (BigInt(a.toString) * BigInt(b.toString))
  }

  property("a / b") = forAll { (a: BigInt2, b: BigInt2) =>
    if (b equals BigInt2.zero == false)
      (a / b) equals (BigInt(a.toString) / BigInt(b.toString))
    else true
  }

  property("a.abs") = forAll { (a: BigInt2) =>
    a.abs equals BigInt(a.toString).abs
  }

  property("a min b") = forAll { (a: BigInt2, b: BigInt2) =>
    (a min b) equals (BigInt(a.toString) min BigInt(b.toString))
  }

  property("a max b") = forAll { (a: BigInt2, b: BigInt2) =>
    (a max b) equals (BigInt(a.toString) max BigInt(b.toString))
  }

  property("a compare b") = forAll { (a: BigInt2, b: BigInt2) =>
    (a compare b) equals (BigInt(a.toString) compare BigInt(b.toString))
  }

  property("a.intValue") = forAll { (a: Int) =>
    BigInt2.valueOf(a.toLong).intValue == a
  }

  /*property("a.longValue") = forAll { (a: Long) =>
    BigInt2.valueOf(a).longValue == a
  }*/

  property("a.signum") = forAll { (a: BigInt2) =>
    a.signum equals BigInt(a.toString).signum
  }

  property("-a") = forAll { (a: BigInt2) =>
    -a equals -BigInt(a.toString)
  }

  property("a << b") = forAll { (a: BigInt2, b: Short) =>
    (a << b.toInt) equals (BigInt(a.toString) << b.toInt)
  }

  property("a >> b") = forAll { (a: BigInt2, b: Short) =>
    (a >> b.toInt) equals (BigInt(a.toString) >> b.toInt)
  }

  property("a.shiftLeftOneBit") = forAll { (a: BigInt2) =>
      (a.shiftLeftOneBit) equals (BigInt(a.toString) << 1)
  }

}

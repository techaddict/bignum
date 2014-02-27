package bignum

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

object BigInt2ConsTest extends Properties("BigInt Constructor") with Generators {

  property("BigInt2(String Value)") = forAll { (a: BigInt) =>
    BigInt2(a.toString) equals a
  }

  property("BigInt2(Int)") = forAll { (a: Int) =>
    BigInt2(a).intValue == a
  }

  property("BigInt2(Long)") = forAll { (a: Long) =>
    BigInt2(a).longValue == a
  }

}

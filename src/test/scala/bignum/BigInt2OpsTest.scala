package bignum

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object BigInt2OpsTest extends Properties("BigInt Op") with Generators {

  property("List[Op]") = forAll { (y0: BigInt, ops: List[Op]) =>
    var x = BigInt2(y0.toString)
    var y = y0
    ops.forall { op =>
      x = op(x)
      y = op(y)
      x equals y
    }
  }

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
    if (!b.isZero)
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
    BigInt2(a).intValue == a
  }

  /*property("a.longValue") = forAll { (a: Long) =>
    BigInt2(a).longValue == a
  }*/

  property("a.signum") = forAll { (a: BigInt2) =>
    a.signum equals BigInt(a.toString).signum
  }

  property("a.BitLength") = forAll { (a: BigInt2) =>
    a.bitLength == BigInt(a.toString).bitLength
  }

  property("a.bitCount") = forAll { (a: BigInt2) =>
    a.bitCount == BigInt(a.toString).bitCount
  }

  property("a.lowestSetBit") = forAll { (a: BigInt2) =>
    a.lowestSetBit == BigInt(a.toString).lowestSetBit
  }

  property("a.testBit") = forAll { (a: BigInt2, b1: Byte) =>
    val b: Int = b1 & 0xff
    a.testBit(b) == BigInt(a.toString).testBit(b)
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

  property("a.pow(b)") = forAll { (a: BigInt2, b1: Byte) =>
    val b = b1 & 0xff
    (a pow b) equals (BigInt(a.toString) pow b)
  }

}

package bignum

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.{oneOf, listOf}
import org.scalacheck.Arbitrary.arbitrary

trait Generators {

  implicit val arbBigInt2: Arbitrary[BigInt2] =
    Arbitrary(arbitrary[BigInt].map(n => BigInt2(n.toString)))

  implicit val arbNonZeroBigInt2: Arbitrary[NonZeroBigInt2] =
    Arbitrary(arbitrary[BigInt].map(n => if (n != BigInt(0))NonZeroBigInt2(n.toString) else NonZeroBigInt2("1")))

  implicit lazy val arbOp: Arbitrary[Op] =
    Arbitrary(oneOf(
      'negate, 'abs, 'add, 'subtract, 'subtractfrom, 'multiply, 'max, 'min)
      .flatMap {
      case 'negate => Negate
      case 'abs => Abs
      case 'add => arbitrary[BigInt2].map(n => Add(n))
      case 'subtract => arbitrary[BigInt2].map(n => Subtract(n))
      case 'subtractfrom => arbitrary[BigInt2].map(n => Subtract(n))
      case 'multiply => arbitrary[BigInt2].map(n => Multiply(n))
      case 'divide => arbitrary[NonZeroBigInt2].map(n => Divide(n))
      case 'divideby => arbitrary[NonZeroBigInt2].map(n => DivideBy(n))
      case 'max => arbitrary[BigInt2].map(n => Min(n))
      case 'min => arbitrary[BigInt2].map(n => Max(n))
    })

  implicit lazy val arbListOp: Arbitrary[List[Op]] =
    Arbitrary(listOf(arbitrary[Op]))

}

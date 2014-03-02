package bignum

sealed trait Op {
  def apply(a: BigInt2): BigInt2
  def apply(a: BigInt): BigInt
}

abstract class Unop(f: BigInt2 => BigInt2)(g: BigInt => BigInt) extends Op {
  def apply(a: BigInt2): BigInt2 = f(a)
  def apply(a: BigInt): BigInt = g(a)
}

abstract class Binop(y1: BigInt2, y2: BigInt)(f: (BigInt2, BigInt2) => BigInt2)(g: (BigInt, BigInt) => BigInt) extends Op {
  def apply(a: BigInt2): BigInt2 = f(a, y1)
  def apply(a: BigInt): BigInt = g(a, y2)
  //def apply(a: NonZeroBigInt2): BigInt2 = f(a, y1)
}

//case class NonZeroBigInt2(a: String) extends BigInt2(a, 10)

case object Negate extends Unop(-_)(-_)
case object Abs extends Unop(_.abs)(_.abs)

case class Add(y: BigInt2) extends Binop(y, BigInt(y.toString))(_ + _)(_ + _)
case class Subtract(y: BigInt2) extends Binop(y, BigInt(y.toString))(_ - _)(_ - _)
case class SubtractFrom(y: BigInt2) extends Binop(y, BigInt(y.toString))((x, y) => y - x)((x, y) => y - x)
case class Multiply(y: BigInt2) extends Binop(y, BigInt(y.toString))(_ * _)(_ * _)
//case class Divide(y: NonZeroBigInt2) extends Binop(y, BigInt(y.toString))(_ / _)(_ / _)
//case class DivideBy(y: NonZeroBigInt2) extends Binop(y, BigInt(y.toString))((x, y) => y / x)((x, y) => y / x)
case class Min(y: BigInt2) extends Binop(y, BigInt(y.toString))(_ min _)(_ min _)
case class Max(y: BigInt2) extends Binop(y, BigInt(y.toString))(_ max _)(_ max _)

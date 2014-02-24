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

  def BigIntGen = for (n <- Gen.numStr if (n != "")) yield n
  def IntGen = for (n <- Gen.choose(0, 20000)) yield n

  property("Abs a.abs") = forAll(BigIntGen) {
    case (a: String) =>
      //Check only for negative BigInteger's
      val biga = BigInt2("-" + a)
      val bigintegera = new BigInteger("-" + a)
      val res0 = biga.abs
      val res1 = bigintegera.abs
      res0.toString == res1.toString
  }

  property("Minimum a.min(b)") = forAll(BigIntGen, BigIntGen) {
    case (a: String, b: String) =>
      val biga = BigInt2(a)
      val bigb = BigInt2(b)
      val bigintegera = new BigInteger(a)
      val bigintegerb = new BigInteger(b)
      val res0 = biga.min(bigb)
      val res1 = bigintegera.min(bigintegerb)
      res0.toString == res1.toString
  }

  property("Minimum a.max(b)") = forAll(BigIntGen, BigIntGen) {
    case (a: String, b: String) =>
      val biga = BigInt2(a)
      val bigb = BigInt2(b)
      val bigintegera = new BigInteger(a)
      val bigintegerb = new BigInteger(b)
      val res0 = biga.max(bigb)
      val res1 = bigintegera.max(bigintegerb)
      res0.toString == res1.toString
  }

  property("Left Shift a << b") = forAll(BigIntGen, IntGen) {
    case (a: String, b: Int) =>
      val biga = BigInt2(a)
      val bigintegera = new BigInteger(a)
      val res0 = biga << b
      val res1 = bigintegera.shiftLeft(b)
      res0.toString == res1.toString
  }

  property("Right Shift a >> b") = forAll(BigIntGen, IntGen) {
    case (a: String, b: Int) =>
      val biga = BigInt2(a)
      val bigintegera = new BigInteger(a)
      val res0 = biga >> b
      val res1 = bigintegera.shiftRight(b)
      res0.toString == res1.toString
  }

}

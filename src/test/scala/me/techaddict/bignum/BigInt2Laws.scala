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

  def BigIntGen = for (n <- Gen.numStr if (n != "")) yield n

  property("a + b == b + a") = forAll(BigIntGen, BigIntGen) {
    case (a: String, b: String) =>
      val biga = new BigInt2(a)
      val bigb = new BigInt2(b)
      val bigintegera = new BigInteger(a)
      val bigintegerb = new BigInteger(b)
      val res0 = (biga + bigb)
      val res1 = bigintegera.add( bigintegerb)
      res0.toString == res1.toString && res0.toString == (bigb + biga).toString
  }

  property("a + (b + c) = (a + b) + c") = forAll(BigIntGen, BigIntGen, BigIntGen) {
    case (a: String, b: String, c: String) =>
      val biga = new BigInt2(a)
      val bigb = new BigInt2(b)
      val bigc = new BigInt2(c)
      val bigintegera = new BigInteger(a)
      val bigintegerb = new BigInteger(b)
      val bigintegerc = new BigInteger(c)
      val res0 = biga + (bigb + bigc)
      val res1 = bigintegera.add( bigintegerb).add(bigintegerc)
      res0.toString == res1.toString && res0.toString == ((biga + bigb) + bigc).toString
  }

  property("a * (b + c) = (a * b) + (a * c)") = forAll(BigIntGen, BigIntGen, BigIntGen) {
    case (a: String, b: String, c: String) =>
      val biga = new BigInt2(a)
      val bigb = new BigInt2(b)
      val bigc = new BigInt2(c)
      val bigintegera = new BigInteger(a)
      val bigintegerb = new BigInteger(b)
      val bigintegerc = new BigInteger(c)
      val res0 = biga * (bigb + bigc)
      val res1 = bigintegera.multiply(bigintegerb.add(bigintegerc))
      res0.toString == res1.toString && res0.toString == ((biga * bigb) + (biga * bigc)).toString
  }

  property("a + 0 = a") = forAll(BigIntGen) {
    case (a: String) =>
      val biga = new BigInt2(a)
      val bigintegera = new BigInteger(a)
      val res0 = biga + BigInt2.ZERO
      val res1 = bigintegera.add(BigInteger.ZERO)
      res0.toString == res1.toString
  }

  property("a + (-a) = 0") = forAll(BigIntGen) {
    case (a: String) =>
      val biga = new BigInt2(a)
      val bigintegera = new BigInteger(a)
      val res0 = biga + biga.negate
      val res1 = bigintegera.add(bigintegera.negate())
      res0.toString == res1.toString
  }

  property("a - b = a + (-b)") = forAll(BigIntGen, BigIntGen) {
    case (a: String, b: String) =>
      val biga = new BigInt2(a)
      val bigb = new BigInt2(b)
      val bigintegera = new BigInteger(a)
      val bigintegerb = new BigInteger(b)
      val res0 = (biga - bigb)
      val res1 = bigintegera.subtract( bigintegerb)
      res0.toString == res1.toString && res0.toString == (biga + bigb.negate).toString
  }

  property("a * 0 = 0") = forAll(BigIntGen) {
    case (a: String) =>
      val biga = new BigInt2(a)
      val bigintegera = new BigInteger(a)
      val res0 = biga * BigInt2.ZERO
      val res1 = bigintegera.multiply(BigInteger.ZERO)
      res0.toString == res1.toString
  }

  property("a * 1 = a") = forAll(BigIntGen) {
    case (a: String) =>
      val biga = new BigInt2(a)
      val bigintegera = new BigInteger(a)
      val res0 = biga * BigInt2.ONE
      val res1 = bigintegera.multiply(BigInteger.ONE)
      res0.toString == res1.toString && res0.toString == biga.toString
  }

  property("a * b == b * a") = forAll(BigIntGen, BigIntGen) {
    case (a: String, b: String) =>
      val biga = new BigInt2(a)
      val bigb = new BigInt2(b)
      val bigintegera = new BigInteger(a)
      val bigintegerb = new BigInteger(b)
      val res0 = (biga * bigb)
      val res1 = bigintegera.multiply( bigintegerb)
      res0.toString == res1.toString && res0.toString == (bigb * biga).toString
  }

  // Below Test's should take care of divide by 0 :P
  /*property("a / a = 1") = forAll(BigIntGen) {
    case (a: String) =>
      val biga = new BigInt2(a)
      val bigintegera = new BigInteger(a)
      val res0 = biga / biga
      val res1 = bigintegera.divide(bigintegera)
      res0.toString == res1.toString && res0.toString == BigInteger.ONE.toString
  }

  property("(a * b) / b = a") = forAll(BigIntGen, BigIntGen) {
    case (a: String, b: String) =>
      val biga = new BigInt2(a)
      val bigb = new BigInt2(b)
      val bigintegera = new BigInteger(a)
      val bigintegerb = new BigInteger(b)
      val res0 = (biga * bigb) / bigb
      val res1 = (bigintegera.multiply( bigintegerb)).divide(bigintegerb)
      res0.toString == res1.toString
  }*/

}

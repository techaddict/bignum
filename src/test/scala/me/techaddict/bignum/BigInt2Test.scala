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

object BigInt2Test extends Properties("BigInteger") {

  def BigNumGen = for (n <- Gen.numStr if (n != "")) yield n

  property("(a+b) == (b+a)") = forAll(BigNumGen, BigNumGen) {
    case (a: String, b: String) =>
      val biga = new BigInt2(a)
      val bigb = new BigInt2(b)
      val bigintegera = new BigInteger(a)
      val bigintegerb = new BigInteger(b)
      val res0 = (biga + bigb)
      val res1 = bigintegera.add( bigintegerb)
      res0.toString == res1.toString && res0.toString == (bigb + biga).toString
  }

  property("a + (b + c) = (a + b) + c") = forAll(BigNumGen, BigNumGen, BigNumGen) {
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

  property("a * (b + c) = (a * b) + (a * c)") = forAll(BigNumGen, BigNumGen, BigNumGen) {
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

  property("a + 0 = a") = forAll(BigNumGen) {
    case (a: String) =>
      val biga = new BigInt2(a)
      val bigintegera = new BigInteger(a)
      val res0 = biga + BigInt2.ZERO
      val res1 = bigintegera.add(BigInteger.ZERO)
      res0.toString == res1.toString
  }

  property("a + (-a) = 0") = forAll(BigNumGen) {
    case (a: String) =>
      val biga = new BigInt2(a)
      val bigintegera = new BigInteger(a)
      val res0 = biga + biga.negate
      val res1 = bigintegera.add(bigintegera.negate())
      res0.toString == res1.toString
  }

}

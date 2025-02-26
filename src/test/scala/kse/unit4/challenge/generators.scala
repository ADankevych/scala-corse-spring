package kse.unit4.challenge

import kse.unit4.challenge.numerals.*
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object generators:

  lazy val genZero: Gen[Zero] = Gen.const(Zero)

  lazy val genSuccessor: Gen[Successor] = ???

  lazy val genNumeral: Gen[Numeral] =
    Gen.frequency(1 -> lzy(genZero), 1 -> lzy(genSuccessor))

  given Arbitrary[Zero]      = ???
  given Arbitrary[Successor] = ???
  given Arbitrary[Numeral]   = ???

package kse.unit4.challenge

import kse.unit4.challenge.numerals.*
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object generators:

  lazy val genZero: Gen[Zero] = Gen.const(Zero)

  lazy val genSuccessor: Gen[Successor] = Gen.lzy(genNumeral).map(Successor(_))

  lazy val genNumeral: Gen[Numeral] =
    Gen.frequency(1 -> lzy(genZero), 1 -> lzy(genSuccessor))

  given Arbitrary[Zero]      = Arbitrary(genZero)
  given Arbitrary[Successor] = Arbitrary(genSuccessor)
  given Arbitrary[Numeral]   = Arbitrary(genNumeral)

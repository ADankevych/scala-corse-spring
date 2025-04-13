package kse.unit6.topic

import org.scalacheck.*

object generators:
  lazy val genSmallPositiveNumber: Gen[BigInt] = Gen.chooseNum(1, 5)

  given Arbitrary[BigInt] = Arbitrary(genSmallPositiveNumber)

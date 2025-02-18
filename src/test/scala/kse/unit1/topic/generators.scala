package kse.unit1.topic

import kse.unit1.topic.functions.Number
import org.scalacheck.*

object generators:
  lazy val genSmallPositiveNumber: Gen[Number]    = Gen.chooseNum(1, 5)
  lazy val genSmallNegativeNumber: Gen[Number]    = Gen.chooseNum(-5, -1)
  lazy val genSmallNonNegativeNumber: Gen[Number] = Gen.chooseNum(0, 5)
  lazy val genSmallNumber: Gen[Number]            = Gen.chooseNum(-5, 5)

  lazy val genNonNegativeNumberRange: Gen[(Number, Number)] =
    for
      start  <- genSmallNonNegativeNumber
      length <- genSmallPositiveNumber
    yield (start, start + length)

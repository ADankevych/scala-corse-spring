package kse.unit1.challenge

import kse.unit1.challenge.arithmetic.Number
import org.scalacheck.*

object generators:
  lazy val genSmallNonNegativeNumber: Gen[Number] = Gen.chooseNum(0, 8)
  lazy val genSmallNumber: Gen[Number]            = Gen.chooseNum(-8, 8)

package kse.unit2.challenge

import booleans.*
import org.scalacheck.*

object generators:
  lazy val genBoolean: Gen[Boolean]              = Gen.oneOf(True, False)
  lazy val genListOfBooleans: Gen[List[Boolean]] = Gen.nonEmptyListOf(genBoolean)

  given Arbitrary[Boolean]       = Arbitrary(genBoolean)
  given Arbitrary[List[Boolean]] = Arbitrary(genListOfBooleans)

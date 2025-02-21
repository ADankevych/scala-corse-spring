package kse.unit2.challenge

import kse.unit2.challenge.booleans.*
import org.scalacheck.*

object generators:
  lazy val genBoolean: Gen[Boolean]              = Gen.oneOf(???, ???)
  lazy val genListOfBooleans: Gen[List[Boolean]] = Gen.nonEmptyListOf(genBoolean)

  given Arbitrary[Boolean]       = Arbitrary(genBoolean)
  given Arbitrary[List[Boolean]] = Arbitrary(genListOfBooleans)

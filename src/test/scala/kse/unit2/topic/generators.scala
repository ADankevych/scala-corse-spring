package kse.unit2.topic

import kse.unit2.topic.nats.*
import org.scalacheck.*

object generators:
  lazy val genNat3: Gen[Nat3]             = Gen.oneOf(Zero, One, Two)
  lazy val genListOfNats: Gen[List[Nat3]] = Gen.nonEmptyListOf(genNat3)

  given Arbitrary[Nat3]       = Arbitrary(genNat3)
  given Arbitrary[List[Nat3]] = Arbitrary(genListOfNats)

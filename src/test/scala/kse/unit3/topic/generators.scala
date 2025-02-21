package kse.unit3.topic

import kse.unit3.topic.expressions.*
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object generators:

  val genNat3: Gen[Nat3] = Gen.oneOf(Zero, One, Two)

  val genVariableName: Gen[String] = Gen.alphaStr.suchThat(_.nonEmpty)

  val genVariable: Gen[Variable] =
    for name <- genVariableName
    yield Variable(name)

  val genAddition: Gen[Addition] =
    for
      left  <- genExpression
      right <- genExpression
    yield Addition(left, right)

  val genMultiplication: Gen[Multiplication] =
    for
      left  <- genExpression
      right <- genExpression
    yield Multiplication(left, right)

  lazy val genExpression: Gen[Expression] =
    Gen.frequency((2, lzy(genNat3)), (1, lzy(genVariable)), (1, lzy(genAddition)), (1, lzy(genMultiplication)))

  given Arbitrary[Nat3]       = Arbitrary(genNat3)
  given Arbitrary[Variable]   = Arbitrary(genVariable)
  given Arbitrary[Expression] = Arbitrary(genExpression)

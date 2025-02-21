package kse.unit3.challenge

import kse.unit3.challenge.expressions.*
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object generators:

  val genBoolean: Gen[Boolean] = ???

  val genVariableName: Gen[String] = Gen.alphaStr.suchThat(_.nonEmpty)

  val genVariable: Gen[Variable] =
    for name <- genVariableName
    yield Variable(name)

  val genNegation: Gen[Negation] = ???

  val genConjunction: Gen[Conjunction] = ???

  val genDisjunction: Gen[Disjunction] = ???

  val genImplication: Gen[Implication] = ???

  val genEquivalence: Gen[Equivalence] = ???

  lazy val genExpression: Gen[Expression] =
    Gen.frequency(
      3 -> lzy(genBoolean),
      2 -> lzy(genVariable),
      1 -> lzy(genNegation),
      1 -> lzy(genConjunction),
      1 -> lzy(genDisjunction),
      1 -> lzy(genImplication),
      1 -> lzy(genEquivalence),
    )

  given Arbitrary[Boolean]    = ???
  given Arbitrary[Variable]   = ???
  given Arbitrary[Expression] = ???

package kse.unit3.challenge

import kse.unit3.challenge.expressions.*
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object generators:

  val genBoolean: Gen[Boolean] = Gen.oneOf(True, False)

  val genVariableName: Gen[String] = Gen.alphaStr.suchThat(_.nonEmpty)

  val genVariable: Gen[Variable] =
    for name <- genVariableName
    yield Variable(name)

  val genNegation: Gen[Negation] =
    for expr <- lzy(genExpression)
    yield Negation(expr)

  val genConjunction: Gen[Conjunction] =
    for
      left  <- lzy(genExpression)
      right <- lzy(genExpression)
    yield Conjunction(left, right)

  val genDisjunction: Gen[Disjunction] =
    for
      left  <- lzy(genExpression)
      right <- lzy(genExpression)
    yield Disjunction(left, right)

  val genImplication: Gen[Implication] =
    for
      left  <- lzy(genExpression)
      right <- lzy(genExpression)
    yield Implication(left, right)

  val genEquivalence: Gen[Equivalence] =
    for
      left  <- lzy(genExpression)
      right <- lzy(genExpression)
    yield Equivalence(left, right)

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

  given Arbitrary[Boolean]    = Arbitrary(genBoolean)
  given Arbitrary[Variable]   = Arbitrary(genVariable)
  given Arbitrary[Expression] = Arbitrary(genExpression)

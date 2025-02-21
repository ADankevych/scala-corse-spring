package kse.unit3.topic

import scala.annotation.targetName

object expressions:

  sealed trait Expression:
    def evaluate: Expression
    def substitute(variable: Variable, expression: Expression): Expression

  sealed trait Nat3 extends Expression:
    val evaluate: Expression                                               = this
    def substitute(variable: Variable, expression: Expression): Expression = this

  type Zero = Zero.type
  case object Zero extends Nat3

  type One = One.type
  case object One extends Nat3

  type Two = Two.type
  case object Two extends Nat3

  case class Variable(name: String) extends Expression:
    val evaluate: Expression = this

    def substitute(variable: Variable, substitution: Expression): Expression =
      if this == variable then substitution
      else this

  case class Addition(left: Expression, right: Expression) extends Expression:

    lazy val evaluate: Expression =
      (left.evaluate, right.evaluate) match
        case (Zero, value)           => value
        case (value, Zero)           => value
        case (One, One)              => Two
        case (One, Two) | (Two, One) => Zero
        case (Two, Two)              => One
        case (left, right)           => Addition(left, right)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Addition(left.substitute(variable, substitution), right.substitute(variable, substitution))

    override def toString: String = s"($left + $right)"

  case class Multiplication(left: Expression, right: Expression) extends Expression:

    lazy val evaluate: Expression =
      (left.evaluate, right.evaluate) match
        case (Zero, _) | (_, Zero) => Zero
        case (One, value)          => value
        case (value, One)          => value
        case (Two, Two)            => One
        case (left, right)         => Multiplication(left, right)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Multiplication(left.substitute(variable, substitution), right.substitute(variable, substitution))

    override def toString: String = s"($left * $right)"

  given Conversion[String, Variable] with
    def apply(str: String): Variable = Variable(str)

  extension (expr: Expression)

    @targetName("addition")
    infix def |+|(that: Expression): Addition = Addition(expr, that)

    @targetName("multiplication")
    infix def |*|(that: Expression): Multiplication = Multiplication(expr, that)

package kse.unit3.challenge

import scala.annotation.{tailrec, targetName}

object expressions:

  sealed trait Expression:
    def evaluate: Expression
    def substitute(variable: Variable, expression: Expression): Expression

  sealed trait Boolean extends Expression:
    val evaluate: Expression                                                 = ???
    def substitute(variable: Variable, substitution: Expression): Expression = ???

  type True = True.type
  case object True extends Boolean

  type False = False.type
  case object False extends Boolean

  case class Variable(name: String) extends Expression:
    val evaluate: Expression                                               = ???
    def substitute(variable: Variable, expression: Expression): Expression = ???

  case class Negation(expression: Expression) extends Expression:
    def evaluate: Expression                                                 = ???
    def substitute(variable: Variable, substitution: Expression): Expression = ???
    override def toString: String                                            = ???

  case class Conjunction(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression                                                 = ???
    def substitute(variable: Variable, substitution: Expression): Expression = ???
    override def toString: String                                            = ???

  case class Disjunction(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression                                                 = ???
    def substitute(variable: Variable, substitution: Expression): Expression = ???
    override def toString: String                                            = ???

  case class Implication(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression                                                 = ???
    def substitute(variable: Variable, substitution: Expression): Expression = ???
    override def toString: String                                            = ???

  case class Equivalence(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression                                                 = ???
    def substitute(variable: Variable, substitution: Expression): Expression = ???
    override def toString: String                                            = ???

  given Conversion[String, Variable] with
    def apply(str: String): Variable = Variable(str)

  extension (expr: Expression)

    @targetName("negation")
    infix def unary_! : Negation = ???

    @targetName("conjunction")
    infix def ∧(that: Expression): Conjunction = ???

    @targetName("disjunction")
    infix def ∨(that: Expression): Disjunction = ???

    @targetName("implication")
    infix def →(that: Expression): Implication = ???

    @targetName("equivalence")
    infix def ↔(that: Expression): Equivalence = ???

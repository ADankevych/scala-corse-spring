package kse.unit3.challenge

import kse.unit3.challenge.expressions.*
import kse.unit3.challenge.generators.given
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}

object BooleansSpecification extends Properties("Expressions"):

  include(BooleanEvaluationSpecification)
  include(VariableEvaluationSpecification)
  include(NegationEvaluationSpecification)
  include(ConjunctionEvaluationSpecification)
  include(DisjunctionEvaluationSpecification)
  include(ImplicationEvaluationSpecification)
  include(EquivalenceEvaluationSpecification)
  include(BooleanSubstitutionSpecification)
  include(VariableSubstitutionSpecification)
  include(ExpressionSubstitutionSpecification)

end BooleansSpecification

object BooleanEvaluationSpecification extends Properties("Boolean Evaluation"):

  property("boolean value should be evaluated to itself") = forAll: (boolean: Boolean) =>
    boolean.evaluate == boolean

end BooleanEvaluationSpecification

object VariableEvaluationSpecification extends Properties("Variable Evaluation"):

  property("variable should be evaluated to itself") = ???

end VariableEvaluationSpecification

object NegationEvaluationSpecification extends Properties("Negation Evaluation"):

  property("!True should be evaluated to False") = ???

  property("!False should be evaluated to True") = ???

  property("!variable should be evaluated to !variable") = ???

  property("!expression should be correctly evaluated") = forAll: (expression: Expression) =>
    (!expression).evaluate == (!expression.evaluate).evaluate

end NegationEvaluationSpecification

object ConjunctionEvaluationSpecification extends Properties("Conjunction Evaluation"):

  property("True ∧ expression should be evaluated to expression evaluation") = ???

  property("expression ∧ True should be evaluated to expression evaluation") = ???

  property("False ∧ expression should be evaluated to False") = ???

  property("expression ∧ False should be evaluated to False") = ???

  property("left ∧ right should be correctly evaluated") = ???

end ConjunctionEvaluationSpecification

object DisjunctionEvaluationSpecification extends Properties("Disjunction Evaluation"):

  property("True ∨ expression should be evaluated to True") = ???

  property("expression ∨ True should be evaluated to True") = ???

  property("False ∨ expression should be evaluated to expression evaluation") = ???

  property("expression ∨ False should be evaluated to expression evaluation") = ???

  property("left ∨ right should be correctly evaluated") = ???

end DisjunctionEvaluationSpecification

object ImplicationEvaluationSpecification extends Properties("Implication Evaluation"):

  property("True → expression should be evaluated to expression evaluation") = ???

  property("False → expression should be evaluated to True") = ???

  property("left → right should be correctly evaluated") = ???

end ImplicationEvaluationSpecification

object EquivalenceEvaluationSpecification extends Properties("Equivalence Evaluation"):

  property("Reflexivity") = ???

  property("Symmetry") = ???

  property("Transitivity") = ???

  property("left ↔ right should be correctly evaluated") = ???

end EquivalenceEvaluationSpecification

object BooleanSubstitutionSpecification extends Properties("Boolean Substitution"):

  property("substitution into boolean should make no changes") = ???

end BooleanSubstitutionSpecification

object VariableSubstitutionSpecification extends Properties("Variable Substitution"):

  property("substitution into different variable should make no changes") = forAll: (v1: Variable, v2: Variable, substitution: Expression) =>
    v1 != v2 ==> {
      v1.substitute(v2, substitution) == v1
    }

  property("substitution into the same variable should return the given expression") = ???

end VariableSubstitutionSpecification

object ExpressionSubstitutionSpecification extends Properties("Expression Substitution"):

  property("substitution into !expression should be equal to !(substitution into expression)") = ???

  property("substitution into left ∧ right should be equal to substitution into left ∧ substitution into right") = ???

  property("substitution into left ∨ right should be equal to substitution into left ∨ substitution into right") = ???

  property("substitution into left → right should be equal to substitution into left → substitution into right") = ???

  property("substitution into left ↔ right should be equal to substitution into left ↔ substitution into right") = ???

end ExpressionSubstitutionSpecification

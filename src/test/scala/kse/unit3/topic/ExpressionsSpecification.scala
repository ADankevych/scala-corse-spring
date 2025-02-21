package kse.unit3.topic

import kse.unit3.topic.expressions.*
import kse.unit3.topic.generators.given
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}

object ExpressionsSpecification extends Properties("Expressions"):

  include(Nat3AdditionSpecification)
  include(Nat3MultiplicationSpecification)
  include(ExpressionAdditionSpecification)
  include(ExpressionMultiplicationSpecification)
  include(Nat3SubstitutionSpecification)
  include(VariableSubstitutionSpecification)
  include(ExpressionSubstitutionSpecification)

end ExpressionsSpecification

object Nat3AdditionSpecification extends Properties("Nat3 Addition"):

  property("Zero |+| Zero must be Zero") = propBoolean:
    (Zero |+| Zero).evaluate == Zero

  property("Zero |+| value must be value") = forAll: (value: Nat3) =>
    (Zero |+| value).evaluate == value

  property("value |+| Zero must be value") = forAll: (value: Nat3) =>
    (value |+| Zero).evaluate == value

  property("One |+| One must be Two") = propBoolean:
    (One |+| One).evaluate == Two

  property("One |+| Two must be Zero") = propBoolean:
    (One |+| Two).evaluate == Zero

  property("Two |+| One must be Zero") = propBoolean:
    (Two |+| One).evaluate == Zero

  property("Two |+| Two must be One") = propBoolean:
    (Two |+| Two).evaluate == One

  property("left |+| right must be correctly evaluated") = forAll: (left: Nat3, right: Nat3) =>
    (left |+| right).evaluate == (left.evaluate |+| right.evaluate).evaluate

end Nat3AdditionSpecification

object Nat3MultiplicationSpecification extends Properties("Nat3 Multiplication"):

  property("Zero |*| value must be Zero") = forAll: (value: Nat3) =>
    (Zero |*| value).evaluate == Zero

  property("value |*| Zero must be Zero") = forAll: (value: Nat3) =>
    (value |*| Zero).evaluate == Zero

  property("One |*| value must be value") = forAll: (value: Nat3) =>
    (One |*| value).evaluate == value

  property("value |*| One must be value") = forAll: (value: Nat3) =>
    (value |*| One).evaluate == value

  property("Two |*| Two must be One") = propBoolean:
    (Two |*| Two).evaluate == One

  property("left |*| right must be correctly evaluated") = forAll: (left: Nat3, right: Nat3) =>
    (left |*| right).evaluate == (left.evaluate |*| right.evaluate).evaluate

end Nat3MultiplicationSpecification

object ExpressionAdditionSpecification extends Properties("Expression Addition"):

  property("Zero |+| expression must be value") = forAll: (expression: Expression) =>
    (Zero |+| expression).evaluate == expression.evaluate

  property("expression |+| Zero must be value") = forAll: (expression: Expression) =>
    (expression |+| Zero).evaluate == expression.evaluate

  property("left |+| right must be correctly evaluated") = forAll: (left: Expression, right: Expression) =>
    (left |+| right).evaluate == (left.evaluate |+| right.evaluate).evaluate

end ExpressionAdditionSpecification

object ExpressionMultiplicationSpecification extends Properties("Expression Multiplication"):

  property("Zero |*| expression must be Zero") = forAll: (expression: Expression) =>
    (Zero |*| expression).evaluate == Zero

  property("expression |*| Zero must be Zero") = forAll: (expression: Expression) =>
    (expression |*| Zero).evaluate == Zero

  property("left |*| right must be correctly evaluated") = forAll: (left: Expression, right: Expression) =>
    (left |*| right).evaluate == (left.evaluate |*| right.evaluate).evaluate

end ExpressionMultiplicationSpecification

object Nat3SubstitutionSpecification extends Properties("Nat3 Substitution"):

  property("substitution into Nat should make no changes") = forAll: (nat3: Nat3, variable: Variable, expression: Expression) =>
    nat3.substitute(variable, expression) == nat3

end Nat3SubstitutionSpecification

object VariableSubstitutionSpecification extends Properties("Variable Substitution"):

  property("substitution into different variable should make no changes") = forAll: (v1: Variable, v2: Variable, expression: Expression) =>
    v1 != v2 ==> {
      v1.substitute(v2, expression) == v1
    }

  property("substitution into the same variable should return the given expression") = forAll: (variable: Variable, expression: Expression) =>
    variable.substitute(variable, expression) == expression

end VariableSubstitutionSpecification

object ExpressionSubstitutionSpecification extends Properties("Expression Substitution"):

  property("substitution into left |+| right should be equal to substitution into left |+| substitution into right") = forAll:
    (left: Expression, right: Expression, variable: Variable, expression: Expression) =>
      (left |+| right).substitute(variable, expression) == (left.substitute(variable, expression) |+| right.substitute(variable, expression))

  property("substitution into left |*| right should be equal to substitution into left |*| substitution into right") = forAll:
    (left: Expression, right: Expression, variable: Variable, expression: Expression) =>
      (left |*| right).substitute(variable, expression) == (left.substitute(variable, expression) |*| right.substitute(variable, expression))

end ExpressionSubstitutionSpecification

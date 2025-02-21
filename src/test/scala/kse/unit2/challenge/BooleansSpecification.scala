package kse.unit2.challenge

import kse.model.DummyError
import kse.unit2.challenge.booleans.*
import kse.unit2.challenge.generators.given
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}

object BooleansSpecification extends Properties("Booleans"):

  include(NegationSpecification)
  include(ConjunctionSpecification)
  include(DisjunctionSpecification)
  include(ImplicationSpecification)
  include(EquivalenceSpecification)
  include(AxiomsSpecification)
  include(FoldSpecification)

end BooleansSpecification

object NegationSpecification extends Properties("Negation"):

  property("!True is False") = propBoolean:
    ???

  property("!False is True") = propBoolean:
    ???

end NegationSpecification

object ConjunctionSpecification extends Properties("Conjunction"):

  property("True ∧ value is value") = forAll: (value: Boolean) =>
    ???

  property("False ∧ value is False") = forAll: (value: Boolean) =>
    ???

  property("False ∧ value should ignore the second argument") = forAll: (value: Boolean) =>
    ???

end ConjunctionSpecification

object DisjunctionSpecification extends Properties("Disjunction"):

  property("True ∨ value is True") = forAll: (value: Boolean) =>
    ???

  property("True ∨ value should ignore the second argument") = forAll: (value: Boolean) =>
    ???

  property("False ∨ value is value") = forAll: (value: Boolean) =>
    ???

end DisjunctionSpecification

object ImplicationSpecification extends Properties("Implication"):

  property("True → value is value") = forAll: (value: Boolean) =>
    ???

  property("False → value is True") = forAll: (value: Boolean) =>
    ???

  property("False → value should ignore the second argument") = forAll: (value: Boolean) =>
    ???

end ImplicationSpecification

// Optional challenge
object EquivalenceSpecification extends Properties("Equivalence"):

end EquivalenceSpecification

object AxiomsSpecification extends Properties("Axioms"):

  property("a → (b → a)") = ???

  property("(a → (b → c)) → ((a → b) → (a → c))") = ???

  property("(a ∧ b) → a") = ???

  property("(a ∧ b) → b") = ???

  property("a → (b → (a ∧ b))") = ???

  property("a → (a ∨ b)") = ???

  property("b → (a ∨ b)") = ???

  property("(a → c) → ((b → c) → ((a ∨ b) → c))") = ???

  property("!a → (a → b)") = ???

  property("(a → b) → ((a → !b) → !a)") = ???

  property("a ∨ !a") = ???

end AxiomsSpecification

object FoldSpecification extends Properties("Fold"):

  property("Conjunction of all elements from the empty list should be True") = propBoolean:
    Nil.conjunction == True

  property("Conjunction of all elements from non-empty list should be correctly evaluated") = forAll: (booleans: List[Boolean]) =>
    booleans.conjunction == booleans.foldRight[Boolean](???)((value, acc) => value ∧ acc)

  property("Disjunction of all elements from the empty list should be False") = propBoolean:
    Nil.disjunction == False

  property("Disjunction of all elements from non-empty list should be correctly evaluated") = forAll: (booleans: List[Boolean]) =>
    booleans.disjunction == booleans.foldRight[Boolean](???)((value, acc) => value ∨ acc)

end FoldSpecification

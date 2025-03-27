package kse.unit6.challenge

import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.Numeral
import kse.unit6.algebra.{*, given}
import kse.unit6.challenge.generators.given
import kse.unit6.challenge.order.{*, given}
import kse.unit6.challenge.set.*
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object GeneralSetSpecification extends Properties("Set"):

  include(EmptySpecification)
  include(NonEmptySpecification)
  include(SetSpecification)

end GeneralSetSpecification

object EmptySpecification extends Properties("Empty set laws"):

  property("Universal quantifier applied to the empty set should return true for any predicate") = propBoolean:
    Empty.forAll(_ => false)

  property("Existential quantifier applied to the empty set should return false for any predicate") = propBoolean:
    !Empty.exists(_ => true)

  property("Empty set should not contain any element") = forAll: (numeral: Numeral) =>
    !Empty.contains(numeral)
end EmptySpecification

object NonEmptySpecification extends Properties("Non-empty set laws"):

  property("Universal quantifier applied to an non-empty set should return true for a tautology") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    nonEmpty.forAll(_ => true)

  property("Universal quantifier applied to an non-empty set should return false for a contradiction") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    !nonEmpty.forAll(_ => false)

  property("Existential quantifier applied to an non-empty set should return true for a tautology") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    nonEmpty.exists(_ => true)

  property("Existential quantifier applied to an non-empty set should return false for a contradiction") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    !nonEmpty.exists(_ => false)

  property("Single-element set should contain an element") = forAll: (numeral: Numeral) =>
    NonEmpty(Empty, numeral, Empty).contains(numeral)

end NonEmptySpecification

object SetSpecification extends Properties("Set laws"):

  property("Universal quantifier applied to a set should return true for a tautology") = forAll: (set: Set[Numeral]) =>
    set.forAll(_ => true)

  property("Any set should contain its elements") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    set.contains(numeral) == set.exists(_ == numeral)

  property("If a set does not contain a given element then all elements of the set should not be equal to the given element") = forAll:
    (set: Set[Numeral], numeral: Numeral) => !set.contains(numeral) ==> set.forAll(_ != numeral)

  property("If a set does not contain a given element then there is no element in the set equal to the given element") = forAll:
    (set: Set[Numeral], numeral: Numeral) => !set.contains(numeral) ==> !set.exists(_ == numeral)

  property("If a given element is added to a set then there is an element in the set equals to the give element") = forAll:
    (set: Set[Numeral], numeral: Numeral) => (set.include(numeral)).contains(numeral)

  property("If a given element is added to a set then set should include the given element") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    (set.include(numeral)).contains(numeral)

  property("Inclusion should be idempotent") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    set.include(numeral).include(numeral) == set.include(numeral)

  property("If a given element is removed from a set then all elements of the set should not be equals to the given element") = forAll:
    (set: Set[Numeral], numeral: Numeral) => !set.contains(numeral) ==> set.forAll(_ != numeral)

  property("If a given element is removed from a set then the set should not contain the given element") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    !set.remove(numeral).contains(numeral)

  // Optional
  // Uncomment if needed
//  property("If a given new element is added to a set and then removed from the set then the set should not be changed") = forAll:
//    (set: Set[Numeral], numeral: Numeral) => set.include(numeral).remove(numeral) == set

//  property("Removal should be idempotent") = forAll: (set: Set[Numeral], numeral: Numeral) =>
//    set.remove(numeral).remove(numeral) == set

  property("A union of two given sets should contain the elements from both sets") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
    (left ∪ right).forAll(element => left.contains(element) || right.contains(element))

  property("An intersections of two given sets should contain the same elements from both sets") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
    (left ∩ right).forAll(element => left.contains(element) && right.contains(element))

  property("A difference of two given sets should contain elements from a base set but should not contain elements from an exclusion set") = forAll:
    (left: Set[Numeral], right: Set[Numeral]) => (left \ right).forAll(element => left.contains(element) && !right.contains(element))

  property("A disjunctive union of two given sets should include elements from a union of the sets but exclude elements from an intersection of the sets") =
    forAll: (left: Set[Numeral], right: Set[Numeral]) =>
      val union               = left ∪ right
      val intersection        = left ∩ right
      val symmetricDifference = left ∆ right
      union.forAll(element => union.contains(element) && !intersection.contains(element))
      symmetricDifference.forAll(element => union.contains(element) && !intersection.contains(element))

  property("Union left unit") = forAll: (set: Set[Numeral]) =>
    (Empty ∪ set) == set

  property("Union right unit") = forAll: (set: Set[Numeral]) =>
    (set ∪ Empty) == set

  property("Intersection left zero") = forAll: (set: Set[Numeral]) =>
    (Empty ∩ set) == Empty

  property("Intersection right zero") = forAll: (set: Set[Numeral]) =>
    (set ∩ Empty) == Empty

  property("Difference left zero") = forAll: (set: Set[Numeral]) =>
    (Empty \ set) == Empty

  property("Difference right unit") = forAll: (set: Set[Numeral]) =>
    (set \ Empty) == set

  property("Symmetric difference left unit") = forAll: (set: Set[Numeral]) =>
    (Empty ∆ set) == set

  property("Symmetric difference right unit") = forAll: (set: Set[Numeral]) =>
    (set ∆ Empty) == set

  property("Union should be idempotent") = forAll: (set: Set[Numeral]) =>
    set ∪ set == set

  property("Intersection should be idempotent") = forAll: (set: Set[Numeral]) =>
    set ∩ set == set

  property("Difference should be self-inverted") = forAll: (set: Set[Numeral]) =>
    set \ set == Empty

//  property("Union should be commutative") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
//    left ∪ right == right ∪ left

//  property("Intersection should be commutative") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
//    left ∩ right == right ∩ left

  property("Union should be associative") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
    (a ∪ b) ∪ c == a ∪ (b ∪ c)

  property("Intersection should be associative") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
    (a ∩ b) ∩ c == a ∩ (b ∩ c)

//  property("Union should be distributive over intersection") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
//    (a ∪ b) ∩ c == (a ∩ c) ∪ (b ∩ c)

  property("Intersection should be distributive over union") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
    (a ∩ b) ∪ c == (a ∪ c) ∩ (b ∪ c)

  // Optional
  // Uncomment if needed
//  property("Difference should be distributive over intersection") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
//    (a ∩ b) \ c == (a \ c) ∪ (b \ c)

end SetSpecification

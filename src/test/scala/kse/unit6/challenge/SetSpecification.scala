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

  property("Universal quantifier applied to the empty set should return true for any predicate") = ???

  property("Existential quantifier applied to the empty set should return false for any predicate") = ???

  property("Empty set should not contain any element") = forAll: (numeral: Numeral) =>
    ???

end EmptySpecification

object NonEmptySpecification extends Properties("Non-empty set laws"):

  property("Universal quantifier applied to an non-empty set should return true for a tautology") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    ???

  property("Universal quantifier applied to an non-empty set should return false for a contradiction") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    ???

  property("Existential quantifier applied to an non-empty set should return true for a tautology") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    ???

  property("Existential quantifier applied to an non-empty set should return false for a contradiction") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    ???

  property("Single-element set should contain an element") = forAll: (numeral: Numeral) =>
    ???

end NonEmptySpecification

object SetSpecification extends Properties("Set laws"):

  property("Universal quantifier applied to a set should return true for a tautology") = forAll: (set: Set[Numeral]) =>
    ???

  property("Any set should contain its elements") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    ???

  property("If a set does not contain a given element then all elements of the set should not be equal to the given element") = forAll:
    (set: Set[Numeral], numeral: Numeral) => ???

  property("If a set does not contain a given element then there is no element in the set equal to the given element") = forAll:
    (set: Set[Numeral], numeral: Numeral) => ???

  property("If a given element is added to a set then there is an element in the set equals to the give element") = forAll:
    (set: Set[Numeral], numeral: Numeral) => ???

  property("If a given element is added to a set then set should include the given element") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    ???

  property("Inclusion should be idempotent") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    ???

  // Optional
  // Uncomment if needed
//  property("If a given element is removed from a set then all elements of the set should not be equals to the given element") = forAll:
//    (set: Set[Numeral], numeral: Numeral) => ???

  // Optional
  // Uncomment if needed
//  property("If a given element is removed from a set then the set should not contain the given element") = forAll: (set: Set[Numeral], numeral: Numeral) =>
//    ???

  // Optional
  // Uncomment if needed
//  property("If a given new element is added to a set and then removed from the set then the set should not be changed") = forAll:
//    (set: Set[Numeral], numeral: Numeral) => ???

  property("Removal should be idempotent") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    ???

  property("A union of two given sets should contain the elements from both sets") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
    ???

  property("An intersections of two given sets should contain the same elements from both sets") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
    ???

  // Optional.
  // Uncomment if needed
//  property("A difference of two given sets should contain elements from a base set but should not contain elements from an exclusion set") = forAll:
//    (left: Set[Numeral], right: Set[Numeral]) => ???

  // Optional.
  // Uncomment if needed
//  property("A disjunctive union of two given sets should include elements from a union of the sets but exclude elements from an intersection of the sets") =
//    forAll: (left: Set[Numeral], right: Set[Numeral]) =>
//      ???

  // TODO: The next part should be implemented by students
  property("Union left unit") = forAll: (set: Set[Numeral]) =>
    ???

  property("Union right unit") = forAll: (set: Set[Numeral]) =>
    ???

  property("Intersection left zero") = forAll: (set: Set[Numeral]) =>
    ???

  property("Intersection right zero") = forAll: (set: Set[Numeral]) =>
    ???

  // Optional
  // Uncomment if needed
//  property("Difference left zero") = forAll: (set: Set[Numeral]) =>
//    ???

  // Optional
  // Uncomment if needed
//  property("Difference right unit") = forAll: (set: Set[Numeral]) =>
//    ???

  // Optional
  // Uncomment if needed
//  property("Symmetric difference left unit") = forAll: (set: Set[Numeral]) =>
//    ???

  // Optional
  // Uncomment if needed
//  property("Symmetric difference right unit") = forAll: (set: Set[Numeral]) =>
//    ???

  property("Union should be idempotent") = forAll: (set: Set[Numeral]) =>
    ???

  property("Intersection should be idempotent") = forAll: (set: Set[Numeral]) =>
    ???

  // Optional
  // Uncomment if needed
//  property("Difference should be self-inverted") = forAll: (set: Set[Numeral]) =>
//    ???

  property("Union should be commutative") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
    ???

  property("Intersection should be commutative") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
    ???

  property("Union should be associative") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
    ???

  property("Intersection should be associative") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
    ???

  property("Union should be distributive over intersection") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
    ???

  property("Intersection should be distributive over union") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
    ???

  // Optional
  // Uncomment if needed
//  property("Difference should be distributive over intersection") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
//    ???

end SetSpecification

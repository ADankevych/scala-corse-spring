package kse.unit5.challenge

import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.Numeral
import kse.unit5.challenge.generators.given
import kse.unit5.challenge.set.*
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object NumeralSetSpecification extends Properties("NumeralSet"):

  include(EmptySpecification)
  include(NonEmptySpecification)
  include(SetSpecification)

end NumeralSetSpecification

object EmptySpecification extends Properties("Empty set laws"):

  property("Universal quantifier applied to the empty set should return true for any predicate") = propBoolean:
    Empty.forAll(_ => false)

  property("Existential quantifier applied to the empty set should return false for any predicate") = propBoolean:
    ???

  property("Empty set should not contain any element") = forAll: (numeral: Numeral) =>
    ???

end EmptySpecification

object NonEmptySpecification extends Properties("Non-empty set laws"):

  property("Universal quantifier applied to an non-empty set should return true for a tautology") = forAll: (nonEmpty: NonEmpty) =>
    nonEmpty.forAll(_ => true)

  property("Universal quantifier applied to an non-empty set should return false for a contradiction") = forAll: (nonEmpty: NonEmpty) =>
    ???

  property("Existential quantifier applied to an non-empty set should return true for a tautology") = forAll: (nonEmpty: NonEmpty) =>
    ???

  property("Existential quantifier applied to an non-empty set should return false for a contradiction") = forAll: (nonEmpty: NonEmpty) =>
    ???

  property("Single-element set should contain an element") = forAll: (numeral: Numeral) =>
    ???

end NonEmptySpecification

object SetSpecification extends Properties("Set laws"):

  property("Universal quantifier applied to a set should return true for a tautology") = ???

  property("Any set should contain its elements") = forAll: (set: NumeralSet) =>
    ???

  property("If a set does not contain a given element then all elements of the set should not be equal to the given element") = forAll:
    (set: NumeralSet, numeral: Numeral) =>
      !set.contains(numeral) ==> {
        ???
      }

  property("If a set does not contain a given element then there is no element in the set equal to the given element") = ???

  property("If a given element is added to a set then there is an element in the set equals to the give element") = forAll:
    (set: NumeralSet, numeral: Numeral) =>
      !set.contains(numeral) ==> {
        ???
      }

  property("If a given element is added to a set then set should include the given element") = forAll: (set: NumeralSet, numeral: Numeral) =>
    !set.contains(numeral) ==> {
      ???
    }

  property("Inclusion should be idempotent") = forAll: (set: NumeralSet, numeral: Numeral) =>
    ???

  // Optional
  // Uncomment if needed
//  property("If a given element is removed from a set then all elements of the set should not be equals to the given element") = forAll:
//    (set: NumeralSet, numeral: Numeral) => ???

  // Optional
  // Uncomment if needed
//  property("If a given element is removed from a set then the set should not contain the given element") = forAll: (set: NumeralSet, numeral: Numeral) =>
//    ???

  // Optional
  // Uncomment if needed
//  property("If a given new element is added to a set and then removed from the set then the set should not be changed") = forAll:
//    (set: NumeralSet, numeral: Numeral) =>
//      !set.contains(numeral) ==> {
//        ???
//      }

  property("Removal should be idempotent") = forAll: (set: NumeralSet, numeral: Numeral) =>
    ???

  property("A union of two given sets should contain the elements from both sets") = forAll: (left: NumeralSet, right: NumeralSet) =>
    (left ∪ right).forAll(element => ???)

  property("An intersections of two given sets should contain the same elements from both sets") = forAll: (left: NumeralSet, right: NumeralSet) =>
    ???

  // Optional.
  // Uncomment if needed
//  property("A difference of two given sets should contain elements from a base set but should not contain elements from an exclusion set") = forAll:
//    (left: NumeralSet, right: NumeralSet) =>
//      val difference = left \ right
//      ???

  // Optional.
  // Uncomment if needed
//  property("A disjunctive union of two given sets should include elements from a union of the sets but exclude elements from an intersection of the sets") =
//    forAll: (left: NumeralSet, right: NumeralSet) =>
//      val symmetricDifference = left ∆ right
//      val union               = left ∪ right
//      val intersection        = left ∩ right
//
//      ???

  property("Union left unit") = forAll: (set: NumeralSet) =>
    (Empty ∪ set) == set

  property("Union right unit") = forAll: (set: NumeralSet) =>
    ???

  property("Intersection left zero") = forAll: (set: NumeralSet) =>
    ???

  property("Intersection right zero") = forAll: (set: NumeralSet) =>
    ???

  // Optional
  // Uncomment if needed
//  property("Difference left zero") = forAll: (set: NumeralSet) =>
//    ???

  // Optional
  // Uncomment if needed
//  property("Difference right unit") = forAll: (set: NumeralSet) =>
//    ???

  // Optional
  // Uncomment if needed
//  property("Symmetric difference left unit") = forAll: (set: NumeralSet) =>
//   ???

  // Optional
  // Uncomment if needed
//  property("Symmetric difference right unit") = forAll: (set: NumeralSet) =>
//    (set ∆ Empty) == set

  property("Union should be idempotent") = forAll: (set: NumeralSet) =>
    ???

  property("Intersection should be idempotent") = forAll: (set: NumeralSet) =>
    ???

  // Optional
  // Uncomment if needed
//  property("Difference should be self-inverted") = forAll: (set: NumeralSet) =>
//    ???

  property("Union should be commutative") = forAll: (left: NumeralSet, right: NumeralSet) =>
    ???

  property("Intersection should be commutative") = forAll: (left: NumeralSet, right: NumeralSet) =>
    ???

  property("Union should be associative") = forAll: (a: NumeralSet, b: NumeralSet, c: NumeralSet) =>
    ???

  property("Intersection should be associative") = forAll: (a: NumeralSet, b: NumeralSet, c: NumeralSet) =>
    ???

  property("Union should be distributive over intersection") = forAll: (a: NumeralSet, b: NumeralSet, c: NumeralSet) =>
    ???

  property("Intersection should be distributive over union") = forAll: (a: NumeralSet, b: NumeralSet, c: NumeralSet) =>
    ???

  // Optional
  // Uncomment if needed
//  property("Difference should be distributive over intersection") = ???
//

end SetSpecification

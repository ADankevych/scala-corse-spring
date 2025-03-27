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

  property("Universal quantifier applied to the empty set should return true for any predicate") = forAll: (numeral: Numeral) =>
    Empty.forAll((_: Nothing) => false)

  property("Existential quantifier applied to the empty set should return false for any predicate") = forAll: (numeral: Numeral) =>
    !Empty.exists((_: Nothing) => true)

  property("Empty set should not contain any element") = forAll: (numeral: Numeral) =>
    !Empty.contains(numeral)
end EmptySpecification

object NonEmptySpecification extends Properties("Non-empty set laws"):

  property("Universal quantifier applied to an non-empty set should return true for a tautology") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    nonEmpty.forAll((_: Numeral) => true)

  property("Universal quantifier applied to an non-empty set should return false for a contradiction") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    !nonEmpty.forAll((_: Numeral) => false)

  property("Existential quantifier applied to an non-empty set should return true for a tautology") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    nonEmpty.exists((_: Numeral) => true)

  property("Existential quantifier applied to an non-empty set should return false for a contradiction") = forAll: (nonEmpty: NonEmpty[Numeral]) =>
    !nonEmpty.exists((_: Numeral) => false)

  property("Single-element set should contain an element") = forAll: (numeral: Numeral) =>
    NonEmpty(Empty, numeral, Empty).contains(numeral)

end NonEmptySpecification

object SetSpecification extends Properties("Set laws"):

  property("Universal quantifier applied to a set should return true for a tautology") = forAll: (set: Set[Numeral]) =>
    set.forAll((_: Numeral) => true)

  property("Any set should contain its elements") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    val NewSet = set.include(numeral)
    NewSet.contains(numeral)

  property("If a set does not contain a given element then all elements of the set should not be equal to the given element") = forAll:
    (set: Set[Numeral], numeral: Numeral) => !set.contains(numeral) ==> set.forAll(elem => !summon[Order[Numeral]].compare(elem, numeral).equals(0))

  property("If a set does not contain a given element then there is no element in the set equal to the given element") = forAll:
    (set: Set[Numeral], numeral: Numeral) => !set.contains(numeral) ==> !set.exists(elem => summon[Order[Numeral]].compare(elem, numeral) == 0)

  property("If a given element is added to a set then there is an element in the set equals to the give element") = forAll:
    (set: Set[Numeral], numeral: Numeral) =>
      val NewSet = set.include(numeral)
      NewSet.exists(elem => summon[Order[Numeral]].compare(elem, numeral) == 0)

  property("If a given element is added to a set then set should include the given element") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    val NewSet = set.include(numeral)
    NewSet.contains(numeral)

  property("Inclusion should be idempotent") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    val once  = set.include(numeral)
    val twice = once.include(numeral)
    once == twice

  property("Removal should be idempotent") = forAll: (set: Set[Numeral], numeral: Numeral) =>
    true

  property("A union of two given sets should contain the elements from both sets") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
    val union = left ∪ right
    left.forAll(union.contains) && right.forAll(union.contains)

  property("An intersections of two given sets should contain the same elements from both sets") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
    val intersection = left ∩ right
    intersection.forAll(left.contains) && intersection.forAll(right.contains)

  property("Union left unit") = forAll: (set: Set[Numeral]) =>
    Empty ∪ set == set

  property("Union right unit") = forAll: (set: Set[Numeral]) =>
    set ∪ Empty == set

  property("Intersection left zero") = forAll: (set: Set[Numeral]) =>
    Empty ∩ set == Empty

  property("Intersection right zero") = forAll: (set: Set[Numeral]) =>
    set ∩ Empty == Empty

  property("Union should be idempotent") = forAll: (set: Set[Numeral]) =>
    set ∪ set == set

  property("Intersection should be idempotent") = forAll: (set: Set[Numeral]) =>
    set ∩ set == set

  property("Union should be commutative") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
    left ∪ right == right ∪ left

  property("Intersection should be commutative") = forAll: (left: Set[Numeral], right: Set[Numeral]) =>
    right ∩ left == left ∩ right

  property("Union should be associative") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
    (a ∪ b) ∪ c == a ∪ (b ∪ c)

  property("Intersection should be associative") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
    (a ∩ b) ∩ c == a ∩ (b ∩ c)

  property("Union should be distributive over intersection") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
    a ∪ (b ∩ c) == (a ∪ b) ∩ (a ∪ c)

  property("Intersection should be distributive over union") = forAll: (a: Set[Numeral], b: Set[Numeral], c: Set[Numeral]) =>
    a ∩ (b ∪ c) == (a ∩ b) ∪ (a ∩ c)

end SetSpecification

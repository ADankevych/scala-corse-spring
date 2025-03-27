package kse.unit6.challenge

import kse.model.DummyError
import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.Numeral
import kse.unit6.algebra.*
import kse.unit6.challenge.generators.{*, given}
import kse.unit6.challenge.order.*
import kse.unit6.challenge.set.{Empty, NonEmpty, Set}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object GenSpecification extends Properties("Gen"):

  (0 to 10) foreach { size =>
    property(s"List of numerals of size $size should satisfy the condition of strict order") = forAll(genStrictlyOrderedListOfNumerals[Numeral](size)):
      (list: List[Numeral]) => verifyListOrderConditions(list)
  }

  (0 to 4) foreach { depth =>
    property(s"Set of numeral of depth $depth should satisfy the condition of order") = forAll(genNumeralSet[Numeral](depth)): (set: Set[Numeral]) =>
      verifySetOrderConditions(set)
  }

  def verifyListOrderConditions(list: List[Numeral]): Boolean =
    list.sliding(2) forall {
      case Nil                    => true
      case single :: Nil          => true
      case first :: second :: Nil => first < second
      case _                      => throw DummyError("Should not be thrown")
    }

  def verifySetOrderConditions(set: Set[Numeral]): Boolean =
    def verifySetOrderConditionsReq(set: Set[Numeral], rootElement: Numeral, condition: (Numeral, Numeral) => Boolean): Boolean =
      set match
        case Empty => true
        case NonEmpty(left, element, right) =>
          condition(rootElement, element)
          && verifySetOrderConditionsReq(left, element, (root, elem) => root > elem)
          && verifySetOrderConditionsReq(right, element, (root, elem) => root < elem)

    set match
      case Empty => true
      case NonEmpty(left, element, right) =>
        verifySetOrderConditionsReq(left, element, (root, elem) => root > elem)
        && verifySetOrderConditionsReq(right, element, (root, elem) => root < elem)

end GenSpecification

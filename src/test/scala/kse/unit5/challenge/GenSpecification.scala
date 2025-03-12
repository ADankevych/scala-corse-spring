package kse.unit5.challenge

import kse.model.DummyError
import kse.unit4.challenge.numerals.Numeral
import kse.unit5.challenge.generators.*
import kse.unit5.challenge.set.{Empty, NonEmpty, NumeralSet}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object GenSpecification extends Properties("Gen"):

  (0 to 10) foreach { size =>
    property(s"List of numerals of size $size should satisfy the condition of order") = forAll(genSortedListOfNumerals(size)): (list: List[Numeral]) =>
      verifyListOrderConditions(list)
  }

  (0 to 4) foreach { depth =>
    property(s"Set of numeral of depth $depth should satisfy the condition of order") = forAll(genNumeralSet(depth)): (set: NumeralSet) =>
      verifySetOrderConditions(set)
  }

  def verifyListOrderConditions(list: List[Numeral]): Boolean =
    list.sliding(2) forall {
      case Nil                    => true
      case single :: Nil          => true
      case first :: second :: Nil => first < second
      case _                      => throw DummyError("Should not be thrown")
    }

  def verifySetOrderConditions(set: NumeralSet): Boolean =
    def verifySetOrderConditionsReq(set: NumeralSet, rootElement: Numeral, condition: (Numeral, Numeral) => Boolean): Boolean =
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

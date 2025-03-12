package kse.unit5.challenge

import kse.unit4.challenge.generators.genSuccessor
import kse.unit4.challenge.numerals.Numeral
import kse.unit5.challenge.set.*
import org.scalacheck.*

object generators:

  def genSortedListOfNumerals(size: Int): Gen[List[Numeral]] =

    def genSortedListOfNumeralsReq(size: Int, last: Numeral, list: List[Numeral]): Gen[List[Numeral]] =
      if size == 0 then Gen.const(list)
      else
        for
          delta  <- genSuccessor
          result <- genSortedListOfNumeralsReq(size - 1, last + delta, list :+ last)
        yield result

    for
      last   <- genSuccessor
      result <- genSortedListOfNumeralsReq(size, last, Nil)
    yield result

  def genNumeralSet(depth: Int): Gen[NumeralSet] =

    def genNumeralSetReq(elements: List[Numeral]): Gen[NumeralSet] =
      elements match
        case Nil            => Gen.const(Empty)
        case element :: Nil => Gen.const(NonEmpty(Empty, element, Empty))
        case _ =>
          for
            left  <- genNumeralSetReq(elements.take(elements.size / 2))
            right <- genNumeralSetReq(elements.takeRight(elements.size / 2))
          yield NonEmpty(left, elements(elements.size / 2), right)

    for
      elements <- genSortedListOfNumerals(math.pow(2, depth + 1).toInt - 1)
      set      <- genNumeralSetReq(elements)
    yield set

  lazy val genNonEmpty: Gen[NonEmpty] =
    (for
      depth <- Gen.chooseNum(1, 4)
      set   <- genNumeralSet(depth).retryUntil(_ != Empty)
    yield set) flatMap:
      case Empty       => genNonEmpty
      case v: NonEmpty => Gen.const(v)

  lazy val genNumeralSet: Gen[NumeralSet] =
    Gen.oneOf(Gen.const(Empty), genNonEmpty)

  given Arbitrary[NonEmpty]   = Arbitrary(genNonEmpty)
  given Arbitrary[NumeralSet] = Arbitrary(genNumeralSet)

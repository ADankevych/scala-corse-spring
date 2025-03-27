package kse.unit6.challenge

import kse.unit6.algebra.*
import kse.unit6.algebra.given
import kse.unit6.challenge.set.*
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object generators:

  def genStrictlyOrderedListOfNumerals[V: Arbitrary as gen: Monoid](size: Int): Gen[List[V]] =

    def genStrictlyOrderedListOfNumeralsReq(size: Int, last: V, list: List[V]): Gen[List[V]] =
      if size == 0 then Gen.const(list)
      else
        for
          // Generated `delta` should not be a unit
          // otherwise `last + delta` equals to `last`
          // and condition on strict order is violated
          delta  <- gen.arbitrary.retryUntil(_ != Monoid[V].unit)
          result <- genStrictlyOrderedListOfNumeralsReq(size - 1, last + delta, list :+ last)
        yield result

    for
      last   <- gen.arbitrary
      result <- genStrictlyOrderedListOfNumeralsReq(size, last, Nil)
    yield result

  def genNumeralSet[V: Arbitrary: Monoid](depth: Int): Gen[Set[V]] =

    def genNumeralSetReq(elements: List[V]): Gen[Set[V]] =
      elements match
        case Nil            => Gen.const(Empty)
        case element :: Nil => Gen.const(NonEmpty(Empty, element, Empty))
        case _ =>
          for
            left  <- genNumeralSetReq(elements.take(elements.size / 2))
            right <- genNumeralSetReq(elements.takeRight(elements.size / 2))
          yield NonEmpty(left, elements(elements.size / 2), right)

    for
      elements <- genStrictlyOrderedListOfNumerals[V](math.pow(2, depth + 1).toInt - 1)
      set      <- genNumeralSetReq(elements)
    yield set

  def genNonEmpty[V: Arbitrary: Monoid]: Gen[NonEmpty[V]] =
    (for
      depth <- Gen.chooseNum(1, 4)
      set   <- genNumeralSet[V](depth).retryUntil(_ != Empty)
    yield set) flatMap:
      case Empty             => genNonEmpty
      case NonEmpty(l, v, r) => Gen.const(NonEmpty[V](l, v, r))

  def genSet[V: Arbitrary: Monoid]: Gen[Set[V]] =
    Gen.oneOf(Gen.const(Empty), genNonEmpty[V])

  given [V: Arbitrary: Monoid]: Arbitrary[NonEmpty[V]] = Arbitrary(genNonEmpty[V])
  given [V: Arbitrary: Monoid]: Arbitrary[Set[V]]      = Arbitrary(genSet[V])

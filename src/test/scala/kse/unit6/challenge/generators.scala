package kse.unit6.challenge

import kse.unit6.algebra.*
import kse.unit6.algebra.given
import kse.unit6.challenge.order.*
import kse.unit6.challenge.set.*
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object generators:

  def genStrictlyOrderedListOfNumerals[V: Arbitrary: Monoid: Order](size: Int): Gen[List[V]] =

    def genStrictlyOrderedListOfNumeralsReq(size: Int, last: V, list: List[V]): Gen[List[V]] =
      if size == 0 then Gen.const(list)
      else
        for
          delta  <- Arbitrary.arbitrary[V].retryUntil(_ != Monoid[V].unit)
          result <- genStrictlyOrderedListOfNumeralsReq(size - 1, last + delta, last :: list)
        yield result.reverse

    for
      last   <- Arbitrary.arbitrary[V]
      result <- genStrictlyOrderedListOfNumeralsReq(size, last, Nil)
    yield result

  def genNumeralSet[V: Arbitrary: Monoid: Order](depth: Int): Gen[Set[V]] =

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

  def genNonEmpty[V: Arbitrary: Monoid: Order]: Gen[NonEmpty[V]] =
    for
      depth <- Gen.chooseNum(1, 4)
      set   <- genNumeralSet[V](depth).retryUntil(_ != Empty)
    yield set match
      case NonEmpty(l, v, r) => NonEmpty(l, v, r)
      case _                 => throw new IllegalStateException("This should not happen.")

  def genSet[V: Arbitrary: Monoid: Order]: Gen[Set[V]] =
    Gen.oneOf(Gen.const(Empty), genNonEmpty[V])

  given [V: Arbitrary: Monoid: Order]: Arbitrary[NonEmpty[V]] = Arbitrary(genNonEmpty[V])
  given [V: Arbitrary: Monoid: Order]: Arbitrary[Set[V]]      = Arbitrary(genSet[V])

package kse.unit5.challenge

import kse.unit4.challenge.numerals.Numeral
import scala.annotation.targetName

object set:

  trait NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean

    infix def exists(predicate: Numeral => Boolean): Boolean

    infix def contains(x: Numeral): Boolean

    infix def include(x: Numeral): NumeralSet

    infix def remove(x: Numeral): NumeralSet

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet

    @targetName("difference")
    infix def \(that: NumeralSet): NumeralSet

    @targetName("symmetric difference")
    infix def ∆(that: NumeralSet): NumeralSet

  end NumeralSet

  type Empty = Empty.type

  case object Empty extends NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean = true

    infix def exists(predicate: Numeral => Boolean): Boolean = false

    infix def contains(x: Numeral): Boolean = false

    infix def include(x: Numeral): NumeralSet = NonEmpty(Empty, x, Empty)

    infix def remove(x: Numeral): NumeralSet = this

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet = that

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet = Empty

    @targetName("difference")
    infix def \(that: NumeralSet): NumeralSet = Empty

    @targetName("symmetric difference")
    infix def ∆(that: NumeralSet): NumeralSet = that

    override def toString: String = "[*]"

    override def equals(obj: Any): Boolean = obj match {
      case _: Empty => true
      case _        => false
    }

    override def hashCode(): Int = 0

  end Empty

  case class NonEmpty(left: NumeralSet, element: Numeral, right: NumeralSet) extends NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean =
      predicate(element) && left.forAll(predicate) && right.forAll(predicate)

    infix def exists(predicate: Numeral => Boolean): Boolean =
      predicate(element) || left.exists(predicate) || right.exists(predicate)

    infix def contains(x: Numeral): Boolean =
      if x == element then true
      else if x < element then left.contains(x)
      else right.contains(x)

    infix def include(x: Numeral): NumeralSet =
      if x == element then this
      else if x < element then NonEmpty(left.include(x), element, right)
      else NonEmpty(left, element, right.include(x))

    infix def remove(x: Numeral): NumeralSet =
      if x < element then NonEmpty(left.remove(x), element, right)
      else if x > element then NonEmpty(left, element, right.remove(x))
      else {
        val union = left ∪ right
        if union.contains(x) then union.remove(x) else union
      }

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet =
      left ∪ right ∪ that.include(element)

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet =
      val newLeft  = left ∩ that
      val newRight = right ∩ that
      if that.contains(element) then NonEmpty(newLeft, element, newRight)
      else newLeft ∪ newRight

    @targetName("difference")
    infix def \(that: NumeralSet): NumeralSet =
      if that.contains(element) then (left \ that) ∪ (right \ that)
      else NonEmpty(left \ that, element, right \ that)

    @targetName("symmetric difference")
    infix def ∆(that: NumeralSet): NumeralSet =
      (this ∪ that) \ (this ∩ that)

    override def toString: String = s"[$left - [$element] - $right]"

    override def equals(obj: Any): Boolean = obj match {
      case NonEmpty(l, e, r) =>
        val allElementsInThisSet  = (l ∪ r) ∪ NonEmpty(Empty, e, Empty)
        val allElementsInOtherSet = (l ∪ r) ∪ NonEmpty(Empty, e, Empty)

        allElementsInThisSet.forAll(x => allElementsInOtherSet.contains(x)) &&
        allElementsInOtherSet.forAll(x => allElementsInThisSet.contains(x))

      case _ => false
    }

    override def hashCode: Int = {
      val allElements = (left ∪ right) ∪ NonEmpty(Empty, element, Empty)
      allElements.hashCode
    }

  end NonEmpty

end set

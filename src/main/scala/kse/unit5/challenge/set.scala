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

    override def equals(obj: Any): Boolean = obj.isInstanceOf[Empty]

  end Empty

  case class NonEmpty(left: NumeralSet, element: Numeral, right: NumeralSet) extends NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean =
      left.forAll(predicate) && predicate(element) && right.forAll(predicate)

    infix def exists(predicate: Numeral => Boolean): Boolean =
      left.exists(predicate) || predicate(element) || right.exists(predicate)

    infix def contains(x: Numeral): Boolean =
      left.contains(x) || x == element || right.contains(x)

    infix def include(x: Numeral): NumeralSet =
      if x < element then NonEmpty(left.include(x), element, right)
      else if x > element then NonEmpty(left, element, right.include(x))
      else this

    infix def remove(x: Numeral): NumeralSet =
      if x < element then NonEmpty(left.remove(x), element, right)
      else if x > element then NonEmpty(left, element, right.remove(x))
      else {
        val union = left ∪ right
        if union.contains(x) then union.remove(x) else union
      }

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet =
      that match
        case Empty => this
        case NonEmpty(l, e, r) =>
          if e < element then NonEmpty(l ∪ left, e, right ∪ r)
          else if e > element then NonEmpty(left ∪ l, element, right ∪ r)
          else NonEmpty(left ∪ l, element, right ∪ r)

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet =
      (left ∩ that) ∪ (right ∩ that) match
        case Empty       => Empty
        case nonEmptySet => if that.contains(element) then NonEmpty(nonEmptySet, element, Empty) else nonEmptySet

    @targetName("difference")
    infix def \(that: NumeralSet): NumeralSet =
      if that.contains(element) then (left \ that) ∪ (right \ that)
      else NonEmpty(left \ that, element, right \ that)

    @targetName("symmetric difference")
    infix def ∆(that: NumeralSet): NumeralSet =
      (this ∪ that) \ (this ∩ that)

    override def toString: String = s"[$left - [$element] - $right]"

    override def equals(obj: Any): Boolean =
      obj match
        case Empty => false
        case NonEmpty(l, e, r) =>
          left == l && element == e && right == r

  end NonEmpty

end set

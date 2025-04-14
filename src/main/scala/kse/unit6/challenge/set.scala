package kse.unit6.challenge

import kse.unit6.challenge.order.{Order, *, given}
import scala.annotation.targetName

object set:

  trait Set[+A]:

    infix def forAll(predicate: A => Boolean): Boolean

    infix def exists(predicate: A => Boolean): Boolean

    infix def contains[B >: A: Order](x: B): Boolean

    infix def include[B >: A: Order](x: B): Set[B]

    @targetName("union")
    infix def ∪[B >: A: Order](that: Set[B]): Set[B]

    @targetName("intersection")
    infix def ∩[B >: A: Order](that: Set[B]): Set[B]

    def isSubsetOf(that: Set[?]): Boolean = this.forAll(element => that.exists(_ == element))
  end Set

  type Empty = Empty.type

  // TODO: Remind about type system
  case object Empty extends Set[Nothing]:

    infix def forAll(predicate: Nothing => Boolean): Boolean = true

    infix def exists(predicate: Nothing => Boolean): Boolean = false

    infix def contains[B: Order](x: B): Boolean = false

    infix def include[B: Order](x: B): Set[B] = NonEmpty(Empty, x, Empty)

    @targetName("union")
    infix def ∪[B: Order](that: Set[B]): Set[B] = that

    @targetName("intersection")
    infix def ∩[B: Order](that: Set[B]): Set[B] = Empty

    override def toString: String = "[*]"

    override def equals(obj: Any): Boolean = obj match
      case _: Empty.type => true
      case _             => false

    override def hashCode(): Int = 0
  end Empty

  case class NonEmpty[A](left: Set[A], element: A, right: Set[A]) extends Set[A]:

    infix def forAll(predicate: A => Boolean): Boolean =
      predicate(element) && left.forAll(predicate) && right.forAll(predicate)

    infix def exists(predicate: A => Boolean): Boolean =
      predicate(element) || left.exists(predicate) || right.exists(predicate)

    infix def contains[B >: A: Order](x: B): Boolean =
      val comparison = summon[Order[B]].compare(x, element)
      if comparison == 0 then true
      else if comparison < 0 then left.contains(x)
      else right.contains(x)

    infix def include[B >: A: Order](x: B): Set[B] =
      val comparison = summon[Order[B]].compare(x, element)
      if comparison == 0 then this
      else if comparison < 0 then NonEmpty(left.include(x), element, right)
      else NonEmpty(left, element, right.include(x))

    @targetName("union")
    infix def ∪[B >: A: Order](that: Set[B]): Set[B] =
      (left ∪ (right ∪ that)).include(element)

    @targetName("intersection")
    infix def ∩[B >: A: Order](that: Set[B]): Set[B] =
      if that.contains(element) then
        val leftIntersection  = left ∩ that
        val rightIntersection = right ∩ that
        NonEmpty(leftIntersection, element, rightIntersection)
      else (left ∩ that) ∪ (right ∩ that)

    override def toString: String = s"[$left - [$element] - $right]"

    override def equals(obj: Any): Boolean =
      obj match
        case that: NonEmpty[_] => that.isSubsetOf(this) && this.isSubsetOf(that)
        case _                 => false

    override def hashCode(): Int =
      val leftHash    = left.hashCode()
      val rightHash   = right.hashCode()
      val elementHash = element.hashCode()
      leftHash ^ rightHash ^ elementHash
  end NonEmpty
end set

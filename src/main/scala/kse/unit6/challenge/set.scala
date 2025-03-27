package kse.unit6.challenge

import kse.unit6.challenge.order.{Order, given}
import scala.annotation.targetName

object set:

  trait Set[+A]:

    infix def forAll(predicate: A => Boolean): Boolean

    infix def exists(predicate: A => Boolean): Boolean

    infix def contains[B >: A](x: B)(using Order[B]): Boolean

    infix def include[B >: A](x: B)(using Order[B]): Set[B]

    infix def remove[B >: A](x: B)(using Order[B]): Set[B]

    @targetName("union")
    infix def ∪[B >: A](that: Set[B])(using Order[B]): Set[B]

    @targetName("intersection")
    infix def ∩[B >: A](that: Set[B])(using Order[B]): Set[B]

    @targetName("difference")
    infix def \[B >: A](that: Set[B])(using Order[B]): Set[B]

    @targetName("symmetric difference")
    infix def ∆[B >: A](that: Set[B])(using Order[B]): Set[B] = (this \ that) ∪ (that \ this)

  type Empty = Empty.type

  case object Empty extends Set[Nothing]:

    infix def forAll(predicate: Nothing => Boolean): Boolean = true

    infix def exists(predicate: Nothing => Boolean): Boolean = false

    infix def contains[B: Order](x: B): Boolean = false

    infix def include[B: Order](x: B): Set[B] = NonEmpty(Empty, x, Empty)

    infix def remove[B: Order](x: B): Set[B] = this

    @targetName("union")
    infix def ∪[B: Order](that: Set[B]): Set[B] = that

    @targetName("intersection")
    infix def ∩[B: Order](that: Set[B]): Set[B] = this

    @targetName("difference")
    infix def \[B: Order](that: Set[B]): Set[B] = this

    override def toString: String = "[*]"

  case class NonEmpty[A](left: Set[A], element: A, right: Set[A])(using Order[A]) extends Set[A]:

    infix def forAll(predicate: A => Boolean): Boolean =
      predicate(element) && left.forAll(predicate) && right.forAll(predicate)

    infix def exists(predicate: A => Boolean): Boolean =
      predicate(element) || left.exists(predicate) || right.exists(predicate)

    infix def contains[B >: A](x: B)(using Order[B]): Boolean =
      Order[B].compare(x, element) match
        case 0  => true
        case -1 => left.contains(x)
        case 1  => right.contains(x)

    infix def include[B >: A](x: B)(using Order[B]): Set[B] =
      Order[B].compare(x, element) match
        case 0  => this
        case -1 => NonEmpty(left.include(x), element, right)
        case 1  => NonEmpty(left, element, right.include(x))

    infix def remove[B >: A](x: B)(using Order[B]): Set[B] =
      Order[B].compare(x, element) match
        case 0 =>
          val union = left ∪ right
          if union.contains(x) then union.remove(x) else union
        case -1 => NonEmpty(left.remove(x), element, right)
        case 1  => NonEmpty(left, element, right.remove(x))

    @targetName("union")
    infix def ∪[B >: A](that: Set[B])(using Order[B]): Set[B] =
      left ∪ right ∪ that.include(element)

    @targetName("intersection")
    infix def ∩[B >: A](that: Set[B])(using Order[B]): Set[B] =
      val newLeft  = left ∩ that
      val newRight = right ∩ that
      if that.contains(element) then NonEmpty(newLeft, element, newRight)
      else newLeft ∪ newRight

    @targetName("difference")
    infix def \[B >: A](that: Set[B])(using Order[B]): Set[B] =
      val newLeft  = left \ that
      val newRight = right \ that
      if that.contains(element) then newLeft ∪ newRight
      else NonEmpty(newLeft, element, newRight)

    override def toString: String = s"[$left - [$element] - $right]"

  end NonEmpty
end set

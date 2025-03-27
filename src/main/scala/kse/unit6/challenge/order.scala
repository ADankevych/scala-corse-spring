package kse.unit6.challenge

import kse.unit4.challenge.numerals.{Numeral, Successor, Zero}
import scala.annotation.targetName

object order:

  trait Order[T]:
    def compare(left: T, right: T): Int

  given Order[Numeral] with

    def compare(left: Numeral, right: Numeral): Int = (left, right) match
      case (Zero, Zero)                 => 0
      case (Zero, _)                    => -1
      case (_, Zero)                    => 1
      case (Successor(a), Successor(b)) => compare(a, b)

  object Order:
    def apply[T](using ord: Order[T]): Order[T] = ord

  extension [V](elem: V)(using ord: Order[V])

    infix def >(that: V): Boolean =
      ord.compare(elem, that) > 0

    infix def <(that: V): Boolean =
      ord.compare(elem, that) < 0

    infix def >=(that: V): Boolean =
      ord.compare(elem, that) >= 0

    infix def <=(that: V): Boolean =
      ord.compare(elem, that) <= 0

end order

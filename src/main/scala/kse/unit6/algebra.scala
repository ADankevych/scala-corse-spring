package kse.unit6

import kse.unit4.challenge.numerals.{Numeral, Zero}
import scala.annotation.targetName

object algebra:

  trait Semigroup[V]:
    def combine(left: V, right: V): V

  object Semigroup:
    def apply[V: Semigroup as semigroup]: Semigroup[V] = semigroup

  trait Monoid[V] extends Semigroup[V]:
    def unit: V

  object Monoid:
    def apply[V: Monoid as monoid]: Monoid[V] = monoid

  extension [V: Monoid as monoid](elem: V)

    @targetName("addition")
    infix def +(that: V): V =
      monoid.combine(elem, that)

  given Monoid[Int] with
    def combine(left: Int, right: Int): Int = left + right
    def unit: Int                           = 0

  given Monoid[String] with
    def combine(left: String, right: String): String = left + right
    def unit: String                                 = ""

  given Monoid[Numeral] with
    def combine(left: Numeral, right: Numeral): Numeral = left + right
    def unit: Numeral                                   = Zero

  given [V]: Monoid[List[V]] with
    def combine(left: List[V], right: List[V]): List[V] = left ::: right
    def unit: List[V]                                   = Nil

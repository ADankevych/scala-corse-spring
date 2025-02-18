package kse.unit1.challenge

import scala.annotation.tailrec

object arithmetic:

  type Number = Long

  val increment: Number => Number =
    value => value + 1

  val decrement: Number => Number =
    value => value - 1

  val isZero: Number => Boolean =
    value => value == 0

  val isNonNegative: Number => Boolean =
    value => value >= 0

  val abs: Number => Number =
    value =>
      if isNonNegative(value) then value
      else -value

  def addition(left: Number, right: Number): Number =
    require(left >= 0, "Left must be non-negative")
    require(right >= 0, "Right must be non-negative")

    ???

  def multiplication(left: Number, right: Number): Number =
    require(left >= 0, "Left must be non-negative")
    require(right >= 0, "Right must be non-negative")

    ???

  def power(base: Number, p: Number): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    ???

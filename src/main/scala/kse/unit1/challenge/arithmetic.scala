package kse.unit1.challenge

import scala.annotation.tailrec

object arithmetic:

  type Number = Long

  private val increment: Number => Number =
    value => value + 1

  private val decrement: Number => Number =
    value => value - 1

  private val isZero: Number => Boolean =
    value => value == 0

  private val isNonNegative: Number => Boolean =
    value => value >= 0

  private val abs: Number => Number =
    value =>
      if isNonNegative(value) then value
      else -value

  @tailrec
  def addition(left: Number, right: Number): Number =
    if isZero(right) then left
    else if isNonNegative(right) then addition(increment(left), decrement(right))
    else addition(decrement(left), increment(right))

  def multiplication(left: Number, right: Number): Number =
    @tailrec
    def multiplicationLoop(left: Number, right: Number, acc: Number): Number =
      if isZero(right) then acc
      else if isNonNegative(right) then multiplicationLoop(left, decrement(right), addition(acc, left))
      else multiplicationLoop(left, increment(right), addition(acc, addition(0, left)))

    if !isNonNegative(right) && !isNonNegative(left) then multiplicationLoop(abs(left), abs(right), 0)
    else if !isNonNegative(right) && isNonNegative(left) then multiplicationLoop(right, left, 0)
    else multiplicationLoop(left, right, 0)

  def power(base: Number, p: Number): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    @tailrec
    def powerLoop(base: Number, p: Number, acc: Number): Number =
      if isZero(p) then acc
      else powerLoop(base, decrement(p), multiplication(acc, base))

    powerLoop(base, p, 1)

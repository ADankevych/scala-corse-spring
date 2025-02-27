package kse.unit4.challenge

import scala.annotation.targetName

object numerals:

  trait Numeral:

    def isZero: Boolean

    def predecessor: Numeral

    def successor: Numeral = Successor(this)

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = this.toInt > that.toInt

    @targetName("greater or equal to")
    infix def >=(that: Numeral): Boolean = this.toInt >= that.toInt

    @targetName("less than")
    infix def <(that: Numeral): Boolean = this.toInt < that.toInt

    @targetName("less or equal to")
    infix def <=(that: Numeral): Boolean = this.toInt <= that.toInt

    @targetName("addition")
    infix def +(that: Numeral): Numeral =
      numerals.Numeral.fromInt(this.toInt + that.toInt)

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral =
      numerals.Numeral.fromInt(math.max(0, this.toInt - that.toInt))

    def toInt: Int

    override def toString: String = s"Nat($predecessor)"

  object Numeral:

    def fromInt(n: Int): Numeral =
      if n <= 0 then Zero
      else Successor(fromInt(n - 1))

  type Zero = Zero.type

  object Zero extends Numeral:

    def isZero: Boolean = true

    def predecessor: Numeral = throw new UnsupportedOperationException("Zero has no predecessor")

    @targetName("greater than")
    override infix def >(that: Numeral): Boolean = this.toInt > that.toInt

    @targetName("addition")
    override infix def +(that: Numeral): Numeral =
      numerals.Numeral.fromInt(this.toInt + that.toInt)

    // Optional
    @targetName("subtraction")
    override infix def -(that: Numeral): Numeral =
      numerals.Numeral.fromInt(math.max(0, this.toInt - that.toInt))

    def toInt: Int = 0

    override def toString: String = "Zero"

    override def equals(obj: Any): Boolean = obj match
      case n: Numeral => this.toInt == n.toInt
      case _          => false

  object Successor:
    def unapply(successor: Successor): Option[Numeral] = Option(successor.predecessor)

  class Successor(n: Numeral) extends Numeral:

    def isZero: Boolean = false

    def predecessor: Numeral = n

    @targetName("greater than")
    override infix def >(that: Numeral): Boolean = this.toInt > that.toInt

    @targetName("addition")
    override infix def +(that: Numeral): Numeral =
      numerals.Numeral.fromInt(this.toInt + that.toInt)

    // Optional
    @targetName("subtraction")
    override infix def -(that: Numeral): Numeral =
      numerals.Numeral.fromInt(math.max(0, this.toInt - that.toInt))

    def toInt: Int = 1 + n.toInt

    override def toString: String = s"Succ(${n.toInt})"

    override def equals(obj: Any): Boolean = obj match
      case n: Numeral => this.toInt == n.toInt
      case _          => false

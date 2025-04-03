package kse.unit4.challenge

import scala.annotation.targetName

object numerals:

  trait Numeral:

    def isZero: Boolean

    def predecessor: Numeral

    def successor: Numeral = Successor(this)

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = (this, that) match
      case (_, Zero)                    => !this.isZero
      case (Zero, _)                    => false
      case (a: Successor, b: Successor) => a.predecessor > b.predecessor

    @targetName("greater or equal to")
    infix def >=(that: Numeral): Boolean = (this, that) match
      case (_, Zero)                    => true
      case (Zero, _)                    => false
      case (a: Successor, b: Successor) => a.predecessor >= b.predecessor

    @targetName("less than")
    infix def <(that: Numeral): Boolean = that > this

    @targetName("less or equal to")
    infix def <=(that: Numeral): Boolean = that >= this

    @targetName("addition")
    infix def +(that: Numeral): Numeral = that match
      case Zero         => this
      case s: Successor => this.successor + s.predecessor

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral = (this, that) match
      case (_, Zero)                    => this
      case (Zero, _)                    => Zero
      case (a: Successor, b: Successor) => a.predecessor - b.predecessor

    override def toString: String = s"Nat($predecessor)"

    override def equals(obj: Any): Boolean = obj.isInstanceOf[Numeral] && obj.hashCode == hashCode

    override def hashCode: Int =
      if isZero then 0
      else 31 * predecessor.hashCode + 1

  object Numeral:

    def fromInt(n: Int): Numeral =
      if n <= 0 then Zero
      else Successor(fromInt(n - 1))

  type Zero = Zero.type

  object Zero extends Numeral:

    def isZero: Boolean = true

    def predecessor: Numeral = throw new UnsupportedOperationException("Zero has no predecessor")

    override def toString: String = "Zero"

    override def equals(obj: Any): Boolean = obj.isInstanceOf[Zero]

    override def hashCode: Int = 0

  object Successor:
    def unapply(successor: Successor): Option[Numeral] = Option(successor.predecessor)

  class Successor(n: Numeral) extends Numeral:

    def isZero: Boolean = false

    def predecessor: Numeral = n

    override def toString: String = s"Succ(${n.toString})"

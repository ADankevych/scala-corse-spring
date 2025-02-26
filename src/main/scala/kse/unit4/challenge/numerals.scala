package kse.unit4.challenge

import scala.annotation.targetName

object numerals:

  trait Numeral:

    def isZero: Boolean

    def predecessor: Numeral

    def successor: Numeral = ???

    @targetName("greater than")
    infix def >(that: Numeral): Boolean

    @targetName("greater or equal to")
    infix def >=(that: Numeral): Boolean = ???

    @targetName("less than")
    infix def <(that: Numeral): Boolean = ???

    @targetName("less or equal to")
    infix def <=(that: Numeral): Boolean = ???

    @targetName("addition")
    infix def +(that: Numeral): Numeral

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral

    def toInt: Int

    override def toString: String = s"Nat($predecessor)"

  type Zero = Zero.type

  object Zero extends Numeral:

    def isZero: Boolean = ???

    def predecessor: Numeral = ???

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = ???

    @targetName("addition")
    infix def +(that: Numeral): Numeral = ???

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral = ???

    def toInt: Int = ???

    override def toString: String = ???

    override def equals(obj: Any): Boolean = ???

  object Successor:
    def unapply(successor: Successor): Option[Numeral] = Option(successor.predecessor)

  class Successor(n: Numeral) extends Numeral:

    def isZero: Boolean = ???

    def predecessor: Numeral = ???

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = ???

    @targetName("addition")
    infix def +(that: Numeral): Numeral = ???

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral = ???

    def toInt: Int = 1 + n.toInt

    override def toString: String = ???

    override def equals(obj: Any): Boolean = ???

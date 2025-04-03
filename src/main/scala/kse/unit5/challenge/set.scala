package kse.unit5.challenge

import kse.unit4.challenge.numerals.Numeral
import scala.annotation.targetName

object set:

  trait NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean

    infix def exists(predicate: Numeral => Boolean): Boolean

    infix def contains(x: Numeral): Boolean

    infix def include(x: Numeral): NumeralSet

    // Optional
    // Uncomment if needed
//    infix def remove(x: Numeral): NumeralSet

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet

  // Optional
  // Uncomment if needed
//    @targetName("difference")
//    infix def \(that: NumeralSet): NumeralSet

  // Optional
  // Uncomment if needed
//    @targetName("symmetric difference")
//    infix def ∆(that: NumeralSet): NumeralSet = ???

  end NumeralSet

  type Empty = Empty.type

  case object Empty extends NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean = ???

    infix def exists(predicate: Numeral => Boolean): Boolean = ???

    infix def contains(x: Numeral): Boolean = ???

    infix def include(x: Numeral): NumeralSet = ???

    // Optional
    // Uncomment if needed
//    infix def remove(x: Numeral): NumeralSet = ???

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet = ???

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet = ???

    // Optional
    // Uncomment if needed
//    @targetName("difference")
//    infix def \(that: NumeralSet): NumeralSet = ???

    override def toString: String = "[*]"

    override def equals(obj: Any): Boolean = ???

  end Empty

  case class NonEmpty(left: NumeralSet, element: Numeral, right: NumeralSet) extends NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean = ???

    infix def exists(predicate: Numeral => Boolean): Boolean = ???

    infix def contains(x: Numeral): Boolean = ???

    infix def include(x: Numeral): NumeralSet = ???

    // Optional
    // Uncomment if needed
//    infix def remove(x: Numeral): NumeralSet = ???

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet = ???

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet = ???

    // Optional
    // Uncomment if needed
//    @targetName("difference")
//    infix def \(that: NumeralSet): NumeralSet = ???

    override def toString: String = s"[$left - [$element] - $right]"

    override def equals(obj: Any): Boolean = ???

  end NonEmpty

end set

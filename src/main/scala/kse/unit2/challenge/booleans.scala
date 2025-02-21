package kse.unit2.challenge

import scala.annotation.{tailrec, targetName}

object booleans:

  case object True
  case object False

  type True  = True.type
  type False = False.type
  type Boolean

  val negation: Boolean => Boolean = ???

  val conjunction: (Boolean, => Boolean) => Boolean = ???

  val disjunction: (Boolean, => Boolean) => Boolean = ???

  val implication: (Boolean, => Boolean) => Boolean = ???

  val equivalence: (Boolean, => Boolean) => Boolean = ???

  extension (value: Boolean)

    @targetName("negation")
    infix def unary_! : Boolean = ???

    @targetName("conjunction")
    infix def ∧(that: => Boolean): Boolean = ???

    @targetName("disjunction")
    infix def ∨(that: => Boolean): Boolean = ???

    @targetName("implication")
    infix def →(that: => Boolean): Boolean = ???

    @targetName("equivalence")
    infix def ↔(that: => Boolean): Boolean = ???

  def fold(operation: (Boolean, Boolean) => Boolean, unit: Boolean)(list: List[Boolean]): Boolean = ???

  val conjunctionOfElements: List[Boolean] => Boolean = ???
  val disjunctionOfElements: List[Boolean] => Boolean = ???

  extension (booleans: List[Boolean])
    infix def conjunction: Boolean = conjunctionOfElements(booleans)
    infix def disjunction: Boolean = disjunctionOfElements(booleans)

package kse.unit2.challenge

import scala.annotation.{tailrec, targetName}

object booleans:

  case object True
  case object False

  type True    = True.type
  type False   = False.type
  type Boolean = True | False

  val negation: Boolean => Boolean = value => if value == True then False else True

  val conjunction: (Boolean, => Boolean) => Boolean = (a, b) => if a == True then b else False

  val disjunction: (Boolean, => Boolean) => Boolean = (a, b) => !(!a ∧ !b)

  val implication: (Boolean, => Boolean) => Boolean = (a, b) => if a == True then b else True

  val equivalence: (Boolean, => Boolean) => Boolean = (a, b) => (a → b) ∧ (b → a)

  extension (value: Boolean)

    @targetName("negation")
    infix def unary_! : Boolean = negation(value)

    @targetName("conjunction")
    infix def ∧(that: => Boolean): Boolean = conjunction(value, that)

    @targetName("disjunction")
    infix def ∨(that: => Boolean): Boolean = disjunction(value, that)

    @targetName("implication")
    infix def →(that: => Boolean): Boolean = implication(value, that)

    @targetName("equivalence")
    infix def ↔(that: => Boolean): Boolean = equivalence(value, that)

  def fold(operation: (Boolean, Boolean) => Boolean, unit: Boolean)(list: List[Boolean]): Boolean =
    @tailrec
    def foldLoop(lst: List[Boolean], acc: Boolean): Boolean =
      if lst.isEmpty then acc
      else foldLoop(lst.tail, operation(lst.head, acc))
    foldLoop(list, unit)

  val conjunctionOfElements: List[Boolean] => Boolean = fold((a, b) => if a == True then b else False, True)
  val disjunctionOfElements: List[Boolean] => Boolean = fold((a, b) => if a == True then True else b, False)

  extension (booleans: List[Boolean])
    infix def conjunction: Boolean = conjunctionOfElements(booleans)
    infix def disjunction: Boolean = disjunctionOfElements(booleans)

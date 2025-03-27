package kse.unit6.topic

import kse.unit6.algebra.*
import kse.unit6.algebra.given

object folding:

  def fold[T: Monoid](list: List[T]): T =
    list match
      case Nil          => Monoid[T].unit
      case head :: tail => Monoid[T].combine(head, fold(tail))

package kse.unit6.topic

import kse.unit6.algebra.*
import kse.unit6.algebra.given
import kse.unit6.topic.folding.fold
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object FoldSpecification extends Properties("Fold"):

  property("List of integer should be folded to a sum of values") = forAll: (list: List[Int]) =>
    fold(list) == list.sum

  property("List of string should be folded to a concatenation of values") = forAll: (list: List[String]) =>
    fold(list) == list.fold("")(_ + _)

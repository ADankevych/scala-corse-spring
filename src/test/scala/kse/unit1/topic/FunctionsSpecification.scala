package kse.unit1.topic

import kse.unit1.topic.functions.Number
import kse.unit1.topic.generators.{genNonNegativeNumberRange, genSmallNegativeNumber, genSmallNonNegativeNumber}
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}

object FunctionsSpecification extends Properties("Functions"):

  include(RecursiveFactorialSpecification)
  include(TailRecursiveFactorialSpecification)
  include(HigherOrderFunctionsSpecification)

end FunctionsSpecification

object RecursiveFactorialSpecification extends Properties("Recursive factorial"):
  import functions.recursive.factorial

  property("factorial should be correctly evaluated") = forAll(genSmallNonNegativeNumber): (value: Number) =>
    factorial(value) == (0 to value.toInt).fold(1)((acc, value) => if value == 0 then acc else acc * value)

  property("factorial should fail for negative numbers ") = forAll(genSmallNegativeNumber): (value: Number) =>
    throws(classOf[IllegalArgumentException]):
      factorial(value)

end RecursiveFactorialSpecification

object TailRecursiveFactorialSpecification extends Properties("Tail-recursive factorial"):
  import functions.`tail recursion`.factorial

  property("factorial should be correctly evaluated") = forAll(genSmallNonNegativeNumber): (value: Number) =>
    factorial(value) == (0 to value.toInt).fold(1)((acc, value) => if value == 0 then acc else acc * value)

  property("factorial should fail for negative numbers") = forAll(genSmallNegativeNumber): (value: Number) =>
    throws(classOf[IllegalArgumentException]):
      factorial(value)

end TailRecursiveFactorialSpecification

object HigherOrderFunctionsSpecification extends Properties("Higher order functions"):
  import functions.`higher-order functions`.*

  property("sum of integers should be correctly evaluated") = forAll(genNonNegativeNumberRange): (range: (Number, Number)) =>
    val (start, end) = range
    sumOfIntegers(start, end) == (start to end).sum

  property("sum of cubes should be correctly evaluated") = forAll(genNonNegativeNumberRange): (range: (Number, Number)) =>
    val (start, end) = range
    sumOfCubes(start, end) == (start to end).map(v => v * v * v).sum

  property("sum of factorials should be correctly evaluated") = forAll(genNonNegativeNumberRange): (range: (Number, Number)) =>
    val (start, end) = range
    sumOfFactorials(start, end) == (start to end).map(v => (0 to v.toInt).fold(1)((acc, value) => if value == 0 then acc else acc * value)).sum

end HigherOrderFunctionsSpecification

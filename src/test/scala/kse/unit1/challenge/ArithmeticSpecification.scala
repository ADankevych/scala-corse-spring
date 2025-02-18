package kse.unit1.challenge

import kse.unit1.challenge.arithmetic.*
import kse.unit1.challenge.generators.genSmallNonNegativeNumber
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}

object ArithmeticSpecification extends Properties("Arithmetic"):

  include(AdditionSpecification)
  include(MultiplicationSpecification)
  include(PowerSpecification)

end ArithmeticSpecification

object AdditionSpecification extends Properties("Addition"):

  property("left + right should be correctly evaluated") = forAll(genSmallNonNegativeNumber, genSmallNonNegativeNumber): (left: Number, right: Number) =>
    addition(left, right) == left + right

end AdditionSpecification

object MultiplicationSpecification extends Properties("Multiplication"):

  property("left * right should be correctly evaluated") = forAll(genSmallNonNegativeNumber, genSmallNonNegativeNumber): (left: Number, right: Number) =>
    multiplication(left, right) == left * right

end MultiplicationSpecification

object PowerSpecification extends Properties("Power"):

  property("base ^ p should be correctly evaluated") = forAll(genSmallNonNegativeNumber, genSmallNonNegativeNumber): (base: Number, p: Number) =>
    (base != 0 && p != 0) ==> {
      power(base, p) == Math.pow(base, p).toLong
    }

end PowerSpecification

package kse.unit4.challenge

import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}
import org.scalacheck.Test.Parameters

object NumeralsSpecification extends Properties("Numerals"):

  override def overrideParameters(p: Parameters): Parameters =
    p.withMinSuccessfulTests(50).withMaxDiscardRatio(100)

  property("Zero is zero") = forAll: (n: Zero) =>
    n.isZero

  property("Successor increases value") = forAll: (n: Numeral) =>
    n.successor.predecessor == n

  property("Predecessor decreases value unless Zero") = forAll: (n: Successor) =>
    n.predecessor.successor == n

  property("Addition is commutative") = forAll: (a: Numeral, b: Numeral) =>
    (a + b) == (b + a)

  property("Addition is associative") = forAll: (a: Numeral, b: Numeral, c: Numeral) =>
    ((a + b) + c) == (a + (b + c))

  property("Subtraction from itself results in Zero") = forAll: (a: Numeral) =>
    (a - a) == Zero

  property("Subtraction is inverse of addition for successor values") = forAll: (a: Numeral, b: Numeral) =>
    (a + b) - b == a

  property("Zero is the identity for addition") = forAll: (a: Numeral) =>
    (a + Zero) == a && (Zero + a) == a

  property("Greater than and less than are consistent") = forAll: (a: Numeral, b: Numeral) =>
    (a > b) == (b < a)

  property("Greater or equal is consistent with less or equal") = forAll: (a: Numeral, b: Numeral) =>
    (a >= b) == !(a < b)

  property("Zero has no predecessor") = throws(classOf[UnsupportedOperationException]):
    Zero.predecessor

end NumeralsSpecification

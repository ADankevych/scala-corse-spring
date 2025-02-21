package kse.unit2.topic

import kse.model.DummyError
import kse.unit2.topic.generators.given
import kse.unit2.topic.nats.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}

object Nat3Specification extends Properties("Nat3"):

  include(AdditionSpecification)
  include(MultiplicationSpecification)
  include(AxiomsSpecification)
  include(FoldSpecification)

end Nat3Specification

object AdditionSpecification extends Properties("Addition"):

  property("Zero + value is value") = forAll: (value: Nat3) =>
    (Zero + value) == value

  property("value + Zero is value") = forAll: (value: Nat3) =>
    (value + Zero) == value

  property("One + One is Two") = propBoolean:
    (One + One) == Two

  property("One + Two is Zero") = propBoolean:
    (One + Two) == Zero

  property("Two + One is Zero") = propBoolean:
    (Two + One) == Zero

  property("Two + Two is One") = propBoolean:
    (Two + Two) == One

end AdditionSpecification

object MultiplicationSpecification extends Properties("Multiplication"):

  property("Zero * value is Zero") = forAll: (value: Nat3) =>
    (Zero * value) == Zero

  property("value * Zero is Zero") = forAll: (value: Nat3) =>
    (value * Zero) == Zero

  property("One * value is value") = forAll: (value: Nat3) =>
    (One * value) == value

  property("value * One is value") = forAll: (value: Nat3) =>
    (value * One) == value

  property("Two * Two is One") = propBoolean:
    (Two * Two) == One

end MultiplicationSpecification

object AxiomsSpecification extends Properties("Axioms"):

  property("left unit: Zero + a ≡ a") = forAll: (a: Nat3) =>
    (Zero + a) == a

  property("right unit: a + Zero ≡ a") = forAll: (a: Nat3) =>
    (a + Zero) == a

  property("commutativity: a + b ≡ b + a") = forAll: (a: Nat3, b: Nat3) =>
    (a + b) == (b + a)

  property("associativity: (a + b) + c ≡ a + (b + c)") = forAll: (a: Nat3, b: Nat3, c: Nat3) =>
    ((a + b) + c) == (a + (b + c))

  property("left unit: One * a ≡ a") = forAll: (a: Nat3) =>
    (One * a) == a

  property("right unit: a * One ≡ a") = forAll: (a: Nat3) =>
    (a * One) == a

  property("commutativity: a * b ≡ b * a") = forAll: (a: Nat3, b: Nat3) =>
    (a * b) == (b * a)

  property("associativity: (a * b) * c ≡ a * (b * c)") = forAll: (a: Nat3, b: Nat3, c: Nat3) =>
    ((a * b) * c) == (a * (b * c))

  property("distributivity: (a + b) * c ≡ a * c + b * c") = forAll: (a: Nat3, b: Nat3, c: Nat3) =>
    ((a + b) * c) == (a * c + b * c)

end AxiomsSpecification

object FoldSpecification extends Properties("Fold"):

  property("Sum of all elements from the empty list should be Zero") = propBoolean:
    Nil.∑ == Zero

  property("Sum of all elements from non-empty list should be correctly evaluated") = forAll: (nats: List[Nat3]) =>
    nats.∑ == nats.foldRight[Nat3](Zero)((value, acc) => value + acc)

  property("Product of all elements from the empty list should be One") = propBoolean:
    Nil.∏ == One

  property("Product of all elements from non-empty list should be correctly evaluated") = forAll: (nats: List[Nat3]) =>
    nats.∏ == nats.foldRight[Nat3](One)((value, acc) => value * acc)

end FoldSpecification

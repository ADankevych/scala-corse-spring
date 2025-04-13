package kse.unit6.topic

import kse.unit6.topic.generators.given
import kse.unit6.topic.polymorphic.*
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object PolymorphicSpecification extends Properties("Polymorphic"):

  include(IdentitySpecification)
  include(CompositionSpecification)
  include(YCombinatorSpecification)

end PolymorphicSpecification

object IdentitySpecification extends Properties("Identity"):

  property("Polymorphic identity function should be applicable for integers") = forAll: (int: Int) =>
    id(int) == int

  property("Polymorphic identity function should be applicable for strings") = forAll: (string: String) =>
    id(string) == string

  property("Polymorphic identity function should be applicable for lists") = forAll: (list: List[Boolean]) =>
    id(list) == list

end IdentitySpecification

object CompositionSpecification extends Properties("Composition"):

  property("Polymorphic composition of function should be correctly evaluated") = forAll: (f: Int => String, g: Boolean => Int, x: Boolean) =>
    `f(g(x))`(f)(g)(x) == (f compose g)(x)

end CompositionSpecification

object YCombinatorSpecification extends Properties("Y-combinator"):

  lazy val almostFactorialFunction: (BigInt => BigInt) => (BigInt => BigInt) =
    (f: BigInt => BigInt) =>
      (x: BigInt) =>
        if x == 0 then 1
        else x * f(x - 1)

  property("Polymorphic Y-combinator should allow to evaluate factorial recursively") = forAll: (value: BigInt) =>
    Y(almostFactorialFunction)(value) == (0 to value.toInt).fold(1)((acc, value) => if value == 0 then acc else acc * value)

end YCombinatorSpecification

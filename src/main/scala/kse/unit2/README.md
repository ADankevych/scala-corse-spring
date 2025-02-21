# Unit 2

You are challenged to implement Boolean algebra:
* implement `negation`, `conjunction`, `disjunction`, `implication` and `equivalence` operators;
* it is not allowed to use embedded `scala.Boolean`;
* it is not allowed to use embedded logic operations for implementing boolean operators;
* `fold` method should be implemented recursively, it's not allowed to use `List`/`Seq` api (i.e. `list.fold(...)`).
* it is required to provide implementation for `NegationSpecification`, `ConjunctionSpecification`
`DisjunctionSpecification`, `ImplicationSpecification`, `AxiomsSpecification`, `FoldSpecification`
* `False ∧ value should ignore the second argument`, `True ∨ value should ignore the second argument` 
and `False → value should ignore the second argument` are required.
* `EquivalenceSpecification` is an optional challenge;
* implement the required tests;
* all tests should pass;
* github build must be green.

Implementing optional challenge is rewarded by additional **1 point** and **1 Wolkov point**.

One Wolkov point will be awarded to the first person who suggests an idea for implementing these tests.
```scala 3
property("False ∧ value should ignore the second argument")

property("True ∨ value should ignore the second argument")

property("False → value should ignore the second argument")
```

## Key notes

### Math
In mathematics, a semiring is an algebraic structure similar to a ring, but it does not require the existence of additive inverses. 
A semiring consists of a set equipped with two binary operations, typically called addition and multiplication.

Review
* [Monoid](https://en.wikipedia.org/wiki/Monoid)
* [Semiring](https://en.wikipedia.org/wiki/Semiring)
* [Boolean algebra](https://en.wikipedia.org/wiki/Boolean_algebra)

### Union types

```scala 3
type Nat3 = Zero | One | Two
```

Review
* [Union type](https://en.wikipedia.org/wiki/Union_type)
* [Scala 3 Reference. Union Types](https://docs.scala-lang.org/scala3/reference/new-types/union-types.html)
* [Scala 3 Book. Union Types](https://docs.scala-lang.org/scala3/book/types-union.html)
* [Baeldung. Type Disjunction (Union Types) in Scala](https://www.baeldung.com/scala/type-disjunction)
* [Understanding Union Types in Scala 3](https://www.turingtaco.com/understanding-union-types-in-scala-3/)

### Explicit Nulls
Explicit nulls is an opt-in feature that modifies the Scala type system, 
which makes reference types (anything that extends AnyRef) non-nullable.

Review
* [Scala 3 Reference. Explicit Nulls](https://docs.scala-lang.org/scala3/reference/experimental/explicit-nulls.html)

### Pattern matching

```scala 3
def addition(left: Nat3, right: Nat3): Nat3 =
  (left, right) match
    case (Zero, value)           => value
    case (value, Zero)           => value
    case (One, One)              => Two
    case (One, Two) | (Two, One) => Zero
    case (Two, Two)              => One
```

Review
* [Pattern matching](https://en.wikipedia.org/wiki/Pattern_matching)
* [Scala 3 Book. Pattern Matching](https://docs.scala-lang.org/tour/pattern-matching.html)
* [Baeldung. Pattern Matching in Scala](https://www.baeldung.com/scala/pattern-matching)
* [Rock the JVM. Scala 3: Match Types Quickly Explained](https://rockthejvm.com/articles/scala-3-match-types)
* [Lean 4 Reference. Pattern Matching](https://lean-lang.org/doc/reference/latest/Terms/Pattern-Matching/)

### Call-by-Value and Call-by-Name parameters

Call-by-Name parameters are used when the evaluation of the argument is not always required.

```scala 3
def multiplication(left: Nat3, right: => Nat3): Nat3
```

Review
* [Scala 3 Book. By-name parameters](https://docs.scala-lang.org/tour/by-name-parameters.html)
* [Baeldung. By-Value and By-Name Parameters in Scala](https://www.baeldung.com/scala/parameters-by-value-by-name)
* [Rock the JVM. 3 Fun Call-by-Name Tricks in Scala](https://rockthejvm.com/articles/3-call-by-name-tricks-in-scala)


### The @targetName annotation
```scala 3
@targetName("addition")
```
Review
* [Scala 3 Reference. The @targetName annotation](https://docs.scala-lang.org/scala3/reference/other-new-features/targetName.html)
* [Baeldung. @targetName Annotation in Scala 3](https://www.baeldung.com/scala/targetname-annotation)

### Operators
Scala 3 is a full OOP language, everything in Scala is an object, that's why there is no "embedded" primitive operators,
each operator is a method of a certain class.

```scala 3
def +(that: Nat3): Nat3
```
Review
* [Scala 3 Reference. Rules for Operators](https://docs.scala-lang.org/scala3/reference/changed-features/operators.html)
* [Scala 3 Book. Operators](https://docs.scala-lang.org/tour/operators.html)
* [Baeldung. Introduction to Scala Operators](https://www.baeldung.com/scala/operators-intro)


### Extension methods
Extension methods provides an extra functionality without adding inheritance, especially for the cases where it is 
not possible to extend the classes.
```scala 3
extension (value: Nat3)

    @targetName("addition")
    infix def +(that: Nat3): Nat3 = functions.addition(value, that)
    
    @targetName("multiplication")
    infix def *(that: => Nat3): Nat3 = functions.multiplication(value, that)
```

Review
* [Scala 3 Reference. Extension Methods](https://docs.scala-lang.org/scala3/reference/contextual/extension-methods.html)
* [Scala 3 Book. Extension Methods](https://docs.scala-lang.org/scala3/book/ca-extension-methods.html)
* [Extension Methods in Scala 3](https://www.baeldung.com/scala/extension-methods)
* [. Scala 3: Extension Methods Quickly Explained](https://rockthejvm.com/articles/scala-3-extension-methods)


### Folding

Folds allows to process data recursively in a sequential data structures. 

```scala 3
  def fold(operation: (Nat3, Nat3) => Nat3, unit: Nat3)(list: List[Nat3]): Nat3 =
    @tailrec
    def foldReq(list: List[Nat3], acc: Nat3): Nat3 =
      list match
        case Nil          => acc
        case head :: tail => foldReq(tail, operation(head, acc))

    foldReq(list, unit)
```

Review
* [Fold (higher-order function)](https://en.wikipedia.org/wiki/Fold_(higher-order_function))
* [Catamorphism](https://en.wikipedia.org/wiki/Catamorphism)
* [Understanding Scala folds](https://avapl.github.io/posts/understanding-scala-folds/)
* [Writing Tail-Recursive Algorithms in Scala (and the tailrec annotation)](https://alvinalexander.com/scala/fp-book/tail-recursive-algorithms/)

### ScalaCheck properties

Parameter less property when all arguments are well known
```scala 3
  property("One + One is Two") = propBoolean:
    (One + One) == Two
```

A property with generated arguments
```scala 3
  property("value + Zero is value") = forAll: (value: Nat3) =>
    (value + Zero) == value
```

`forAll` implicitly takes a certain `Arbitrary` instance typed with `Nat3` and uses it for value generation. 

Compare to `Unit 1`
```scala 3
forAll(genSmallNonNegativeNumber, genSmallNonNegativeNumber)
```

Review
* [User Guide. Properties](https://github.com/typelevel/scalacheck/blob/main/doc/UserGuide.md#properties)
* [Stateless Tests: Properties](https://www.baeldung.com/scala/scalacheck#stateless-tests-properties)
* [Scala Exercises. ScalaCheck Properties](https://www.scala-exercises.org/scalacheck/properties)
* [Conditional Properties](https://github.com/typelevel/scalacheck/blob/main/doc/UserGuide.md#conditional-properties). Required for the optional challenge


### ScalaCheck generators
`Gen` used as a schema to generate the values. `Gen` does not generate the values immediately, but provides a 
pattern which is used for generation.

```scala 3
lazy val genNat3: Gen[Nat3] = Gen.oneOf(Zero, One, Two)
```

Review
* [User Guide. Generators](https://github.com/typelevel/scalacheck/blob/main/doc/UserGuide.md#generators)
* [Scala Exercises. ScalaCheck Generators](https://www.scala-exercises.org/scalacheck/generators)
* [Introduction to ScalaCheck. Generators](https://www.baeldung.com/scala/scalacheck#generators)

### ScalaCheck arbitrary
`Arbitrary` encapsulates the generating logic to be used in tests via implicit (`given` and `using`) Scala magic.

```scala 3
given Arbitrary[Nat3] = Arbitrary(genNat3)
```

Review
* [User Guide. Arbitrary](https://github.com/typelevel/scalacheck/blob/main/doc/UserGuide.md#the-arbitrary-generator)
* [Scala Exercises. ScalaCheck Arbitrary](https://www.scala-exercises.org/scalacheck/arbitrary)


### The magic of `given` and `using` (former `implicit`s)
Generally `given` and `using` pair are used to create so-called type classes for ad-hoc polymorphism. This approach
also can be used for "implicit" substitution for the variable depends on its type.

```scala 3
given Arbitrary[Nat3] = Arbitrary(genNat3)
```

```scala 3
  /** Converts a function into a universally quantified property */
  def forAll[A1, P](
      f: A1 => P
  )(implicit // `using` in Scala 3
      p: P => Prop,
      a1: Arbitrary[A1],
      s1: Shrink[A1],
      pp1: A1 => Pretty
  ): Prop = forAllShrink(arbitrary[A1], s1.shrink)(f andThen p)
```

Given instances exists in a "separate" name space.

Import
```scala 3
import kse.unit2.topic.generators.*
```
imports everything except `given` instances.

Import
```scala 3
import kse.unit2.topic.generators.given
```
imports only `given` instances.


Review
* [Given Instances](https://docs.scala-lang.org/scala3/reference/contextual/givens.html)
* [Using Clauses](https://docs.scala-lang.org/scala3/reference/contextual/using-clauses.html)
* [Scala 3: Given and Using Clauses](https://rockthejvm.com/articles/scala-3-given-and-using-clauses)
* [Implicit Parameters in Scala](https://www.baeldung.com/scala/implicit-parameters)


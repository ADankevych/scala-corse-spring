# Unit 4

You are challenged to implement Numerals:
* implement `Numeral`, `Zero` and `Successor`;
* subtraction operation is an optional challenge;
* provide test cases and implement them, covering all operations;
* all tests should pass;
* github build must be green.

## Natural Numbers

```
Nat ::= Zero | Succ(Nat)
```

```scala 3
trait Numeral
object Zero extends Numeral
class Successor(n: Numeral) extends Numeral
```

Review
* [Peano axioms](https://en.wikipedia.org/wiki/Peano_axioms)
* [Church Encoding](https://en.wikipedia.org/wiki/Church_encoding)
* [Natural Numbers as Church Numerals](https://www.cs.unc.edu/~stotts/723/Lambda/church.html)

In computer science, an abstract data type (ADT) is a mathematical model for data types, defined by its behavior (semantics)
from the point of view of a user of the data, specifically in terms of possible values, possible operations on data of this type,
and the behavior of these operations.

Review
* [Abstract data type](https://en.wikipedia.org/wiki/Abstract_data_type)

## Patter matching

```scala 3
object Successor:
  def unapply(successor: Successor): Option[Numeral] = Option(successor.predecessor)
```

Review
* [Scala Exercises. Classes vs Case Classes](https://www.scala-exercises.org/scala_tutorial/classes_vs_case_classes)
* [Scala 3 Reference. Pattern Matching](https://docs.scala-lang.org/scala3/reference/changed-features/pattern-matching.html)
* [Scala Docs. Pattern Matching. Extractor Patterns](https://www.scala-lang.org/files/archive/spec/2.13/08-pattern-matching.html#extractor-patterns)
* [Scala pattern matching: apply the unapply](https://medium.com/wix-engineering/scala-pattern-matching-apply-the-unapply-7237f8c30b41)

## Equals and hashcode contract

```scala 3
override def equals(obj: Any): Boolean = ???
```

Review
* [Baeldung. Java equals() and hashCode() Contracts](https://www.baeldung.com/java-equals-hashcode-contracts)
* [Medium. Java Equals & Hashcode contract — Java Interview Question](https://medium.com/@ghoshsiddharth25/java-equals-and-hashcode-contract-656d481f86da)
* [Baeldung. Equality in Scala](https://www.baeldung.com/scala/equality-in-scala)
* [Geeks for Geeks. Equality in Scala](https://www.geeksforgeeks.org/object-equality-in-scala/)

## ScalaCheck

```scala 3
override def overrideParameters(p: Parameters): Parameters =
  p.withMinSuccessfulTests(50).withMaxDiscardRatio(100)
```

Review
* [Introduction to ScalaCheck](https://www.baeldung.com/scala/scalacheck)


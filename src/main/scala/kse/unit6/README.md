# Unit 6

You are challenged to implement abstract set:
* implement set;
* implement order for Numerals and use it in test;
* implement the required tests;
* all tests should pass;
* github build must be green.

## Order

```scala 3
trait Order[V]
```

* [Order](https://en.wikipedia.org/wiki/Order_(mathematics))
* [Partial order](https://en.wikipedia.org/wiki/Partially_ordered_set#Partial_orders)
* [Total order](https://en.wikipedia.org/wiki/Total_order)


## Algebra

```scala 3
trait Semigroup[V]
trait Monoid[V] extends Semigroup[V]
```

* [Abstract algebra](https://en.wikipedia.org/wiki/Abstract_algebra)
* [Semigroup](https://en.wikipedia.org/wiki/Semigroup)
* [Monoid](https://en.wikipedia.org/wiki/Monoid)

## Type hierarchies

```scala 3
trait Set[+A]
case object Empty extends Set[Nothing]
case class NonEmpty[A](left: Set[A], element: A, right: Set[A]) extends Set[A]
```

Review
* [Scala 3 Reference. Explicit Nulls](https://docs.scala-lang.org/scala3/reference/experimental/explicit-nulls.html)
* [Scala 3 Book. A First Look at Types](https://docs.scala-lang.org/scala3/book/first-look-at-types.html)
* [Scala Docs. Unified Types](https://docs.scala-lang.org/tour/unified-types.html)
* [Baeldung. Type Hierarchies in Scala](https://www.baeldung.com/scala/type-hierarchies)

## Covariance and contravariance

Variance lets you control how type parameters behave with regards to subtyping. 
Scala supports variance annotations of type parameters of generic classes, to allow them to be covariant, contravariant, 
or invariant if no annotations are used. The use of variance in the type system allows us to make intuitive connections between complex types.

```scala 3
trait Set[+A]
```

Review
* [Covariance and contravariance (computer science)](https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science))
* [Category Theory. Functors. Covariance and contravariance](https://en.wikipedia.org/wiki/Functor#Covariance_and_contravariance)
* [Scala 3 Book. Variances](https://docs.scala-lang.org/tour/variances.html)
* [Baeldung. Variances](https://www.baeldung.com/scala/variances)
* [Rock the JVM. Demystifying Variance Positions in Scala](https://rockthejvm.com/articles/demystifying-variance-positions-in-scala)

## Type classes

```scala 3
  trait Semigroup[V]:
    def combine(left: V, right: V): V

  object Semigroup:
    def apply[V: Semigroup as semigroup]: Semigroup[V] = semigroup
```

Review
* [Ad hoc polymorphism](Ad hoc polymorphism)
* [Scala 3 Reference. Implementing Type classes](https://docs.scala-lang.org/scala3/reference/contextual/type-classes.html)
* [Scala 3 Book. Type Classes](https://docs.scala-lang.org/scala3/book/ca-type-classes.html)
* [Baeldung. Type Classes in Scala](https://www.baeldung.com/scala/type-classes)


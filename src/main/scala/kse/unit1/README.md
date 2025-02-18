# Unit 1

You are challenged to implement `addition`, `multiplication` and `power` functions:
* replace `???` with your implementation;
* functions must be tail-recursive;
* functions must cover either **non-negative** numbers (regular tasks);
  or **any (negative including)** numbers (optional tasks) for  `addition` and `multiplication` only; keep `p` in `power` **non-negative**.
* it is allowed to use only `increment`, `decrement`, `isZero`, `isNonNegative`, `abs`, `addition`
and `multiplication` functions. It is not allowed to use regular `+`, `*` or `pow`.
* it is not allowed to use Scala 2 syntax if Scala 3 syntax is applicable
(i.e. curly braces `{...}` instead of semicolon `:` and tabulations);
* all tests should pass;
* github build must be green.

Implementing optional challenge is rewarded by additional **1 point** and **1 Wolkov point**.


## Key notes

### Objects
In this unit `object`s are used for namespace and code structure.

```scala 3
object functions:
  ...
```

For improved code readability, the optional `end` marker is used to highlight the end of the object.

````scala 3
object recursive:
  ...
end recursive
````

Review
* [Scala Docs. Singleton objects](https://docs.scala-lang.org/tour/singleton-objects.html)
* [Scala 3 Reference. The End Marker](https://docs.scala-lang.org/scala3/reference/other-new-features/indentation.html)
* [Scala 3 Reference. Optional braces](https://docs.scala-lang.org/scala3/reference/other-new-features/indentation.html)


### Type aliases

```scala 3
type Number = BigInt
```
Review
* [Alvin Alexander. Scala type aliases (syntax, examples)](https://alvinalexander.com/scala/scala-type-aliases-syntax-examples/)

### Require
```scala 3
require(n >= 0, "Argument should be non-negative")
```

Review
* [Baeldung. Difference Between assert and require](https://www.baeldung.com/scala/assert-vs-require)


### Conditional expressions

```scala 3
if n == 0 then 1
else n * factorial(n - 1)
```

Review
* [Baeldung. Conditional expressions](https://www.baeldung.com/scala/conditional-expressions)

### Recursion and tail recursion
```scala 3
@tailrec
def factorialReq(n: Number, acc: Number): Number =
  if n == 0 then acc
  else factorialReq(n - 1, acc * n)
```

Review
* [Tail call](https://en.wikipedia.org/wiki/Tail_call)
* [Baeldung. Tail recursion in Scala](https://www.baeldung.com/scala/tail-recursion)
* [Scala Docs. @tailrec annotation](https://docs.scala-lang.org/tour/annotations.html#annotations-that-ensure-correctness-of-encodings)

### Function and methods

Review
* [Baeldung. Functions and methods](https://www.baeldung.com/scala/functions-methods)
* [Medium. Methods vs functions](https://rjlfinn.medium.com/scala-methods-vs-functions-970342a842c9)

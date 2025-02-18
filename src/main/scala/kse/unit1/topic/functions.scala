package kse.unit1.topic

import scala.annotation.tailrec

object functions:
  type Number = BigInt

  object recursive:

    def factorial(n: Number): Number =
      require(n >= 0, "Argument should be non-negative")

      if n == 0 then 1
      else n * factorial(n - 1)

  end recursive

  object `tail recursion`:

    def factorial(n: Number): Number =
      require(n >= 0, "Argument should be non-negative")

      @tailrec
      def factorialReq(n: Number, acc: Number): Number =
        if n == 0 then acc
        else factorialReq(n - 1, acc * n)

      factorialReq(n, acc = 1)

  end `tail recursion`

  object `higher-order functions`:

    val id: Number => Number   = v => v
    val cube: Number => Number = v => v * v * v

    val factorial: Number => Number = `tail recursion`.factorial

    val sum: (Number => Number) => (Number, Number) => Number =
      f =>
        (a, b) =>
          if a < 0 || b < 0 || a > b then 0
          else f(a) + sum(f)(a + 1, b)

    end sum

    val /* or `def` */ sumOfIntegers: (Number, Number) => Number = sum(id)
    val /* or `def` */ sumOfCubes: (Number, Number) => Number    = sum(cube)

    /*
      Compile error
      `def sumFactorials(a: Number, b: Number): Number = sum(id)`
     */

    // Works
    def sumOfFactorials(a: Number, b: Number): Number = sum(factorial)(a, b)

    // Also works but the type is undesired
    def anotherSumFactorials(a: Number, b: Number): (Number, Number) => Number = sum(factorial)

  end `higher-order functions`

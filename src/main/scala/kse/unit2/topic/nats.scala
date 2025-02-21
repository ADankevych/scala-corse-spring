package kse.unit2.topic

import scala.annotation.{tailrec, targetName}

object nats:

  case object Zero
  case object One
  case object Two

  type Zero = Zero.type
  type One  = One.type
  type Two  = Two.type
  type Nat3 = Zero | One | Two

  object methods:

    def addition(left: Nat3, right: Nat3): Nat3 =
      (left, right) match
        case (Zero, value)           => value
        case (value, Zero)           => value
        case (One, One)              => Two
        case (One, Two) | (Two, One) => Zero
        case (Two, Two)              => One

    def multiplication(left: Nat3, right: => Nat3): Nat3 =
      if left == Zero then Zero
      else
        (left, right) match
          case (Zero, value) => Zero
          case (value, Zero) => Zero
          case (One, value)  => value
          case (value, One)  => value
          case (Two, Two)    => One

  end methods

  object functions:

    val addition: (Nat3, Nat3) => Nat3 =
      (left, right) =>
        (left, right) match
          case (Zero, value)           => value
          case (value, Zero)           => value
          case (One, One)              => Two
          case (One, Two) | (Two, One) => Zero
          case (Two, Two)              => One

    val multiplication: (Nat3, => Nat3) => Nat3 =
      (left, right) =>
        if left == Zero then Zero
        else
          (left, right) match
            case (Zero, value) => Zero
            case (value, Zero) => Zero
            case (One, value)  => value
            case (value, One)  => value
            case (Two, Two)    => One

  end functions

  object conversions:

    val addition: (Nat3, Nat3) => Nat3 = methods.addition

    def multiplication: (Nat3, Nat3) => Nat3 = methods.multiplication

  end conversions

  extension (value: Nat3)

    @targetName("addition")
    infix def +(that: Nat3): Nat3 = functions.addition(value, that)

    @targetName("multiplication")
    infix def *(that: => Nat3): Nat3 = functions.multiplication(value, that)

  object `sum and product`:

    def sum(list: List[Nat3]): Nat3 =
      @tailrec
      def sumReq(list: List[Nat3], acc: Nat3): Nat3 =
        list match
          case Nil          => acc
          case head :: tail => sumReq(tail, head + acc)

      sumReq(list, Zero)

    def product(list: List[Nat3]): Nat3 =
      @tailrec
      def productReq(list: List[Nat3], acc: Nat3): Nat3 =
        list match
          case Nil          => acc
          case head :: tail => productReq(tail, head * acc)

      productReq(list, One)

  end `sum and product`

  def fold(operation: (Nat3, Nat3) => Nat3, unit: Nat3)(list: List[Nat3]): Nat3 =
    @tailrec
    def foldReq(list: List[Nat3], acc: Nat3): Nat3 =
      list match
        case Nil          => acc
        case head :: tail => foldReq(tail, operation(head, acc))

    foldReq(list, unit)

  val sumOfElements: List[Nat3] => Nat3     = fold((left, right) => left + right, Zero)
  val productOfElements: List[Nat3] => Nat3 = fold((left, right) => left * right, One)

  extension (nats: List[Nat3])

    @targetName("sum")
    infix def ∑ : Nat3 = sumOfElements(nats)

    @targetName("product")
    infix def ∏ : Nat3 = productOfElements(nats)

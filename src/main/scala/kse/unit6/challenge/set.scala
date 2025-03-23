package kse.unit6.challenge

import kse.unit6.challenge.order.{Order, *, given}
import scala.annotation.targetName

object set:

  trait Set[+A]:

    infix def forAll(predicate: A => Boolean): Boolean

    infix def exists(predicate: A => Boolean): Boolean

    infix def contains[B >: A: Order](x: B): Boolean

    infix def include[B >: A: Order](x: B): Set[B]

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    infix def remove[B >: A: Order](x: B): Set[B]

    @targetName("union")
    infix def ∪[B >: A: Order](that: Set[B]): Set[B]

    @targetName("intersection")
    infix def ∩[B >: A: Order](that: Set[B]): Set[B]

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    @targetName("difference")
    //    infix def \[B >: A: Order](that: Set[B]): Set[B]

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    @targetName("symmetric difference")
    //    infix def ∆[B >: A: Order](that: Set[B]): Set[B] = ???

  end Set

  type Empty = Empty.type

  // TODO: Remind about type system
  case object Empty extends Set[Nothing]:

    infix def forAll(predicate: Nothing => Boolean): Boolean = ???

    infix def exists(predicate: Nothing => Boolean): Boolean = ???

    infix def contains[B: Order](x: B): Boolean = ???

    infix def include[B: Order](x: B): Set[B] = ???

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    infix def remove[B: Order](x: B): Set[B] = ???

    @targetName("union")
    infix def ∪[B: Order](that: Set[B]): Set[B] = ???

    @targetName("intersection")
    infix def ∩[B: Order](that: Set[B]): Set[B] = ???

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    @targetName("difference")
    //    infix def \[B: Order](that: Set[B]): Set[B] = ???

    override def toString: String = "[*]"

    override def equals(obj: Any): Boolean = ???

  end Empty

  case class NonEmpty[A](left: Set[A], element: A, right: Set[A]) extends Set[A]:

    infix def forAll(predicate: A => Boolean): Boolean = ???

    infix def exists(predicate: A => Boolean): Boolean = ???

    infix def contains[B >: A: Order](x: B): Boolean = ???

    infix def include[B >: A: Order](x: B): Set[B] = ???

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    infix def remove[B >: A: Order](x: B): Set[B] = ???

    @targetName("union")
    infix def ∪[B >: A: Order](that: Set[B]): Set[B] = ???

    @targetName("intersection")
    infix def ∩[B >: A: Order](that: Set[B]): Set[B] = ???

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    @targetName("difference")
    //    infix def \[B >: A: Order](that: Set[B]): Set[B] = ???

    override def toString: String = s"[$left - [$element] - $right]"

    override def equals(obj: Any): Boolean = ???

  end NonEmpty

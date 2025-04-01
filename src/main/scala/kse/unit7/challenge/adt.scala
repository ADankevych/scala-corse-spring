package kse.unit7.challenge

object adt:

  enum Try[+V]:

    case DummyCase

    def flatMap[Q](f: V => Try[Q]): Try[Q] = ???

    def map[Q](f: V => Q): Try[Q] = ???

  object Try:
    def apply[V](v: V): Try[V] = ???

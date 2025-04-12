package kse.unit7.topic

object adt:

  enum Option[+V]:

    case Some(x: V) extends Option[V]
    case None       extends Option[Nothing]

    def flatMap[Q](f: V => Option[Q]): Option[Q] =
      this match
        case Option.None    => Option.None
        case Option.Some(v) => f(v)

    def map[Q](f: V => Q): Option[Q] =
      this match
        case Option.None    => Option.None
        case Option.Some(v) => Option.Some(f(v))

  object Option:

    def apply[V](v: V): Option[V] =
      if v == null then Option.None else Option.Some(v)

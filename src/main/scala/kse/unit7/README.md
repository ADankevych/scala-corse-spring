# Unit 7

You are challenged to implement Numerals:
* implement `getPostsViews`, `getPostsViewDesugared`, `getPostView` and `getPostViewDesugared`;
* in `getPostsViews` and `getPostsViewDesugared` it is not allowed to change the result type. Hint: use `fold`/`foldLeft` on `List`. 
* Do not implement `getUserProfile`, `getPosts`, `getComments`, `getLikes` and `getShares`;
* no tests needed for this module;
* github build must be green.


## Enumerations

```scala 3
enum Option[+V]
```

Review
* [Scala 3 Reference. Enumerations](https://docs.scala-lang.org/scala3/reference/enums/enums.html)
* [Scala 3 Book. FP Modeling](https://docs.scala-lang.org/scala3/book/domain-modeling-fp.html)
* [Rock the JVM. Enums in Scala 3: Quickly Explained](https://rockthejvm.com/articles/enums-in-scala-3)

## Algebraic Data Types

```scala 3
object adt:

  enum Option[+V]:
    case Some(x: V) extends Option[V]
    case None extends Option[Nothing]
```

Review
* [Scala 3 Reference. Algebraic Data Types](https://docs.scala-lang.org/scala3/reference/enums/adts.html)
* [Scala 3 Book. Algebraic Data Types](https://docs.scala-lang.org/scala3/book/types-adts-gadts.html)
* [Rock the JVM. ADTs (Algebraic Data Types) in Scala](https://rockthejvm.com/articles/algebraic-data-types-in-scala)
* [Baeldung. Algebraic Data Types in Scala](https://www.baeldung.com/scala/algebraic-data-types)


## Opaque type aliases

```scala 3
  opaque type ApiKey <: UUID = UUID

  object ApiKey:
    def apply(apiKey: ApiKey): ApiKey = apiKey

    def generate: ApiKey = ApiKey(UUID.randomUUID())
```

Review
* [Scala 3 Reference. Opaque Type Aliases](https://docs.scala-lang.org/scala3/reference/other-new-features/opaques.html)
* [Scala 3 Book. Opaque Types](https://docs.scala-lang.org/scala3/book/types-opaque-types.html)
* [Baeldung. Opaque Type Alias in Scala 3](https://www.baeldung.com/scala/opaque-type-alias)
* [Rock the JVM. Scala 3: Opaque Types Quickly Explained](https://rockthejvm.com/articles/scala-3-opaque-types)

## For comprehension

```scala 3
def getClientAccounts(apiKey: ApiKey): Option[ClientTransactions] =
  for
    client       <- getClient(apiKey)
    bankAccounts <- getBankAccounts(client.clientId)
    transactions <- getTransaction(bankAccounts.map(_.bankAccountId))
  yield ClientTransactions(client.clientId, transactions)
```

```scala 3
def getClientId(apiKey: ApiKey): Option[ClientId] =
  for client <- getClient(apiKey)
  yield client.clientId

def getClientIdDesugared(apiKey: ApiKey): Option[ClientId] =
  getClient(apiKey: ApiKey) map { client =>
    client.clientId
  }
```

Review
* [Scala 3 Book. For comprehension](https://docs.scala-lang.org/tour/for-comprehensions.html)
* [Baeldung. A Comprehensive Guide to For-Comprehension in Scala](https://www.baeldung.com/scala/for-comprehension)
* [Medium. Scala: comprehending the for-comprehension](https://medium.com/wix-engineering/scala-comprehending-the-for-comprehension-67c9f7953655)

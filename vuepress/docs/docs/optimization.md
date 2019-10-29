# Query optimization
A GraphQL query may request multiple fields that are using the same resolver. It's not a problem if the resolver is a simple value, but it can be less than optimal when the field runs an effect (such as reading from a database).

We might want to:
- **cache** identical queries (deduplication)
- **batch** queries to the same source

This is possible in Caliban using the `ZQuery` data type. 

## Introducing ZQuery
A `ZQuery[R, E, A]` is a purely functional description of an effectual query that may contain requests from one or more data sources. Similarly to `ZIO[R, E, A]`, it requires an environment `R`, may fail with an `E` or succeed with an `A`. All requests that do not need to be performed sequentially will automatically be batched, allowing for aggressive data source specific optimizations. Requests will also automatically be deduplicated and cached.

This allows for writing queries in a high level, compositional style, with confidence that they will automatically be optimized. For example, consider the following query from a user service.

```scala
val getAllUserIds: ZQuery[Any, Nothing, List[Int]] = ???
def getUserNameById(id: Int): ZQuery[Any, Nothing, String] = ???

for {
  userIds   <- getAllUserIds
  userNames <- ZQuery.foreachPar(userIds)(getUserNameById)
} yield userNames
```

This would normally require N + 1 queries, one for `getAllUserIds` and one for each call to `getUserNameById`. In contrast, `ZQuery` will automatically optimize this to two queries, one for `userIds` and one for `userNames`, assuming an implementation of the user service that supports batching.

## Building a DataSource
To build a `ZQuery` that actually execute a request, you first need to build a `DataSource`. A `DataSource[R, E, A]` defines how to execute requests of type `A` and require 2 things:
- an `identifier` that uniquely identifies the data source (requests from different data sources will not be batched together)
- a effectful function `run` from a list of requests to a `Map` of requests and results

Let's consider `getUserNameById` from the previous example. We need to define a corresponding request type:
```scala
case class GetUserName(id: Int) extends Request[String]
```

Now let's build the corresponding `DataSource`. We need to implement the following functions:
```scala
val UserDataSource = new DataSource.Service[Any, Throwable, GetUserName] {
  override val identifier: String = ???
  override def run(requests: Iterable[GetUserName]): ZIO[Any, Throwable, CompletedRequestMap] = ???
}
```

We will use "UserDataSource" as our identifier:
```scala
override val identifier: String = "UserDataSource"
```

We will define two different behaviors depending on whether we receive a single request or multiple requests at once.
```scala
override def run(requests: Iterable[GetUserName]): ZIO[Any, Throwable, CompletedRequestMap] = {
  val resultMap = CompletedRequestMap.empty
  requests.toList match {
    case request :: Nil =>
      // get user by ID e.g. SELECT name FROM users WHERE id = $id
      val result: Task[String] = ???
      result.map(resultMap.insert(request))
    case batch =>
      // get multiple users at once e.g. SELECT id, name FROM users WHERE id IN ($ids)
      val result: Task[List[(Int, String)]] = ???
      result.map(_.foldLeft(resultMap) { case (map, (id, name)) => map.insert(GetUserName(id))(name) })
  }
}
```

Now to build a `ZQuery` from it, we can use `ZQuery.fromRequestWith` and just pass the request and the data source:
```scala
def getUserNameById(id: Int): ZQuery[Any, Throwable, String] =
  ZQuery.fromRequestWith(GetUserName(id))(UserDataSource)
```

To run a `ZQuery`, simply use `ZQuery#run` which will return a `ZIO[R, E, A]`.

## ZQuery constructors and operators
There are several ways to create a `ZQuery`. We've seen `ZQuery.fromRequestWith`, but you can also:
- create a `ZQuery` from a pure value with `ZQuery.succeed`
- create a `ZQuery` from an effect value with `ZQuery.fromEffect`
- create a `ZQuery` from multiple queries with `ZQuery.collectAllPar` and `ZQuery.foreachPar` and their sequential equivalents `ZQuery.collectAll` and `ZQuery.foreach`

If you have a `ZQuery` object, you can use:
- `map` and `mapError` to modify the returned result or error
- `flatMap` or `zip` to combine it with other `ZQuery` objects
- `provide` and `provideSome` to eliminate some of the R requirements

There are several ways to run a `ZQuery`:
- `runCache` runs the query using a given pre-populated cache. This can be useful for deterministically "replaying" a query without executing any new requests.
- `runLog` runs the query and returns its result along with the cache containing a complete log of all requests executed and their results. This can be useful for logging or analysis of query execution.
- `run` runs the query and returns its result.

## Using ZQuery with Caliban
To use ZQuery with Caliban, you can simply include fields of type `ZQuery` in your API definition.
```scala
case class Queries(
  users: ZQuery[Any, Nothing, List[User]],
  user: UserArgs => ZQuery[Any, Nothing, User])
```
During a query execution, Caliban will merge all the requested fields that return a `ZQuery` into a single `ZQuery` and run it, so that all the possible optimizations are applied.

The [examples](https://github.com/ghostdogpr/caliban/tree/master/examples) project provides 2 versions of the problem described in [this article about GraphQL query optimization](https://blog.apollographql.com/optimizing-your-graphql-request-waterfalls-7c3f3360b051):
- a [naive](https://github.com/ghostdogpr/caliban/tree/master/examples/src/main/scala/caliban/optimizations/NaiveTest.scala) version where fields are just returning `IO`, resulting in 47 requests
- an [optimized](https://github.com/ghostdogpr/caliban/tree/master/examples/src/main/scala/caliban/optimizations/OptimizedTest.scala) version where fields are returning `ZQuery`, resulting in 8 requests only
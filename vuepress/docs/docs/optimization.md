# Query optimization

A GraphQL query may request multiple fields that are using the same resolver. It's not a problem if the resolver is a simple value, but it can be less than optimal when the resolver runs an effect (such as reading from a database).

We might want to:

- **cache** identical queries (deduplication)
- **batch** queries to the same source

This is possible in Caliban using the [`ZQuery`](https://github.com/zio/zquery) data type.

Additionally, one may want to perform optimizations based on the fields selected by the client. 
This optimization can be achieved by field metadata from Caliban that can be referenced in your query classes.   

## Introducing ZQuery

A `ZQuery[R, E, A]` is a purely functional description of an effectual query that may contain requests to one or more data sources. Similarly to `ZIO[R, E, A]`, it requires an environment `R`, may fail with an `E` or succeed with an `A`. All requests that do not need to be performed sequentially will automatically be batched, allowing for aggressive data source specific optimizations. Requests will also automatically be deduplicated and cached.

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

To build a `ZQuery` that executes a request, you first need to build a `DataSource`. A `DataSource[R, E, A]` defines how to execute requests of type `A` and it requires 2 things:

- an `identifier` that uniquely identifies the data source (requests from _different_ data sources will _not_ be batched together)
- a effectful function `run` from an `Iterable` of requests to a `Map` of requests and results

Let's consider `getUserNameById` from the previous example. We need to define a corresponding request type that extends `zquery.Request` for a given response type:

```scala
case class GetUserName(id: Int) extends Request[Throwable, String]
```

Now let's build the corresponding `DataSource`. We need to implement the following functions:

```scala
val UserDataSource = new DataSource[Any, GetUserName] {
  override val identifier: String = ???
  override def run(requests: Iterable[GetUserName]): ZIO[Any, Throwable, CompletedRequestMap] = ???
}
```

We will use "UserDataSource" as our identifier. This name should not be reused for other data sources.

```scala
override val identifier: String = "UserDataSource"
```

We will define two different behaviors depending on whether we receive a single request or multiple requests at once.
For each request, we need to insert into the result map a value of type `Either` (`Left` for an error and `Right` for a success).

```scala
override def run(requests: Iterable[GetUserName]): ZIO[Any, Nothing, CompletedRequestMap] = {
  val resultMap = CompletedRequestMap.empty
  requests.toList match {
    case request :: Nil =>
      // get user by ID e.g. SELECT name FROM users WHERE id = $id
      val result: Task[String] = ???
      result.either.map(resultMap.insert(request))
    case batch =>
      // get multiple users at once e.g. SELECT id, name FROM users WHERE id IN ($ids)
      val result: Task[List[(Int, String)]] = ???
      result.fold(
        err => requests.foldLeft(resultMap) { case (map, req) => map.insert(req)(Left(err)) },
        _.foldLeft(resultMap) { case (map, (id, name)) => map.insert(GetUserName(id))(Right(name)) }
      )
  }
}
```

Now to build a `ZQuery` from it, we can use `ZQuery.fromRequest` and just pass the request and the data source:

```scala
def getUserNameById(id: Int): ZQuery[Any, Throwable, String] =
  ZQuery.fromRequest(GetUserName(id))(UserDataSource)
```

To run a `ZQuery`, simply use `ZQuery#run` which will return a `ZIO[R, E, A]`.

## ZQuery constructors and operators

There are several ways to create a `ZQuery`. We've seen `ZQuery.fromRequest`, but you can also:

- create from a pure value with `ZQuery.succeed`
- create from an effect value with `ZQuery.fromEffect`
- create from multiple queries with `ZQuery.collectAllPar` and `ZQuery.foreachPar` and their sequential equivalents `ZQuery.collectAll` and `ZQuery.foreach`

If you have a `ZQuery` object, you can use:

- `map` and `mapError` to modify the returned result or error
- `flatMap` or `zip` to combine it with other `ZQuery` objects
- `provide` and `provideSome` to eliminate some of the `R` requirements

There are several ways to run a `ZQuery`:

- `runCache` runs the query using a given pre-populated cache. This can be useful for deterministically "replaying" a query without executing any new requests.
- `runLog` runs the query and returns its result along with the cache containing a complete log of all requests executed and their results. This can be useful for logging or analysis of query execution.
- `run` runs the query and returns its result.

## Using ZQuery with Caliban

To use `ZQuery` with Caliban, you can simply include fields of type `ZQuery` in your API definition.

```scala
case class Queries(
  users: ZQuery[Any, Nothing, List[User]],
  user: UserArgs => ZQuery[Any, Nothing, User])
```

During the query execution, Caliban will merge all the requested fields that return a `ZQuery` into a single `ZQuery` and run it, so that all the possible optimizations are applied.

The [examples](https://github.com/ghostdogpr/caliban/tree/master/examples) project provides 2 versions of the problem described in [this article about GraphQL query optimization](https://blog.apollographql.com/optimizing-your-graphql-request-waterfalls-7c3f3360b051):

- a [naive](https://github.com/ghostdogpr/caliban/tree/master/examples/src/main/scala/example/optimizations/NaiveTest.scala) version where fields are just returning `IO`, resulting in 47 requests
- an [optimized](https://github.com/ghostdogpr/caliban/tree/master/examples/src/main/scala/example/optimizations/OptimizedTest.scala) version where fields are returning `ZQuery`, resulting in 8 requests only

::: tip
When all your effects are wrapped with `ZQuery.fromRequest`, it is recommended to use `queryExecution = QueryExecution.Batched` instead of the default `QueryExecution.Parallel`.
Doing so will provide better performance as it will avoid forking unnecessary fibers.
This setting is available in `executeRequest` as well as all the adapters.
:::

## Using field metadata 

To reference field metadata in your queries you can simply use a function that takes the `caliban.execution.Field` type in your queries.

```scala
case class User(name: String, expensiveOperation: String)
case class Queries(
  user: Field => User
)
```

You can also do this with functions that take inputs.

```scala
case class UserInput(id: String)
case class User(name: String, expensiveOperation: String)
case class Queries(
  user: Field => (UserInput => User)
)
```

In the resulting GraphQL Schema the *Field* will be ignored giving you the equivalent of just the returned type of the function.

The implementation of the function can then take the field metadata into account for optimization.
For instance one could modify a database query to only select certain columns or do joins to additional tables depending on what the client requests.

For example:
```scala
Queries( (field) => {
  if(field.fields.map(_.name).contains("expensiveOperation")) {
    expensiveUserRequest()
  } else {
    efficientUserRequest()
  }
})
``` 
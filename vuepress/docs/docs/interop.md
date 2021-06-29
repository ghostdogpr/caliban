# Interop

If you prefer using [Cats Effect](https://github.com/typelevel/cats-effect) or [Monix](https://github.com/monix/monix) rather than ZIO, you can use the respective `caliban-cats` and `caliban-monix` modules.

The `caliban-tapir` module allows converting your [Tapir](https://github.com/softwaremill/tapir) endpoints into a GraphQL API.

## Cats Effect
You first need to import `caliban.interop.cats.implicits._` and have an implicit `zio.Runtime` in scope. Then a few helpers are available:

- the `GraphQL` object is enriched with `interpreterAsync`, a variant of `interpreter` that return an `F[_]: Async` instead of a `ZIO`.
- the `GraphQLInterpreter` object is enriched with `executeAsync` and `checkAsync`, variants of `execute` and `check` that return an `F[_]: Async` instead of a `ZIO`.
- the `Http4sAdapter` also has cats-effect variants named `makeRestServiceF` and `makeWebSocketServiceF`.

In addition to that, a `Schema` for any `F[_]: Effect` is provided. That means you can include fields with results wrapped in `F` in your queries, mutations or subscriptions.

The following example shows how to create an interpreter and run a query while only using Cats IO.

```scala
import caliban.GraphQL.graphQL
import caliban.RootResolver
import caliban.interop.cats.implicits._
import cats.effect.{ ExitCode, IO, IOApp }
import zio.Runtime

object ExampleCatsInterop extends IOApp {

  implicit val runtime = Runtime.default

  case class Queries(numbers: List[Int], randomNumber: IO[Int])

  val queries     = Queries(List(1, 2, 3, 4), IO(scala.util.Random.nextInt()))
  val api = graphQL(RootResolver(queries))

  val query = """
  {
    numbers
    randomNumber
  }"""

  override def run(args: List[String]): IO[ExitCode] =
    for {
      interpreter <- api.interpreterAsync[IO]
      result      <- interpreter.executeAsync[IO](query)
      _           <- IO(println(result.data))
    } yield ExitCode.Success
}
```

You can find this example within the [examples](https://github.com/ghostdogpr/caliban/blob/master/examples/src/main/scala/example/interop/cats/ExampleCatsInterop.scala) project.

## Monix
You first need to import `caliban.interop.monix.implicits._` and have an implicit `zio.Runtime` in scope. Then a few helpers are available:

- the `GraphQL` object is enriched with `interpreterAsync`, a variant of `interpreter` that return a Monix `Task` instead of a `ZIO`.
- the `GraphQLInterpreter` object is enriched with `executeAsync` and `checkAsync`, variants of `execute` and `check` that return a Monix `Task` instead of a `ZIO`.

In addition to that, a `Schema` for any Monix `Task` as well as `Observable` is provided.

The following example shows how to create an interpreter and run a query while only using Monix Task.

```scala
import caliban.GraphQL.graphQL
import caliban.RootResolver
import caliban.interop.monix.implicits._
import cats.effect.ExitCode
import monix.eval.{ Task, TaskApp }
import monix.execution.Scheduler
import zio.Runtime

object ExampleMonixInterop extends TaskApp {

  implicit val runtime = Runtime.default
  implicit val monixScheduler: Scheduler = scheduler

  case class Queries(numbers: List[Int], randomNumber: Task[Int])

  val queries     = Queries(List(1, 2, 3, 4), Task.eval(scala.util.Random.nextInt()))
  val api = graphQL(RootResolver(queries))

  val query = """
  {
    numbers
    randomNumber
  }"""

  override def run(args: List[String]): Task[ExitCode] =
    for {
      interpreter <- api.interpreterAsync
      result      <- interpreter.executeAsync(query)
      _           <- Task.eval(println(result.data))
    } yield ExitCode.Success
}
```

You can find this example within the [examples](https://github.com/ghostdogpr/caliban/blob/master/examples/src/main/scala/example/interop/monix/ExampleMonixInterop.scala) project.

## Tapir

After adding the `caliban-tapir` dependency to your build, adding `import caliban.interop.tapir._` to your code will introduce an extension method called `toGraphQL` on Tapir's `Endpoint` and `ServerEndpoint`.
This method will convert your endpoint into a `GraphQL` object that you can then combine and expose.

The conversion rules are the following:
- `GET` endpoints are turned into Queries
- `PUT`, `POST` and `DELETE` endpoints are turned into Mutations
- fixed query paths are used to name GraphQL fields (e.g. an endpoint `/book/add` will give a GraphQL field named `bookAdd`)
- query parameters, headers, cookies and request body are used as GraphQL arguments
- there should be an implicit `Schema` for both the input and the output types and an implicit `ArgBuilder` for the input type (see the [dedicated docs](schema.md))

Let's look at an example. Imagine we have the following Tapir endpoint:
```scala
val addBook: Endpoint[(Book, String), Nothing, Unit, Nothing] =
  infallibleEndpoint
    .post
    .in("books")
    .in("add")
    .in(
      jsonBody[Book]
        .description("The book to add")
        .example(Book("Pride and Prejudice", 1813))
    )
    .in(header[String]("X-Auth-Token").description("The token is 'secret'"))
```

And a possible implementation:
```scala
def bookAddLogic(book: Book, token: String): UIO[Unit] = ???
```

Just like you can create an http4s route by calling `toRoute` and passing an implementation, call `toGraphQL` to create a GraphQL API:
```scala
val api: GraphQL[Any] = addBook.toGraphQL((bookAddLogic _).tupled)
```
That's it! You can combine multiple `GraphQL` objects using `|+|` and expose the result using one of Caliban's adapters.

If you want to reuse `bookAddLogic` for both GraphQL and regular HTTP, you can turn your `Endpoint` into a `ServerEndpoint` by calling `.serverLogic`:
```scala
val addBookEndpoint: ServerEndpoint[(Book, String), Nothing, Unit, Nothing, UIO] =
  addBook.serverLogic[UIO] { case (book, token) => bookAddLogic(book, token).either }
```
This can then be used to generate both an HTTP route (e.g. `toRoutes` with http4s) and a GraphQL API (`.toGraphQL`).
```scala
val api: GraphQL[Any] = addBookEndpoint.toGraphQL
```

You can find a [full example](https://github.com/ghostdogpr/caliban/tree/master/examples/src/main/scala/example/tapir) on github.

### GraphQL restrictions

[GraphQL spec](https://spec.graphql.org/June2018/#sec-Operation-Name-Uniqueness) requires unique naming for all operations.

To customize the [name](https://github.com/softwaremill/tapir/blob/master/core/src/main/scala/sttp/tapir/Endpoint.scala#L287) of an operation use `EndpointInfo.name`

```scala
endpoint
  .name("overrideName")
``` 

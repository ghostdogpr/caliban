# Interop

If you prefer using [Cats Effect](https://github.com/typelevel/cats-effect) or [Monix](https://github.com/monix/monix) rather than ZIO, you can use the respective `caliban-cats` and `caliban-monix` modules.

The `caliban-tapir` module allows converting your [Tapir](https://github.com/softwaremill/tapir) endpoints into a GraphQL API.

## Cats Effect

You first need to import `caliban.interop.cats.implicits._` and have an implicit `zio.Runtime` in scope. Then a few helpers are available:

- the `GraphQL` object is enriched with `interpreterAsync`, a variant of `interpreter` that return an `F[_]: Async` instead of a `ZIO`.
- the `GraphQLInterpreter` object is enriched with `executeAsync` and `checkAsync`, variants of `execute` and `check` that return an `F[_]: Async` instead of a `ZIO`.
- the `Http4sAdapter` also has a helper to turn endpoints into cats-effect named `convertHttpEndpointToF`.

In addition to that, a `Schema` for any `F[_]: Async: Dispatcher` is provided. That means you can include fields with results wrapped in `F` in your queries, mutations or subscriptions.

There are two type classes responsible for the conversion between effects: `caliban.interop.cats.ToEffect` and `caliban.interop.cats.FromEffect`.
The instances are derived implicitly when `Async[F]`, `Dispatcher[F]`, and `Runtime[R]` are available in the implicit scope.

#### Interop with cats.effect.IO

The following example shows how to create an interpreter and run a query while only using Cats IO.

```scala mdoc:silent
import caliban.GraphQL.graphQL
import caliban.RootResolver
import caliban.interop.cats.implicits._
import caliban.schema.auto._
import cats.effect.{ ExitCode, IO, IOApp }
import cats.effect.std.Dispatcher
import zio.Runtime

object ExampleCatsInterop extends IOApp {

  implicit val zioRuntime = Runtime.default

  case class Queries(numbers: List[Int], randomNumber: IO[Int])

  val queries = Queries(List(1, 2, 3, 4), IO(scala.util.Random.nextInt()))

  val query = """
  {
    numbers
    randomNumber
  }"""

  override def run(args: List[String]): IO[ExitCode] =
    Dispatcher[IO].use { implicit dispatcher => // required for a derivation of the schema
      val api = graphQL(RootResolver(queries))

      for {
        interpreter <- api.interpreterAsync[IO]
        result      <- interpreter.executeAsync[IO](query)
        _           <- IO(println(result.data))
      } yield ExitCode.Success
    }
}
```

You can find this example within the [examples](https://github.com/ghostdogpr/caliban/blob/master/examples/src/main/scala/example/interop/cats/ExampleCatsInterop.scala) project.

#### Interop with contextual effect (e.g. Kleisli)

`CatsInterop` (the combination of `ToEffect` and `FromEffect`) allows sharing a context between cats-effect and ZIO:

```scala mdoc:compile-only
import cats.data.Kleisli
import cats.effect.IO
import cats.effect.std.Dispatcher
import caliban.interop.cats.CatsInterop
import zio.RIO

trait Context
type Effect[A] = Kleisli[IO, Context, A]

implicit val dispatcher: Dispatcher[Effect] = ???
implicit val runtime: Runtime[Context] = ???

val interop: CatsInterop.Contextual[Effect, Context] = CatsInterop.contextual(dispatcher)

val rio: RIO[Context, Int] = ???
val ce: Kleisli[IO, Context, Int] = ???

val fromRIO: Kleisli[IO, Context, Int] = interop.toEffect(rio)
val fromCE: RIO[Context, Int] = interop.fromEffect(ce)
```

```scala mdoc:silent
import caliban.GraphQL.graphQL
import caliban.{ GraphQL, RootResolver }
import caliban.interop.cats._
import caliban.interop.cats.implicits._
import caliban.schema.GenericSchema
import cats.data.Kleisli
import cats.effect.{ Async, ExitCode, IO, IOApp }
import cats.effect.std.Dispatcher
import cats.effect.std.Console
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.mtl.Local
import cats.mtl.syntax.local._
import zio.{ Runtime, ZEnvironment }

object Simple extends IOApp {

  case class Queries[F[_]](numbers: List[Int], randomNumber: F[Int])

  val query = """
  {
    numbers
    randomNumber
  }"""

  case class TraceId(value: String)

  type TraceLocal[F[_]] = Local[F, TraceId]

  trait Logger[F[_]] {
    def info(message: String): F[Unit]
  }

  def program[F[_]: Async](implicit
    logger: Logger[F],
    local: Local[F, TraceId],
    inject: InjectEnv[F, TraceId],
    runtime: Runtime[TraceId]
  ): F[ExitCode] =
    Dispatcher[F].use { implicit dispatcher =>
      implicit val interop: CatsInterop.Contextual[F, TraceId] = CatsInterop.contextual(dispatcher) // required for a derivation of the schema

      val genRandomNumber = logger.info("Generating number") >> Async[F].delay(scala.util.Random.nextInt())

      val queries = Queries(
        List(1, 2, 3, 4),
        genRandomNumber.scope[TraceId](TraceId("gen-number"))
      )

      val api: GraphQL[TraceId] = {
        val schema: GenericSchema[TraceId] = new GenericSchema[TraceId] {}
        import schema.auto._

        graphQL(RootResolver(queries))
      }

      for {
        interpreter <- api.interpreterAsync[F]
        result      <- interpreter.executeAsync[F](query)
        _           <- logger.info(result.data.toString)
      } yield ExitCode.Success
    }

  override def run(args: List[String]): IO[ExitCode] = {
    type Effect[A] = Kleisli[IO, TraceId, A]

    val root = TraceId("root")

    implicit val runtime = Runtime.default.withEnvironment(ZEnvironment(root))
    implicit val logger  = new Logger[Effect] {
      def info(message: String): Effect[Unit] =
        for {
          traceId <- Local[Effect, TraceId].ask[TraceId]
          _       <- Console[Effect].println(s"$message - $traceId")
        } yield ()
    }

    program[Effect].run(root)
  }
}
```

There is another real world [example](https://github.com/ghostdogpr/caliban/blob/master/examples/src/main/scala/example/http4s/AuthExampleAppF.scala), that shows how to share auth info between cats-effect and ZIO.

## Monix (only with cats-effect 2.x)

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

  implicit val zioRuntime = Runtime.default
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

```scala mdoc:silent:reset
import io.circe.generic.auto._
import sttp.tapir._
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe._

case class Book(title: String, year: Int)

val addBook: PublicEndpoint[(Book, String), Nothing, Unit, Any] =
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

```scala mdoc:silent
import zio.UIO

def bookAddLogic(book: Book, token: String): UIO[Unit] = ???
```

Just like you can create an http4s route by calling `toRoute` and passing an implementation, call `toGraphQL` to create a GraphQL API:

```scala mdoc:silent
import caliban.GraphQL
import caliban.interop.tapir._ // summons 'toGraphQL' extension
import caliban.schema.ArgBuilder.auto._
import caliban.schema.auto._

val api: GraphQL[Any] = addBook.toGraphQL((bookAddLogic _).tupled)
```

That's it! You can combine multiple `GraphQL` objects using `|+|` and expose the result using one of Caliban's adapters.

If you want to reuse `bookAddLogic` for both GraphQL and regular HTTP, you can turn your `Endpoint` into a `ServerEndpoint` by calling `.serverLogicSuccess`:

```scala mdoc:silent
import sttp.tapir.server.ServerEndpoint

val addBookEndpoint: ServerEndpoint.Full[Unit, Unit, (Book, String), Nothing, Unit, Any, UIO] =
  addBook.serverLogicSuccess[UIO] { case (book, token) => bookAddLogic(book, token) }
```

This can then be used to generate both an HTTP route (e.g. `toRoutes` with http4s) and a GraphQL API (`.toGraphQL`).

```scala mdoc:silent:nest
import caliban.schema.auto._
import caliban.schema.ArgBuilder.auto._

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

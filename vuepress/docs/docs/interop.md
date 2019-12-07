# Interop (Cats)

If you prefer using [Monix](https://github.com/monix/monix) or [Cats IO](https://github.com/typelevel/cats-effect) rather than ZIO, you can use the `caliban-cats` module.

You first need to import `caliban.interop.cats.implicits._` and have an implicit `zio.Runtime` in scope. Then a few helpers are available:

- the GraphQL object is enriched with `executeAsync` and `checkAsync`, variants of `execute` and `check` that return an `F[_]: Async` instead of a `ZIO`.
- the `Http4sAdapter` also has cats-effect variants named `makeRestServiceF` and `makeWebSocketServiceF`.

In addition to that, a `Schema` for any `F[_]: Effect` is provided. That means you can include fields returning Monix Task for Cats IO in your queries, mutations or subscriptions.

The following example shows how to create an interpreter and run a query while only using Cats IO.

```scala
import caliban.GraphQL.graphQL
import caliban.RootResolver
import caliban.interop.cats.implicits._
import cats.effect.{ ExitCode, IO, IOApp }
import zio.DefaultRuntime

object ExampleCatsInterop extends IOApp {

  implicit val runtime = new DefaultRuntime {}

  case class Queries(numbers: List[Int], randomNumber: IO[Int])

  val queries     = Queries(List(1, 2, 3, 4), IO(scala.util.Random.nextInt()))
  val interpreter = graphQL(RootResolver(queries))

  val query = """
  {
    numbers
    randomNumber
  }"""

  override def run(args: List[String]): IO[ExitCode] =
    for {
      result <- interpreter.executeAsync[IO](query)
      _      <- IO(println(result.data))
    } yield ExitCode.Success
}
```

You can find this example within the [examples](https://github.com/ghostdogpr/caliban/blob/master/examples/src/main/scala/caliban/interop/cats/ExampleCatsInterop.scala) project.

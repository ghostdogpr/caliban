package caliban.interop.cats

import caliban.GraphQL.graphQL
import caliban.RootResolver
import cats.effect.{ExitCode, IO, IOApp}
import zio.DefaultRuntime

object ExampleCatsInterop extends IOApp {

  import caliban.interop.cats.implicits._

  implicit val runtime = new DefaultRuntime {}

  case class Number(value: Int)

  case class Queries(numbers: List[Number])

  val query = """
  {
    numbers {
      value
    }
  }"""

  override def run(args: List[String]): IO[ExitCode] = {
    val queries = Queries(List(1, 2, 3, 4).map(Number))
    val interpreter = graphQL(RootResolver(queries))

    for {
      result <- interpreter.executeAsync[IO](query)
      _ <- IO(println(result))
    } yield ExitCode.Success
  }
}

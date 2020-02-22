package caliban.interop.cats

import caliban.GraphQL.graphQL
import caliban.RootResolver
import cats.effect.{ ExitCode, IO, IOApp }
import zio.DefaultRuntime

object ExampleCatsInterop extends IOApp {

  import caliban.interop.cats.implicits._

  implicit val runtime = new DefaultRuntime {}

  case class Number(value: Int)

  case class Queries(numbers: List[Number], randomNumber: IO[Number])

  val numbers      = List(1, 2, 3, 4).map(Number)
  val randomNumber = IO(scala.util.Random.nextInt()).map(Number)

  val queries = Queries(numbers, randomNumber)
  val api     = graphQL(RootResolver(queries))

  val query = """
  {
    numbers {
      value
    }

    randomNumber {
      value
    }
  }"""

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _           <- api.checkAsync[IO](query)
      interpreter <- api.interpreterAsync[IO]
      result      <- interpreter.executeAsync[IO](query)
      _           <- IO(println(result.data))
    } yield ExitCode.Success
}

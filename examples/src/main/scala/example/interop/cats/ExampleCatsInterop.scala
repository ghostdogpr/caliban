package example.interop.cats

import caliban.GraphQL.graphQL
import caliban.RootResolver

import cats.effect.{ ExitCode, IO, IOApp }
import zio.{ Runtime, ZEnv }

object ExampleCatsInterop extends IOApp {

  import caliban.interop.cats.implicits._

  implicit val runtime: Runtime[ZEnv] = Runtime.default

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
      interpreter <- api.interpreterAsync[IO]
      _           <- interpreter.checkAsync[IO](query)
      result      <- interpreter.executeAsync[IO](query)
      _           <- IO(println(result.data))
    } yield ExitCode.Success
}

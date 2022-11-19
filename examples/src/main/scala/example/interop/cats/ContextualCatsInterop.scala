package example.interop.cats

import caliban.GraphQL.graphQL
import caliban.interop.cats.CatsInterop
import caliban.schema.GenericSchema
import caliban.{ GraphQL, RootResolver }
import cats.data.Kleisli
import cats.effect.std.{ Console, Dispatcher }
import cats.effect.{ Async, ExitCode, IO, IOApp }
import cats.mtl.Local
import cats.mtl.syntax.local._
import cats.syntax.flatMap._
import cats.syntax.functor._
import zio.{ Runtime, ZEnvironment }

object ContextualCatsInterop extends IOApp {

  import caliban.interop.cats.implicits._

  case class Number(value: Int)

  case class Queries[F[_]](numbers: List[Number], randomNumber: F[Number])

  val query = """
  {
    numbers {
      value
    }

    randomNumber {
      value
    }
  }"""

  case class LogContext(operation: String) {
    def child(next: String): LogContext = LogContext(s"$operation -> $next")
  }

  /**
   * The example shows the propagation of the `LogContext` from cats-effect to ZIO and vice-versa.
   *
   * Console output:
   * Executing request - root
   * Get random number - root -> execute-request
   * Generating a random number - root -> execute-request -> random-number
   * Generated number: 485599760 - root -> execute-request -> random-number
   * Request result: {"numbers":[{"value":1},{"value":2},{"value":3},{"value":4}],"randomNumber":{"value":485599760}} - root
   */
  override def run(args: List[String]): IO[ExitCode] = {
    type Effect[A] = Kleisli[IO, LogContext, A]

    val root = LogContext("root")

    Dispatcher
      .parallel[Effect]
      .use { dispatcher =>
        implicit val logger: Logger[Effect] =
          (message: String) =>
            for {
              ctx <- Local[Effect, LogContext].ask[LogContext]
              _   <- Console[Effect].println(s"$message - ${ctx.operation}")
            } yield ()

        implicit val zioRuntime: Runtime[LogContext]          = Runtime.default.withEnvironment(ZEnvironment(root))
        implicit val interop: CatsInterop[Effect, LogContext] = CatsInterop.contextual(dispatcher)

        program[Effect]
      }
      .run(root)
  }

  def program[F[_]: Async: Logger](implicit
    local: Local[F, LogContext],
    interop: CatsInterop[F, LogContext], // required for a derivation of the schema
    runtime: Runtime[LogContext]
  ): F[ExitCode] = {
    val numbers = List(1, 2, 3, 4).map(Number)

    val randomNumber =
      Logger[F].info("Get random number") >> {
        for {
          _      <- Logger[F].info("Generating a random number")
          number <- Async[F].delay(scala.util.Random.nextInt())
          _      <- Logger[F].info(s"Generated number: $number")
        } yield Number(number)
      }.local[LogContext](_.child("random-number"))

    val queries = Queries[F](numbers, randomNumber)

    val api: GraphQL[LogContext] = {
      object ContextSchema extends GenericSchema[LogContext]
      import ContextSchema._ // required for a derivation of the schema

      graphQL(RootResolver(queries))
    }

    for {
      interpreter <- api.interpreterAsync[F]
      _           <- interpreter.checkAsync[F](query)
      _           <- Logger[F].info("Executing request")
      result      <- interpreter.executeAsync[F](query)(interop).local[LogContext](_.child("execute-request"))
      _           <- Logger[F].info(s"Request result: ${result.data}")
    } yield ExitCode.Success
  }

  trait Logger[F[_]] {
    def info(message: String): F[Unit]
  }
  object Logger      {
    def apply[F[_]](implicit ev: Logger[F]): Logger[F] = ev
  }

}

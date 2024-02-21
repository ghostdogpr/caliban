package example.interop.cats

import caliban._
import caliban.interop.cats.CatsInterop
import caliban.schema.GenericSchema
import cats.effect.std.{ Console, Dispatcher }
import cats.effect.{ ExitCode, IO, IOApp, IOLocal }
import zio.{ Runtime, ZEnvironment }

object ContextualCatsInteropIO extends IOApp {

  import caliban.interop.cats.implicits._

  case class Number(value: Int)

  case class Queries(numbers: List[Number], randomNumber: IO[Number])

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
    val root = LogContext("root")
    IOLocal(root).flatMap { implicit local =>
      Dispatcher
        .parallel[IO]
        .use { dispatcher =>
          implicit val logger: Logger =
            (message: String) =>
              for {
                ctx <- local.get
                _   <- Console[IO].println(s"$message - ${ctx.operation}")
              } yield ()

          implicit val zioRuntime: Runtime[LogContext]      = Runtime.default.withEnvironment(ZEnvironment(root))
          implicit val interop: CatsInterop[IO, LogContext] = CatsInterop.contextual(dispatcher)

          program
        }
    }
  }

  def program(implicit
    local: IOLocal[LogContext],
    interop: CatsInterop[IO, LogContext], // required for a derivation of the schema
    runtime: Runtime[LogContext],
    log: Logger
  ): IO[ExitCode] = {
    val numbers = List(1, 2, 3, 4).map(Number)

    def locally[A](io: IO[A])(updateCtx: LogContext => LogContext): IO[A] =
      for {
        ctx <- local.get
        _   <- local.update(updateCtx)
        out <- io
        _   <- local.set(ctx)
      } yield out

    val randomNumber =
      log.info("Get random number") >> locally {
        for {
          _      <- log.info("Generating a random number")
          number <- IO(scala.util.Random.nextInt())
          _      <- log.info(s"Generated number: $number")
        } yield Number(number)
      }(_.child("random-number"))

    val queries = Queries(numbers, randomNumber)

    val api: GraphQL[LogContext] = {
      object ContextSchema extends GenericSchema[LogContext]
      import ContextSchema.auto._ // required for a derivation of the schema

      graphQL(RootResolver(queries))
    }

    for {
      interpreter <- api.interpreterAsync[IO]
      _           <- interpreter.checkAsync[IO](query)
      _           <- log.info("Executing request")
      result      <- locally(interpreter.executeAsync[IO](query)(interop))(_.child("execute-request"))
      _           <- log.info(s"Request result: ${result.data}")
    } yield ExitCode.Success
  }

  trait Logger {
    def info(message: String): IO[Unit]
  }

}

package caliban

import cats.effect.IO
import io.circe.Json
import org.openjdk.jmh.annotations._
import sangria.execution._
import sangria.marshalling.circe._
import sangria.parser.QueryParser

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContextExecutor, Future }

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class SimpleQueryBenchmark {
  import SimpleQueryBenchmark._

  implicit val executionContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  @Benchmark
  def simpleCaliban(): Unit = {
    val io = Caliban.interpreter.execute(simpleQuery)
    Caliban.run(io)
    ()
  }

  @Benchmark
  def simpleSangria(): Unit = {
    val future: Future[Json] =
      Future.fromTry(QueryParser.parse(simpleQuery)).flatMap(queryAst => Executor.execute(Sangria.schema, queryAst))
    Await.result(future, 1.minute)
    ()
  }

  @Benchmark
  def simpleGrackle(): Unit = {
    val io = Grackle.compileAndRun(simpleQuery)
    Grackle.run(io)
    ()
  }

  @Benchmark
  def simpleGql(): Unit = {
    val io = gql.Compiler[IO].compile(Gql.schema, simpleQuery) match {
      case Right(gql.Application.Query(run)) => run
      case _                                 => IO.raiseError(new Exception("Failed to compile"))
    }
    Gql.run(io)
    ()
  }
}

object SimpleQueryBenchmark {
  val simpleQuery: String =
    """{
          characters{
            name
            origin
          }
       }""".stripMargin
}

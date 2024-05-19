package caliban

import caliban.parsing.Parser
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
class ParserBenchmark {
  import ComplexQueryBenchmark._

  implicit val executionContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  @Benchmark
  def runCaliban(): Unit = {
    val io = Parser.parseQuery(fullIntrospectionQuery)
    Caliban.run(io)
    ()
  }

  @Benchmark
  def runSangria(): Unit = {
    val future = Future.fromTry(QueryParser.parse(fullIntrospectionQuery))
    Await.result(future, 1.minute)
    ()
  }

  @Benchmark
  def runGrackle(): Unit = {
    Grackle.compiler.compile(fullIntrospectionQuery)
    ()
  }

  @Benchmark
  def runGql(): Unit = {
    gql.parser.parseQuery(fullIntrospectionQuery)
    ()
  }
}

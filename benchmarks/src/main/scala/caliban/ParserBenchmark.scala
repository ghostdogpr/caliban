package caliban

import caliban.parsing.Parser
import caliban.parsing.adt.Document
import cats.data.NonEmptyList
import cats.parse.Caret
import gql.parser.QueryAst
import grackle.Operation
import org.openjdk.jmh.annotations._
import sangria.parser.QueryParser

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContextExecutor

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ParserBenchmark {
  import ComplexQueryBenchmark._

  implicit val executionContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  @Benchmark
  def runCaliban(): Document =
    Parser.parseQueryEither(fullIntrospectionQuery).fold(throw _, identity)

  @Benchmark
  def runSangria(): sangria.ast.Document =
    QueryParser.parse(fullIntrospectionQuery).fold(throw _, identity)

  @Benchmark
  def runGrackle(): Operation =
    Grackle.compiler.compile(fullIntrospectionQuery).getOrElse(throw new Throwable("Grackle failed to parse query"))

  @Benchmark
  def runGql(): NonEmptyList[QueryAst.ExecutableDefinition[Caret]] =
    gql.parser.parseQuery(fullIntrospectionQuery).fold(e => throw new Throwable(e.prettyError.value), identity)
}

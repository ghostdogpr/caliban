package caliban

import caliban.parsing.Parser
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
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
  def runCaliban(bh: Blackhole): Unit = {
    bh.consume(Parser.parseQueryEither(fullIntrospectionQuery).fold(throw _, identity))
    ()
  }

  @Benchmark
  def runSangria(bh: Blackhole): Unit = {
    bh.consume(QueryParser.parse(fullIntrospectionQuery).fold(throw _, identity))
    ()
  }

  @Benchmark
  def runGrackle(bh: Blackhole): Unit = {
    bh.consume(Grackle.compiler.compile(fullIntrospectionQuery))
    ()
  }

  @Benchmark
  def runGql(bh: Blackhole): Unit = {
    bh.consume(gql.parser.parseQuery(fullIntrospectionQuery))
    ()
  }
}

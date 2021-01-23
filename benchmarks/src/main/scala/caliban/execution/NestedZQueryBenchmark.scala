package caliban.execution

import java.util.concurrent.TimeUnit

import caliban.GraphQL.graphQL
import caliban.{CalibanError, GraphQLInterpreter, RootResolver}
import org.openjdk.jmh.annotations._
import zio.{BootstrapRuntime, Runtime, ZEnv}
import zio.internal.Platform

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class NestedZQueryBenchmark {

  val runtime: Runtime[ZEnv] = new BootstrapRuntime {
    override val platform: Platform = Platform.benchmark
  }

  import NestedZQueryBenchmarkSchema._

  val simple100: GraphQLInterpreter[Any, CalibanError] = runtime.unsafeRun(graphQL(RootResolver(NestedZQueryBenchmarkSchema.simple100Elements)).interpreter)
  val simple1000: GraphQLInterpreter[Any, CalibanError] = runtime.unsafeRun(graphQL(RootResolver(NestedZQueryBenchmarkSchema.simple1000Elements)).interpreter)
  val simple10000: GraphQLInterpreter[Any, CalibanError] = runtime.unsafeRun(graphQL(RootResolver(NestedZQueryBenchmarkSchema.simple10000Elements)).interpreter)

  val multifield100: GraphQLInterpreter[Any, CalibanError] = runtime.unsafeRun(graphQL(RootResolver(NestedZQueryBenchmarkSchema.multifield100Elements)).interpreter)
  val multifield1000: GraphQLInterpreter[Any, CalibanError] = runtime.unsafeRun(graphQL(RootResolver(NestedZQueryBenchmarkSchema.multifield1000Elements)).interpreter)
  val multifield10000: GraphQLInterpreter[Any, CalibanError] = runtime.unsafeRun(graphQL(RootResolver(NestedZQueryBenchmarkSchema.multifield10000Elements)).interpreter)

  val deep100: GraphQLInterpreter[Any, CalibanError] = runtime.unsafeRun(graphQL(RootResolver(NestedZQueryBenchmarkSchema.deep100Elements)).interpreter)
  val deep1000: GraphQLInterpreter[Any, CalibanError] = runtime.unsafeRun(graphQL(RootResolver(NestedZQueryBenchmarkSchema.deep1000Elements)).interpreter)
  val deep10000: GraphQLInterpreter[Any, CalibanError] = runtime.unsafeRun(graphQL(RootResolver(NestedZQueryBenchmarkSchema.deep10000Elements)).interpreter)

  @Benchmark
  def simpleParallelQuery100(): Any = {
    val io = simple100.execute(simpleQuery, queryExecution = QueryExecution.Parallel)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def simpleParallelQuery1000(): Any = {
    val io = simple1000.execute(simpleQuery, queryExecution = QueryExecution.Parallel)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def simpleParallelQuery10000(): Any = {
    val io = simple10000.execute(simpleQuery, queryExecution = QueryExecution.Parallel)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def simpleSequentialQuery100(): Any = {
    val io = simple100.execute(simpleQuery, queryExecution = QueryExecution.Sequential)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def simpleSequentialQuery1000(): Any = {
    val io = simple1000.execute(simpleQuery, queryExecution = QueryExecution.Sequential)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def simpleSequentialQuery10000(): Any = {
    val io = simple10000.execute(simpleQuery, queryExecution = QueryExecution.Sequential)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def simpleBatchedQuery100(): Any = {
    val io = simple100.execute(simpleQuery, queryExecution = QueryExecution.Batched)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def simpleBatchedQuery1000(): Any = {
    val io = simple1000.execute(simpleQuery, queryExecution = QueryExecution.Batched)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def simpleBatchedQuery10000(): Any = {
    val io = simple10000.execute(simpleQuery, queryExecution = QueryExecution.Batched)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def multifieldParallelQuery100(): Any = {
    val io = multifield100.execute(multifieldQuery, queryExecution = QueryExecution.Parallel)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def multifieldParallelQuery1000(): Any = {
    val io = multifield1000.execute(multifieldQuery, queryExecution = QueryExecution.Parallel)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def multifieldParallelQuery10000(): Any = {
    val io = multifield10000.execute(multifieldQuery, queryExecution = QueryExecution.Parallel)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def multifieldSequentialQuery100(): Any = {
    val io = multifield100.execute(multifieldQuery, queryExecution = QueryExecution.Sequential)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def multifieldSequentialQuery1000(): Any = {
    val io = multifield1000.execute(multifieldQuery, queryExecution = QueryExecution.Sequential)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def multifieldSequentialQuery10000(): Any = {
    val io = multifield10000.execute(multifieldQuery, queryExecution = QueryExecution.Sequential)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def multifieldBatchedQuery100(): Any = {
    val io = multifield100.execute(multifieldQuery, queryExecution = QueryExecution.Batched)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def multifieldBatchedQuery1000(): Any = {
    val io = multifield1000.execute(multifieldQuery, queryExecution = QueryExecution.Batched)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def multifieldBatchedQuery10000(): Any = {
    val io = multifield10000.execute(multifieldQuery, queryExecution = QueryExecution.Batched)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def deepParallelQuery100(): Any = {
    val io = deep100.execute(deepQuery, queryExecution = QueryExecution.Parallel)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def deepParallelQuery1000(): Any = {
    val io = deep1000.execute(deepQuery, queryExecution = QueryExecution.Parallel)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def deepParallelQuery10000(): Any = {
    val io = deep10000.execute(deepQuery, queryExecution = QueryExecution.Parallel)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def deepSequentialQuery100(): Any = {
    val io = deep100.execute(deepQuery, queryExecution = QueryExecution.Sequential)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def deepSequentialQuery1000(): Any = {
    val io = deep1000.execute(deepQuery, queryExecution = QueryExecution.Sequential)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def deepSequentialQuery10000(): Any = {
    val io = deep10000.execute(deepQuery, queryExecution = QueryExecution.Sequential)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def deepBatchedQuery100(): Any = {
    val io = deep100.execute(deepQuery, queryExecution = QueryExecution.Batched)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def deepBatchedQuery1000(): Any = {
    val io = deep1000.execute(deepQuery, queryExecution = QueryExecution.Batched)
    runtime.unsafeRun(io)
  }

  @Benchmark
  def deepBatchedQuery10000(): Any = {
    val io = deep10000.execute(deepQuery, queryExecution = QueryExecution.Batched)
    runtime.unsafeRun(io)
  }
}

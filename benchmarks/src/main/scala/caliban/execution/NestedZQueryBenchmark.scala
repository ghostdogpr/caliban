package caliban.execution

import caliban.Configurator.ExecutionConfiguration
import caliban._
import caliban.wrappers.{ ApolloTracing, Wrappers }
import org.openjdk.jmh.annotations._
import zio.{ Runtime, Task, Unsafe }

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class NestedZQueryBenchmark {

  private val runtime = Runtime.default

  def run[A](zio: Task[A]): A = Unsafe.unsafe(implicit u => runtime.unsafe.run(zio).getOrThrow())

  import NestedZQueryBenchmarkSchema._

  val simple100: GraphQLInterpreter[Any, CalibanError]   =
    run(graphQL[Any, SimpleRoot, Unit, Unit](RootResolver(NestedZQueryBenchmarkSchema.simple100Elements)).interpreter)
  val simple1000: GraphQLInterpreter[Any, CalibanError]  =
    run(graphQL[Any, SimpleRoot, Unit, Unit](RootResolver(NestedZQueryBenchmarkSchema.simple1000Elements)).interpreter)
  val simple10000: GraphQLInterpreter[Any, CalibanError] =
    run(graphQL[Any, SimpleRoot, Unit, Unit](RootResolver(NestedZQueryBenchmarkSchema.simple10000Elements)).interpreter)

  val multifield100: GraphQLInterpreter[Any, CalibanError]   =
    run(
      graphQL[Any, MultifieldRoot, Unit, Unit](
        RootResolver(NestedZQueryBenchmarkSchema.multifield100Elements)
      ).interpreter
    )
  val multifield1000: GraphQLInterpreter[Any, CalibanError]  =
    run(
      graphQL[Any, MultifieldRoot, Unit, Unit](
        RootResolver(NestedZQueryBenchmarkSchema.multifield1000Elements)
      ).interpreter
    )
  val multifield10000: GraphQLInterpreter[Any, CalibanError] =
    run(
      graphQL[Any, MultifieldRoot, Unit, Unit](
        RootResolver(NestedZQueryBenchmarkSchema.multifield10000Elements)
      ).interpreter
    )

  val deep100: GraphQLInterpreter[Any, CalibanError]   =
    run(
      graphQL[Any, DeepRoot, Unit, Unit](
        RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep100Elements)
      ).interpreter
    )
  val deep1000: GraphQLInterpreter[Any, CalibanError]  =
    run(
      graphQL[Any, DeepRoot, Unit, Unit](
        RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep1000Elements)
      ).interpreter
    )
  val deep10000: GraphQLInterpreter[Any, CalibanError] =
    run(
      graphQL[Any, DeepRoot, Unit, Unit](
        RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep10000Elements)
      ).interpreter
    )

  val metricsInterpreter: GraphQLInterpreter[Any, CalibanError] =
    run(
      graphQL[Any, MultifieldRoot, Unit, Unit](
        RootResolver(NestedZQueryBenchmarkSchema.multifield1000Elements)
      ).withWrapper(Wrappers.metrics()).interpreter
    )

  val apolloInterpreter: GraphQLInterpreter[Any, CalibanError] =
    run(
      graphQL[Any, MultifieldRoot, Unit, Unit](
        RootResolver(NestedZQueryBenchmarkSchema.multifield1000Elements)
      ).withWrapper(ApolloTracing.apolloTracing()).interpreter
    )

  private val batched    = ExecutionConfiguration(queryExecution = QueryExecution.Batched)
  private val parallel   = ExecutionConfiguration(queryExecution = QueryExecution.Parallel)
  private val sequential = ExecutionConfiguration(queryExecution = QueryExecution.Sequential)

  @Benchmark
  def simpleParallelQuery100(): Any = {
    val io =
      simple100
        .wrapExecutionWith(Configurator.setWith(parallel)(_))
        .execute(simpleQuery)
    run(io)
  }

  @Benchmark
  def simpleParallelQuery1000(): Any = {
    val io =
      simple1000
        .wrapExecutionWith(Configurator.setWith(parallel)(_))
        .execute(simpleQuery)
    run(io)
  }

  @Benchmark
  def simpleParallelQuery10000(): Any = {
    val io =
      simple10000
        .wrapExecutionWith(Configurator.setWith(parallel)(_))
        .execute(simpleQuery)
    run(io)
  }

  @Benchmark
  def simpleSequentialQuery100(): Any = {
    val io =
      simple100
        .wrapExecutionWith(Configurator.setWith(sequential)(_))
        .execute(simpleQuery)
    run(io)
  }

  @Benchmark
  def simpleSequentialQuery1000(): Any = {
    val io =
      simple1000
        .wrapExecutionWith(Configurator.setWith(sequential)(_))
        .execute(simpleQuery)
    run(io)
  }

  @Benchmark
  def simpleSequentialQuery10000(): Any = {
    val io =
      simple10000
        .wrapExecutionWith(Configurator.setWith(sequential)(_))
        .execute(simpleQuery)
    run(io)
  }

  @Benchmark
  def simpleBatchedQuery100(): Any = {
    val io =
      simple100
        .wrapExecutionWith(Configurator.setWith(batched)(_))
        .execute(simpleQuery)
    run(io)
  }

  @Benchmark
  def simpleBatchedQuery1000(): Any = {
    val io =
      simple1000
        .wrapExecutionWith(Configurator.setWith(batched)(_))
        .execute(simpleQuery)
    run(io)
  }

  @Benchmark
  def simpleBatchedQuery10000(): Any = {
    val io =
      simple10000
        .wrapExecutionWith(Configurator.setWith(batched)(_))
        .execute(simpleQuery)
    run(io)
  }

  @Benchmark
  def multifieldParallelQuery100(): Any = {
    val io = multifield100
      .wrapExecutionWith(Configurator.setWith(parallel)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def multifieldParallelQuery1000(): Any = {
    val io = multifield1000
      .wrapExecutionWith(Configurator.setWith(parallel)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def multifieldParallelQuery10000(): Any = {
    val io = multifield10000
      .wrapExecutionWith(Configurator.setWith(parallel)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def multifieldSequentialQuery100(): Any = {
    val io = multifield100
      .wrapExecutionWith(Configurator.setWith(sequential)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def multifieldSequentialQuery1000(): Any = {
    val io = multifield1000
      .wrapExecutionWith(Configurator.setWith(sequential)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def multifieldSequentialQuery10000(): Any = {
    val io = multifield10000
      .wrapExecutionWith(Configurator.setWith(sequential)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def multifieldBatchedQuery100(): Any = {
    val io = multifield100
      .wrapExecutionWith(Configurator.setWith(batched)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def multifieldBatchedQuery1000(): Any = {
    val io = multifield1000
      .wrapExecutionWith(Configurator.setWith(batched)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def multifieldBatchedQuery10000(): Any = {
    val io = multifield10000
      .wrapExecutionWith(Configurator.setWith(batched)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def deepParallelQuery100(): Any = {
    val io = deep100
      .wrapExecutionWith(Configurator.setWith(parallel)(_))
      .execute(deepQuery)
    run(io)
  }

  @Benchmark
  def deepParallelQuery1000(): Any = {
    val io = deep1000
      .wrapExecutionWith(Configurator.setWith(parallel)(_))
      .execute(deepQuery)
    run(io)
  }

  @Benchmark
  def deepParallelQuery10000(): Any = {
    val io =
      deep10000
        .wrapExecutionWith(Configurator.setWith(parallel)(_))
        .execute(deepQuery)
    run(io)
  }

  @Benchmark
  def deepSequentialQuery100(): Any = {
    val io =
      deep100
        .wrapExecutionWith(Configurator.setWith(sequential)(_))
        .execute(deepQuery)
    run(io)
  }

  @Benchmark
  def deepSequentialQuery1000(): Any = {
    val io =
      deep1000
        .wrapExecutionWith(Configurator.setWith(sequential)(_))
        .execute(deepQuery)
    run(io)
  }

  @Benchmark
  def deepSequentialQuery10000(): Any = {
    val io =
      deep10000
        .wrapExecutionWith(Configurator.setWith(sequential)(_))
        .execute(deepQuery)
    run(io)
  }

  @Benchmark
  def deepBatchedQuery100(): Any = {
    val io = deep100
      .wrapExecutionWith(Configurator.setWith(batched)(_))
      .execute(deepQuery)
    run(io)
  }

  @Benchmark
  def deepBatchedQuery1000(): Any = {
    val io = deep1000
      .wrapExecutionWith(Configurator.setWith(batched)(_))
      .execute(deepQuery)
    run(io)
  }

  @Benchmark
  def deepBatchedQuery10000(): Any = {
    val io = deep10000
      .wrapExecutionWith(Configurator.setWith(batched)(_))
      .execute(deepQuery)
    run(io)
  }

  @Benchmark
  def noWrappersBenchmark(): Any = {
    val io = multifield1000
      .wrapExecutionWith(Configurator.setWith(batched)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def apolloTracingBenchmark(): Any = {
    val io = apolloInterpreter
      .wrapExecutionWith(Configurator.setWith(batched)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def metricsBenchmark(): Any = {
    val io = metricsInterpreter
      .wrapExecutionWith(Configurator.setWith(batched)(_))
      .execute(multifieldQuery)
    run(io)
  }
}

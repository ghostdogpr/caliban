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
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class NestedZQueryBenchmark {

  private val runtime = Runtime.default

  def run[A](zio: Task[A]): A = Unsafe.unsafe(implicit u => runtime.unsafe.run(zio).getOrThrow())

  import NestedZQueryBenchmarkSchema._

  @Param(Array("100", "10000"))
  var size: Int = _

  @Param(Array("sequential", "parallel", "batched"))
  var execution: String = _

  val simple100: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, SimpleRoot, Unit, Unit](
      RootResolver(NestedZQueryBenchmarkSchema.simple100Elements)
    ).interpreterUnsafe

  val simple1000: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, SimpleRoot, Unit, Unit](
      RootResolver(NestedZQueryBenchmarkSchema.simple1000Elements)
    ).interpreterUnsafe

  val simple10000: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, SimpleRoot, Unit, Unit](
      RootResolver(NestedZQueryBenchmarkSchema.simple10000Elements)
    ).interpreterUnsafe

  val multifield100: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, MultifieldRoot, Unit, Unit](
      RootResolver(NestedZQueryBenchmarkSchema.multifield100Elements)
    ).interpreterUnsafe

  val multifield1000: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, MultifieldRoot, Unit, Unit](
      RootResolver(NestedZQueryBenchmarkSchema.multifield1000Elements)
    ).interpreterUnsafe

  val multifield10000: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, MultifieldRoot, Unit, Unit](
      RootResolver(NestedZQueryBenchmarkSchema.multifield10000Elements)
    ).interpreterUnsafe

  val multifieldEager100: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, MultifieldRoot, Unit, Unit](
      RootResolver(NestedZQueryBenchmarkSchema.multifieldEager100Elements)
    ).interpreterUnsafe

  val multifieldEager1000: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, MultifieldRoot, Unit, Unit](
      RootResolver(NestedZQueryBenchmarkSchema.multifieldEager1000Elements)
    ).interpreterUnsafe

  val multifieldEager10000: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, MultifieldRoot, Unit, Unit](
      RootResolver(NestedZQueryBenchmarkSchema.multifieldEager10000Elements)
    ).interpreterUnsafe

  val deep100: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, DeepRoot, Unit, Unit](
      RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep100Elements)
    ).interpreterUnsafe

  val deep1000: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, DeepRoot, Unit, Unit](
      RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep1000Elements)
    ).interpreterUnsafe

  val deep10000: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, DeepRoot, Unit, Unit](
      RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep10000Elements)
    ).interpreterUnsafe

  val metricsInterpreter: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, MultifieldRoot, Unit, Unit](
      RootResolver(NestedZQueryBenchmarkSchema.multifield1000Elements)
    ).withWrapper(Wrappers.metrics()).interpreterUnsafe

  val apolloInterpreter: GraphQLInterpreter[Any, CalibanError] =
    graphQL[Any, MultifieldRoot, Unit, Unit](
      RootResolver(NestedZQueryBenchmarkSchema.multifield1000Elements)
    ).withWrapper(ApolloTracing.apolloTracing()).interpreterUnsafe

  private val batched    = ExecutionConfiguration(queryExecution = QueryExecution.Batched)
  private val parallel   = ExecutionConfiguration(queryExecution = QueryExecution.Parallel)
  private val sequential = ExecutionConfiguration(queryExecution = QueryExecution.Sequential)

  private def cfg() = execution match {
    case "sequential" => sequential
    case "parallel"   => parallel
    case "batched"    => batched
  }

  @Benchmark
  def simpleQueryBenchmark(): Any = {
    val interpreter = size match {
      case 100   => simple100
      case 1000  => simple1000
      case 10000 => simple10000
    }
    val io          =
      interpreter
        .wrapExecutionWith(Configurator.ref.locally(cfg())(_))
        .execute(simpleQuery)
    run(io)
  }

  @Benchmark
  def multifieldQueryBenchmark(): Any = {
    val interpreter = size match {
      case 100   => multifield100
      case 1000  => multifield1000
      case 10000 => multifield10000
    }
    val io          = interpreter
      .wrapExecutionWith(Configurator.ref.locally(cfg())(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def multifieldQueryEagerBenchmark(): Any = {
    val interpreter = size match {
      case 100   => multifieldEager100
      case 1000  => multifieldEager1000
      case 10000 => multifieldEager10000
    }
    val io          = interpreter
      .wrapExecutionWith(Configurator.ref.locally(cfg())(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def deepQueryBenchmark(): Any = {
    val interpreter = size match {
      case 100   => deep100
      case 1000  => deep1000
      case 10000 => deep10000
    }
    val io          = interpreter
      .wrapExecutionWith(Configurator.ref.locally(cfg())(_))
      .execute(deepQuery)
    run(io)
  }

  @Benchmark
  def apolloTracingBenchmark(): Any = {
    val io = apolloInterpreter
      .wrapExecutionWith(Configurator.ref.locally(batched)(_))
      .execute(multifieldQuery)
    run(io)
  }

  @Benchmark
  def metricsBenchmark(): Any = {
    val io = metricsInterpreter
      .wrapExecutionWith(Configurator.ref.locally(batched)(_))
      .execute(multifieldQuery)
    run(io)
  }
}

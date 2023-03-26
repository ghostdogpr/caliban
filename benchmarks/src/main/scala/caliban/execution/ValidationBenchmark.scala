package caliban.execution

import caliban._
import caliban.parsing.Parser
import caliban.schema.RootType
import caliban.validation.Validator
import org.openjdk.jmh.annotations.{
  Benchmark,
  BenchmarkMode,
  Fork,
  Measurement,
  Mode,
  OutputTimeUnit,
  Scope,
  State,
  Warmup
}

import java.util.concurrent.TimeUnit
import zio._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ValidationBenchmark {

  private val runtime = Runtime.default

  def run[A](zio: Task[A]): A                                      = Unsafe.unsafe(implicit u => runtime.unsafe.run(zio).getOrThrow())
  def toSchema[R](graphQL: GraphQL[R]): IO[CalibanError, RootType] =
    graphQL.validateRootSchema.map { schema =>
      RootType(
        schema.query.opType,
        schema.mutation.map(_.opType),
        schema.subscription.map(_.opType)
      )
    }

  import NestedZQueryBenchmarkSchema._

  val parsedSimpleQuery     = run(Parser.parseQuery(simpleQuery))
  val parsedMultifieldQuery = run(Parser.parseQuery(multifieldQuery))
  val parsedDeepQuery       = run(Parser.parseQuery(deepQuery))

  val simple100Type = run(
    toSchema(graphQL[Any, SimpleRoot, Unit, Unit](RootResolver(NestedZQueryBenchmarkSchema.simple100Elements)))
  )

  val simple1000Type = run(
    toSchema(
      graphQL[Any, SimpleRoot, Unit, Unit](
        RootResolver(NestedZQueryBenchmarkSchema.simple1000Elements)
      )
    )
  )

  val simple10000Type = run(
    toSchema(
      graphQL[Any, SimpleRoot, Unit, Unit](
        RootResolver(NestedZQueryBenchmarkSchema.simple10000Elements)
      )
    )
  )

  val multifield100Type   =
    run(
      toSchema(
        graphQL[Any, MultifieldRoot, Unit, Unit](
          RootResolver(NestedZQueryBenchmarkSchema.multifield100Elements)
        )
      )
    )
  val multifield1000Type  =
    run(
      toSchema(
        graphQL[Any, MultifieldRoot, Unit, Unit](
          RootResolver(NestedZQueryBenchmarkSchema.multifield1000Elements)
        )
      )
    )
  val multifield10000Type =
    run(
      toSchema(
        graphQL[Any, MultifieldRoot, Unit, Unit](
          RootResolver(NestedZQueryBenchmarkSchema.multifield10000Elements)
        )
      )
    )

  val deep100Type   =
    run(
      toSchema(
        graphQL[Any, DeepRoot, Unit, Unit](
          RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep100Elements)
        )
      )
    )
  val deep1000Type  =
    run(
      toSchema(
        graphQL[Any, DeepRoot, Unit, Unit](
          RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep1000Elements)
        )
      )
    )
  val deep10000Type =
    run(
      toSchema(
        graphQL[Any, DeepRoot, Unit, Unit](
          RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep10000Elements)
        )
      )
    )

  @Benchmark
  def simple100(): Any = {
    val io = Validator.validate(parsedSimpleQuery, simple100Type)
    run(io)
  }

  @Benchmark
  def simple1000(): Any = {
    val io = Validator.validate(parsedSimpleQuery, simple1000Type)
    run(io)
  }

  @Benchmark
  def simple10000(): Any = {
    val io = Validator.validate(parsedSimpleQuery, simple10000Type)
    run(io)
  }

  @Benchmark
  def multifield100(): Any = {
    val io = Validator.validate(parsedMultifieldQuery, multifield100Type)
    run(io)
  }

  @Benchmark
  def multifield1000(): Any = {
    val io = Validator.validate(parsedMultifieldQuery, multifield1000Type)
    run(io)
  }

  @Benchmark
  def multifield10000(): Any = {
    val io = Validator.validate(parsedMultifieldQuery, multifield10000Type)
    run(io)
  }

  @Benchmark
  def deep100(): Any = {
    val io = Validator.validate(parsedDeepQuery, deep100Type)
    run(io)
  }

  @Benchmark
  def deep1000(): Any = {
    val io = Validator.validate(parsedDeepQuery, deep1000Type)
    run(io)
  }

  @Benchmark
  def deep10000(): Any = {
    val io = Validator.validate(parsedDeepQuery, deep10000Type)
    run(io)
  }

}

package caliban.validation

import caliban._
import caliban.execution.NestedZQueryBenchmarkSchema
import caliban.introspection.Introspector
import caliban.parsing.{ Parser, VariablesCoercer }
import caliban.schema.{ RootSchema, RootType }
import org.openjdk.jmh.annotations.{ Scope, _ }
import zio._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ValidationBenchmark {

  private val runtime = Runtime.default

  def run[A](zio: Task[A]): A                                                       = Unsafe.unsafe(implicit u => runtime.unsafe.run(zio).getOrThrow())
  def toSchema[R](graphQL: GraphQL[R]): IO[CalibanError, (RootSchema[R], RootType)] =
    graphQL.validateRootSchema.map { schema =>
      schema -> RootType(
        schema.query.opType,
        schema.mutation.map(_.opType),
        schema.subscription.map(_.opType)
      )
    }

  import NestedZQueryBenchmarkSchema._

  val parsedSimpleQuery        = run(Parser.parseQuery(simpleQuery))
  val parsedMultifieldQuery    = run(Parser.parseQuery(multifieldQuery))
  val parsedDeepQuery          = run(Parser.parseQuery(deepQuery))
  val parsedDeepWithArgsQuery  = run(Parser.parseQuery(deepWithArgsQuery))
  val parsedIntrospectionQuery = run(Parser.parseQuery(ComplexQueryBenchmark.fullIntrospectionQuery))

  val (simpleSchema, simpleType) = run(
    toSchema(graphQL[Any, SimpleRoot, Unit, Unit](RootResolver(NestedZQueryBenchmarkSchema.simple100Elements)))
  )

  val (multifieldSchema, multifieldType) =
    run(
      toSchema(
        graphQL[Any, MultifieldRoot, Unit, Unit](
          RootResolver(NestedZQueryBenchmarkSchema.multifield100Elements)
        )
      )
    )

  val (deepSchema, deepType) =
    run(
      toSchema(
        graphQL[Any, DeepRoot, Unit, Unit](
          RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep100Elements)
        )
      )
    )

  val (deepWithArgsSchema, deepWithArgsType) =
    run(
      toSchema(
        graphQL[Any, DeepWithArgsRoot, Unit, Unit](
          RootResolver[DeepWithArgsRoot](NestedZQueryBenchmarkSchema.deepWithArgs100Elements)
        )
      )
    )

  @Benchmark
  def simple(): Any = {
    val io = Validator.validate(parsedSimpleQuery, simpleType)
    run(io)
  }

  @Benchmark
  def multifield(): Any = {
    val io = Validator.validate(parsedMultifieldQuery, multifieldType)
    run(io)
  }

  @Benchmark
  def deep(): Any = {
    val io = Validator.validate(parsedDeepQuery, deepType)
    run(io)
  }

  @Benchmark
  def variableCoercer(): Any = {
    val io = VariablesCoercer.coerceVariables(deepArgs100Elements, parsedDeepWithArgsQuery, deepWithArgsType, false)
    run(io)
  }

  @Benchmark
  def introspection(): Any = {
    val io =
      Validator.validate(parsedIntrospectionQuery, Introspector.introspectionRootType)
    run(io)
  }

  @Benchmark
  def fieldCreationSimple(): Any =
    Validator
      .prepareEither(
        parsedSimpleQuery,
        simpleType,
        simpleSchema,
        None,
        Map.empty,
        skipValidation = true,
        validations = Nil
      )
      .fold(throw _, identity)

  @Benchmark
  def fieldCreationMultifield(): Any =
    Validator
      .prepareEither(
        parsedMultifieldQuery,
        multifieldType,
        multifieldSchema,
        None,
        Map.empty,
        skipValidation = true,
        validations = Nil
      )
      .fold(throw _, identity)

  @Benchmark
  def fieldCreationDeep(): Any =
    Validator
      .prepareEither(
        parsedDeepQuery,
        deepType,
        deepSchema,
        None,
        Map.empty,
        skipValidation = true,
        validations = Nil
      )
      .fold(throw _, identity)

  @Benchmark
  def fieldCreationIntrospection(): Any =
    Validator
      .prepareEither(
        parsedIntrospectionQuery,
        Introspector.introspectionRootType,
        simpleSchema,
        None,
        Map.empty,
        skipValidation = true,
        validations = Nil
      )
      .fold(throw _, identity)

}

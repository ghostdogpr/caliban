package caliban.validation

import caliban._
import caliban.execution.NestedZQueryBenchmarkSchema
import caliban.introspection.Introspector
import caliban.parsing.{ Parser, VariablesCoercer }
import caliban.schema.RootType
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ValidationBenchmark {

  def run[A](either: Either[Throwable, A]): A = either.fold(throw _, identity)

  def toRootType[R](graphQL: GraphQL[R]): Either[CalibanError, RootType] =
    graphQL.validateRootSchema.map { schema =>
      RootType(
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

  val simpleType = run(
    toRootType(graphQL[Any, SimpleRoot, Unit, Unit](RootResolver(NestedZQueryBenchmarkSchema.simple100Elements)))
  )

  val multifieldType =
    run(
      toRootType(
        graphQL[Any, MultifieldRoot, Unit, Unit](
          RootResolver(NestedZQueryBenchmarkSchema.multifield100Elements)
        )
      )
    )

  val deepType =
    run(
      toRootType(
        graphQL[Any, DeepRoot, Unit, Unit](
          RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep100Elements)
        )
      )
    )

  val deepWithArgsType =
    run(
      toRootType(
        graphQL[Any, DeepWithArgsRoot, Unit, Unit](
          RootResolver[DeepWithArgsRoot](NestedZQueryBenchmarkSchema.deepWithArgs100Elements)
        )
      )
    )

  @Benchmark
  def simple(): Any =
    run(Validator.validateAll(parsedSimpleQuery, simpleType))

  @Benchmark
  def multifield(): Any =
    run(Validator.validateAll(parsedMultifieldQuery, multifieldType))

  @Benchmark
  def deep(): Any =
    run(Validator.validateAll(parsedDeepQuery, deepType))

  @Benchmark
  def variableCoercer(): Any =
    run(VariablesCoercer.coerceVariables(deepArgs100Elements, parsedDeepWithArgsQuery, deepWithArgsType, false))

  @Benchmark
  def introspection(): Any =
    run(Validator.validateAll(parsedIntrospectionQuery, Introspector.introspectionRootType))

  @Benchmark
  def fieldCreationSimple(): Any =
    run(
      Validator.prepare(
        parsedSimpleQuery,
        simpleType,
        None,
        Map.empty,
        skipValidation = true,
        validations = Nil
      )
    )

  @Benchmark
  def fieldCreationMultifield(): Any =
    run(
      Validator.prepare(
        parsedMultifieldQuery,
        multifieldType,
        None,
        Map.empty,
        skipValidation = true,
        validations = Nil
      )
    )

  @Benchmark
  def fieldCreationDeep(): Any          =
    run(
      Validator.prepare(
        parsedDeepQuery,
        deepType,
        None,
        Map.empty,
        skipValidation = true,
        validations = Nil
      )
    )
  @Benchmark
  def fieldCreationIntrospection(): Any =
    run(
      Validator.prepare(
        parsedIntrospectionQuery,
        Introspector.introspectionRootType,
        None,
        Map.empty,
        skipValidation = true,
        validations = Nil
      )
    )

}

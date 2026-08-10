package caliban.gateway.prototype

import caliban._
import caliban.InputValue.ObjectValue
import caliban.Value.{ BooleanValue, IntValue }
import caliban.execution.ExecutionRequest
import caliban.parsing.{ Parser, VariablesCoercer }
import caliban.parsing.adt.Document
import caliban.schema.{ ArgBuilder, RootType, Schema }
import caliban.validation.Validator
import org.openjdk.jmh.annotations._

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeUnit

private object GatewayOperationFrontEndSchema {
  final case class Filter(i: Int, next: Option[Filter])
  final case class SearchArgs(filter: Filter, limit: Option[Int])
  final case class Item(id: Int, name: String, score: Double, active: Boolean, category: String, count: Int)
  final case class QueryRoot(search: SearchArgs => List[Item])

  implicit val filterBuilder: ArgBuilder[Filter]         = ArgBuilder.gen
  implicit val searchArgsBuilder: ArgBuilder[SearchArgs] = ArgBuilder.gen
  lazy implicit val filterSchema: Schema[Any, Filter]    = Schema.gen
  implicit val searchArgsSchema: Schema[Any, SearchArgs] = Schema.gen
  implicit val itemSchema: Schema[Any, Item]             = Schema.gen
  implicit val querySchema: Schema[Any, QueryRoot]       = Schema.gen

  val query: String =
    """query Gateway($filter: FilterInput!, $limit: Int, $details: Boolean!) {
      |  search(filter: $filter, limit: $limit) {
      |    ...Core
      |    name @include(if: $details)
      |    score
      |    active
      |    category
      |    count
      |  }
      |}
      |
      |fragment Core on Item {
      |  id
      |}
      |""".stripMargin

  private def nestedFilter(depth: Int): InputValue = {
    def loop(i: Int): Map[String, InputValue] =
      if (i == 0) Map("i" -> IntValue(0))
      else Map("i"        -> IntValue(i), "next" -> ObjectValue(loop(i - 1)))

    ObjectValue(loop(depth))
  }

  val variables: Map[String, InputValue] = Map(
    "filter"  -> nestedFilter(20),
    "limit"   -> IntValue(10),
    "details" -> BooleanValue(true)
  )

  val root: QueryRoot = QueryRoot { args =>
    List.tabulate(args.limit.getOrElse(10)) { i =>
      Item(i, s"item-$i", i.toDouble / 10, i % 2 == 0, "benchmark", args.filter.i)
    }
  }
}

/**
 * Throwaway measurement spike for the gateway operation front end.
 *
 * It deliberately uses Caliban's existing AST and validation machinery. The
 * prepared-request lookup is only a cache-hit lower bound: ExecutionRequest
 * contains concrete variable values and therefore cannot be shared across
 * requests with different variables.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class GatewayOperationFrontEndBenchmark {

  import GatewayOperationFrontEndSchema._

  private def run[A](value: Either[Throwable, A]): A =
    value.fold(throw _, identity)

  private def toRootType[R](api: GraphQL[R]): RootType =
    run(api.validateRootSchema.map { schema =>
      RootType(
        schema.query.opType,
        schema.mutation.map(_.opType),
        schema.subscription.map(_.opType)
      )
    })

  private val rootType: RootType =
    toRootType(
      graphQL[Any, QueryRoot, Unit, Unit](
        RootResolver[QueryRoot](root)
      )
    )

  private val parsed: Document = run(Parser.parseQuery(query))

  // Establish that the document is safe for the static-validation cache path.
  run(Validator.validateAll(parsed, rootType))

  private val coercedVariables =
    run(VariablesCoercer.coerceVariables(variables, parsed, rootType, false, None))

  private val prepared: ExecutionRequest =
    run(Validator.prepare(parsed, rootType, None, coercedVariables, skipValidation = true, validations = Nil))

  private val parseCache = {
    val cache = new ConcurrentHashMap[String, Document]()
    cache.put(query, parsed)
    cache
  }

  private val preparedCache = {
    val cache = new ConcurrentHashMap[String, ExecutionRequest]()
    cache.put(query, prepared)
    cache
  }

  @Benchmark
  def coldCalibanPipeline(): ExecutionRequest = {
    val document  = run(Parser.parseQuery(query))
    val variables =
      run(VariablesCoercer.coerceVariables(GatewayOperationFrontEndSchema.variables, document, rootType, false, None))
    run(Validator.prepare(document, rootType, None, variables, skipValidation = false, Validator.AllValidations))
  }

  @Benchmark
  def parsedDocumentCacheHit(): ExecutionRequest = {
    val document  = parseCache.get(query)
    val variables =
      run(VariablesCoercer.coerceVariables(GatewayOperationFrontEndSchema.variables, document, rootType, false, None))
    run(Validator.prepare(document, rootType, None, variables, skipValidation = false, Validator.AllValidations))
  }

  @Benchmark
  def staticallyValidatedDocumentCacheHit(): ExecutionRequest = {
    val document  = parseCache.get(query)
    val variables =
      run(VariablesCoercer.coerceVariables(GatewayOperationFrontEndSchema.variables, document, rootType, false, None))
    run(Validator.prepare(document, rootType, None, variables, skipValidation = true, validations = Nil))
  }

  @Benchmark
  def preparedRequestLookupLowerBound(): ExecutionRequest =
    preparedCache.get(query)

  @Benchmark
  def parseOnly(): Document =
    run(Parser.parseQuery(query))

  @Benchmark
  def staticValidationOnly(): Unit =
    run(Validator.validateAll(parsed, rootType))

  @Benchmark
  def variableCoercionOnly(): Map[String, InputValue] =
    run(VariablesCoercer.coerceVariables(variables, parsed, rootType, false, None))

  @Benchmark
  def fieldCreationOnly(): ExecutionRequest =
    run(Validator.prepare(parsed, rootType, None, coercedVariables, skipValidation = true, validations = Nil))
}

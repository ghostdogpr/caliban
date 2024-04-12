package caliban

import caliban.execution.QueryExecution
import caliban.validation.Validator.{ AllValidations, QueryValidation }
import zio._
import zio.query.{ Cache, DataSource }

object Configurator {

  /**
   * Configuration for the execution of a GraphQL query.
   * @param skipValidation if true, the query will not be validated (in that case, the `validations` field is ignored). Default: false.
   * @param enableIntrospection if true, introspection queries are allowed. Default: true.
   * @param allowMutationsOverGetRequests if true, mutations are allowed for GET requests. Note that this is highly discouraged as it goes against the recommended practices. Default: false.
   * @param queryExecution the execution strategy to use (sequential, parallel, batched). Default: parallel.
   * @param validations the validations to run on the query during the validation phase. Default: all available validations.
   * @param queryCache An effect used to create a [[Cache]] to use with [[DataSource]]-backed ZQueries.
   *                   The effect will be run for each query execution to create a new cache, so ensure that any side-effects are properly captured in the provided effect.
   *                   Default: The default empty cache implementation from zio-query
   */
  case class ExecutionConfiguration(
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    allowMutationsOverGetRequests: Boolean = false,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    validations: List[QueryValidation] = AllValidations,
    queryCache: UIO[Cache] = Cache.empty(Trace.empty)
  )

  private val configRef: FiberRef[ExecutionConfiguration] =
    Unsafe.unsafe(implicit u => FiberRef.unsafe.make(ExecutionConfiguration()))

  private[caliban] val configuration: UIO[ExecutionConfiguration] =
    configRef.get(Trace.empty)

  private[caliban] def setWith[R, E, A](cfg: ExecutionConfiguration)(f: ZIO[R, E, A])(implicit
    trace: Trace
  ): ZIO[R, E, A] =
    configRef.locally(cfg)(f)

  /**
   * Skip validation of the query.
   * @param skip if true, the query will not be validated (in that case, the `validations` field is ignored).
   */
  def setSkipValidation(skip: Boolean): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(skipValidation = skip))

  /**
   * Set the validations to run on the query during the validation phase.
   * @param validations the validations to run on the query during the validation phase.
   */
  def setValidations(validations: List[QueryValidation]): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(validations = validations))

  /**
   * Enable or disable introspection queries.
   * @param enable if true, introspection queries are allowed.
   */
  def setEnableIntrospection(enable: Boolean): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(enableIntrospection = enable))

  /**
   * Set the execution strategy to use (sequential, parallel, batched).
   * @param queryExecution the execution strategy to use.
   */
  def setQueryExecution(queryExecution: QueryExecution): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(queryExecution = queryExecution))

  /**
   * Enable or disable mutations for GET requests. See [[ExecutionConfiguration]] for more details
   */
  def setAllowMutationsOverGetRequests(allow: Boolean): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(allowMutationsOverGetRequests = allow))

  /**
   * Sets an effect which will be used to create a new ZQuery [[Cache]] for each query execution.
   * This allows customizing the initial cache parameters or providing a custom implementation.
   *
   * @see [[ExecutionConfiguration]] for more details
   */
  def setQueryCache(mkCache: UIO[Cache]): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(queryCache = mkCache))
}

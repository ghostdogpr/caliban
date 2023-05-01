package caliban

import caliban.execution.QueryExecution
import caliban.validation.Validator.{ AllValidations, QueryValidation }
import zio._

object Configurator {

  /**
   * Configuration for the execution of a GraphQL query.
   * @param skipValidation if true, the query will not be validated (in that case, the `validations` field is ignored). Default: false.
   * @param enableIntrospection if true, introspection queries are allowed. Default: true.
   * @param queryExecution the execution strategy to use (sequential, parallel, batched). Default: parallel.
   * @param validations the validations to run on the query during the validation phase. Default: all available validations.
   */
  case class ExecutionConfiguration(
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    validations: List[QueryValidation] = AllValidations
  )

  private val configRef: FiberRef[ExecutionConfiguration] =
    Unsafe.unsafe(implicit u => FiberRef.unsafe.make(ExecutionConfiguration()))

  private[caliban] def configuration: UIO[ExecutionConfiguration] =
    configRef.get

  /**
   * Skip validation of the query.
   * @param skip if true, the query will not be validated (in that case, the `validations` field is ignored).
   */
  def setSkipValidation(skip: Boolean): ULayer[Unit] =
    ZLayer.scoped(setSkipValidationScoped(skip))

  /**
   * Skip validation of the query.
   * @param skip if true, the query will not be validated (in that case, the `validations` field is ignored).
   */
  def setSkipValidationScoped(skip: Boolean): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(skipValidation = skip))

  /**
   * Set the validations to run on the query during the validation phase.
   * @param validations the validations to run on the query during the validation phase.
   */
  def setValidations(validations: List[QueryValidation]): ULayer[Unit] =
    ZLayer.scoped(setValidationsScoped(validations))

  /**
   * Set the validations to run on the query during the validation phase.
   * @param validations the validations to run on the query during the validation phase.
   */
  def setValidationsScoped(validations: List[QueryValidation]): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(validations = validations))

  /**
   * Enable or disable introspection queries.
   * @param enable if true, introspection queries are allowed.
   */
  def setEnableIntrospection(enable: Boolean): ULayer[Unit] =
    ZLayer.scoped(setEnableIntrospectionScoped(enable))

  /**
   * Enable or disable introspection queries.
   * @param enable if true, introspection queries are allowed.
   */
  def setEnableIntrospectionScoped(enable: Boolean): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(enableIntrospection = enable))

  /**
   * Set the execution strategy to use (sequential, parallel, batched).
   * @param queryExecution the execution strategy to use.
   */
  def setQueryExecution(queryExecution: QueryExecution): ULayer[Unit] =
    ZLayer.scoped(setQueryExecutionScoped(queryExecution))

  /**
   * Set the execution strategy to use (sequential, parallel, batched).
   * @param queryExecution the execution strategy to use.
   */
  def setQueryExecutionScoped(queryExecution: QueryExecution): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(queryExecution = queryExecution))
}

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

  def setSkipValidation(skip: Boolean): ULayer[Unit] =
    ZLayer.scoped(setSkipValidationScoped(skip))

  def setSkipValidationScoped(skip: Boolean): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(skipValidation = skip))

  def setValidations(validations: List[QueryValidation]): ULayer[Unit] =
    ZLayer.scoped(setValidationsScoped(validations))

  def setValidationsScoped(validations: List[QueryValidation]): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(validations = validations))

  def setEnableIntrospection(enable: Boolean): ULayer[Unit] =
    ZLayer.scoped(setEnableIntrospectionScoped(enable))

  def setEnableIntrospectionScoped(enable: Boolean): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(enableIntrospection = enable))

  def setQueryExecution(queryExecution: QueryExecution): ULayer[Unit] =
    ZLayer.scoped(setQueryExecutionScoped(queryExecution))

  def setQueryExecutionScoped(queryExecution: QueryExecution): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(queryExecution = queryExecution))
}

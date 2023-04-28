package caliban

import caliban.CalibanError.ValidationError
import caliban.execution.QueryExecution
import caliban.validation.Context
import caliban.validation.Validator.DefaultValidations
import zio._
import zio.prelude.EReader

object Configurator {
  case class ExecutionConfiguration(
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    validations: List[EReader[Context, ValidationError, Unit]] = DefaultValidations
  )

  private val configRef: FiberRef[ExecutionConfiguration] =
    Unsafe.unsafe(implicit u => FiberRef.unsafe.make(ExecutionConfiguration()))

  private[caliban] def configuration: UIO[ExecutionConfiguration] =
    configRef.get

  def setSkipValidation(skip: Boolean): ULayer[Unit] =
    ZLayer.scoped(setSkipValidationScoped(skip))

  def setSkipValidationScoped(skip: Boolean): URIO[Scope, Unit] =
    configRef.locallyScopedWith(_.copy(skipValidation = skip))

  def setValidations(validations: List[EReader[Context, ValidationError, Unit]]): ULayer[Unit] =
    ZLayer.scoped(setValidationsScoped(validations))

  def setValidationsScoped(validations: List[EReader[Context, ValidationError, Unit]]): URIO[Scope, Unit] =
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

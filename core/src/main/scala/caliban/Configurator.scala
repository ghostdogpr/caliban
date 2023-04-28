package caliban

import caliban.CalibanError.ValidationError
import caliban.execution.QueryExecution
import caliban.validation.Context
import caliban.validation.Validator.DefaultValidations
import zio._
import zio.prelude.EReader

object Configurator {
  private val validationFiberRef: FiberRef[List[EReader[Context, ValidationError, Unit]]] =
    Unsafe.unsafe(implicit u => FiberRef.unsafe.make(DefaultValidations))

  private val skipQueryValidationRef: FiberRef[Boolean] =
    Unsafe.unsafe(implicit u => FiberRef.unsafe.make(false))

  private val enableIntrospectionRef: FiberRef[Boolean] =
    Unsafe.unsafe(implicit u => FiberRef.unsafe.make(true))

  private val queryExecutionRef: FiberRef[QueryExecution] =
    Unsafe.unsafe(implicit u => FiberRef.unsafe.make(QueryExecution.Parallel))

  private[caliban] def skipValidation: UIO[Boolean] =
    skipQueryValidationRef.get

  private[caliban] def enableIntrospection: UIO[Boolean] =
    enableIntrospectionRef.get

  private[caliban] def queryExecution: UIO[QueryExecution] =
    queryExecutionRef.get

  def setSkipValidation(skip: Boolean): UIO[Unit] =
    skipQueryValidationRef.set(skip).unit

  def setValidations(validations: List[EReader[Context, ValidationError, Unit]]): IO[Nothing, Unit] =
    validationFiberRef.set(validations).unit

  def setEnableIntrospection(enable: Boolean): IO[Nothing, Unit] =
    enableIntrospectionRef.set(enable).unit

  def setQueryExecutionRef(queryExecution: QueryExecution): IO[Nothing, Unit] =
    queryExecutionRef.set(queryExecution).unit
}

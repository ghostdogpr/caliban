package caliban.gateway.internal

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, GraphQLResponseContext }
import caliban.execution.Executor
import caliban.gateway.{ GatewayInterpreter, GatewayWrapper }
import caliban.gateway.GatewayWrapper.{ Event, Outcome, Result }
import caliban.gateway.internal.execution.PlanExecutor
import caliban.gateway.internal.GatewayInterpreterImpl._
import caliban.GraphQLResponseContext.ServerFailure
import caliban.parsing.adt.OperationType
import caliban.Value.NullValue
import zio.{ Exit, IO, Trace, UIO, URIO, ZIO }

private[gateway] final class GatewayInterpreterImpl[-R](
  operations: OperationPreparation[R],
  executor: PlanExecutor[R],
  control: GatewayExecutionControl,
  wrapper: GatewayWrapper[R]
) extends GatewayInterpreter[R] {

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    control.runRequest(operations.check(query))(ZIO.fail(requestTimeoutError))(ZIO.fail(requestShutdownError))

  def status(implicit trace: Trace): UIO[GatewayInterpreter.Status] =
    operations.cacheStatus.flatMap(control.status)

  def explain(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, String] =
    control
      .runRequest(operations.prepare(request).map(prepared => prepared.plan.render))(
        ZIO.fail(requestTimeoutError)
      )(
        ZIO.fail(requestShutdownError)
      )

  def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    if (wrapper.enabled) executeObservedRequest(request)
    else
      control
        .runRequest(
          operations
            .prepare(request)
            .foldZIO(
              failPreparation,
              prepared =>
                GraphQLResponseContext.markExecuted *>
                  executor.execute(prepared.plan, prepared.executionRequest, prepared.request)
            )
        )(
          GraphQLResponseContext.markServerError(ServerFailure.TimedOut).as(requestTimeoutResponse)
        )(
          GraphQLResponseContext
            .markServerError(ServerFailure.Unavailable)
            .as(requestShutdownResponse)
        )

  private def executeObservedRequest(request: GraphQLRequest)(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] = {
    val execution =
      wrapper
        .wrap(Event.Routing)(operations.prepare(request))(
          Result.fromExit(_)(
            _ => Result(Outcome.Success),
            error => Result(preparationOutcome(error))
          )
        )
        .foldZIO(
          error =>
            wrapper.observeCompletion(failPreparation(error)).map(RequestResult(_, preparationOutcome(error), None)),
          prepared =>
            (GraphQLResponseContext.markExecuted *>
              executor.execute(prepared.plan, prepared.executionRequest, prepared.request)).map { response =>
              RequestResult(
                response,
                if (response.errors.isEmpty) Outcome.Success else Outcome.GraphQLError,
                Some(prepared.plan.operation)
              )
            }
        )

    control
      .runObservedRequest(wrapper, Event.Request(request.operationName))(execution)(
        wrapper.wrap(Event.Completion)(
          GraphQLResponseContext
            .markServerError(ServerFailure.TimedOut)
            .as(RequestResult(requestTimeoutResponse, Outcome.Timeout, None))
        )(classifyRequestResult)
      )(
        wrapper.wrap(Event.Completion)(
          GraphQLResponseContext
            .markServerError(ServerFailure.Unavailable)
            .as(RequestResult(requestShutdownResponse, Outcome.Http5xx, None))
        )(classifyRequestResult)
      )(classifyRequestResult)
      .map(_.response)
  }

  private def failPreparation(error: CalibanError)(implicit trace: Trace): UIO[GraphQLResponse[CalibanError]] =
    (if (OperationHooks.isInternalFailure(error))
       GraphQLResponseContext.markServerError(ServerFailure.Internal)
     else GraphQLResponseContext.markRequestError(error)) *> Executor.fail(error)

  private def preparationOutcome(error: CalibanError): Outcome =
    if (OperationHooks.isInternalFailure(error)) Outcome.InternalError else Outcome.RequestError

  private def classifyRequestResult(exit: Exit[Nothing, RequestResult]): Result =
    Result.fromExit(exit)(
      result => Result(result.outcome, result.operationType, result.response.errors.size),
      _ => Result(Outcome.InternalError)
    )

}

private[gateway] object GatewayInterpreterImpl {
  private val requestTimeoutError = CalibanError.ExecutionError("Gateway request timed out.")

  private val requestShutdownError = CalibanError.ExecutionError("Gateway is shutting down.")

  private val requestTimeoutResponse =
    GraphQLResponse(NullValue, requestTimeoutError :: Nil)

  private val requestShutdownResponse =
    GraphQLResponse(NullValue, requestShutdownError :: Nil)

  private final case class RequestResult(
    response: GraphQLResponse[CalibanError],
    outcome: Outcome,
    operationType: Option[OperationType]
  )

}

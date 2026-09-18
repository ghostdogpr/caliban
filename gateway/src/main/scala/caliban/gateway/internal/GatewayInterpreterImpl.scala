package caliban.gateway.internal

import caliban.GraphQLResponseContext.ServerFailure
import caliban.ResponseValue.StreamValue
import caliban.Value.NullValue
import caliban.execution.{ ExecutionRequest, Executor }
import caliban.gateway.PhaseHooks.{ Event, Outcome, Result }
import caliban.gateway.internal.GatewayInterpreterImpl._
import caliban.gateway.internal.execution.{ PlanExecutor, PreparedPlan }
import caliban.gateway.{ GatewayInterpreter, OperationEvent, PhaseHooks }
import caliban.parsing.adt.{ Document, OperationType }
import caliban._
import zio.stream.ZStream
import zio.{ Exit, IO, Trace, UIO, URIO, ZIO }

private[gateway] final class GatewayInterpreterImpl[-R](
  operations: OperationPreparation[R],
  executor: PlanExecutor[R],
  control: GatewayExecutionControl[R],
  hooks: PhaseHooks[R],
  reservation: Option[GatewayExecutionControl.Lease] = None
) extends GatewayInterpreter[R] {

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    control.runRequest(operations.check(query), reservation)(ZIO.fail(requestTimeoutError))(
      ZIO.fail(requestShutdownError)
    )

  /**
   * A single-use view. The caller must release it even if execution is interrupted before it starts.
   */
  def reserve(implicit trace: Trace): UIO[Option[GatewayInterpreterImpl[R]]] =
    control.reserve.map(_.map(lease => new GatewayInterpreterImpl(operations, executor, control, hooks, Some(lease))))

  def retireSubscriptions(implicit trace: Trace): UIO[Unit] = control.subscriptions.stop(SubscriptionTermination.Reload)

  def release(implicit trace: Trace): UIO[Unit] = reservation.fold[UIO[Unit]](ZIO.unit)(control.release(_))

  def explain(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, String] =
    control
      .runRequest(operations.prepare(request).map(prepared => prepared.plan.plan.render), reservation)(
        ZIO.fail(requestTimeoutError)
      )(
        ZIO.fail(requestShutdownError)
      )

  def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    if (hooks.enabled) executeObservedRequest(request)
    else
      control.runRequest(
        operations.prepare(request).foldZIO(failPreparation, executePrepared),
        reservation
      )(
        GraphQLResponseContext.markServerError(ServerFailure.TimedOut).as(requestTimeoutResponse)
      )(
        shutdownResponse
      )

  private def executeObservedRequest(request: GraphQLRequest)(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] = {
    def completeFailure(
      failure: ServerFailure,
      response: GraphQLResponse[CalibanError],
      outcome: Outcome
    ): URIO[R, RequestResult] =
      hooks.completion.run(Event.Completion)(
        GraphQLResponseContext.markServerError(failure).as(RequestResult.NotExecuted(response, outcome))
      )(classifyRequestResult)

    val preparation = hooks.routing
      .run(Event.Routing)(operations.prepare(request))(
        Result.fromExit(_)(_ => Result(Outcome.Success), error => Result(preparationOutcome(error)))
      )
      .either

    val execution = control
      .runObservedRequest(Event.Request(request.operationName), reservation)(preparation)(
        _.fold(_ => true, _.plan.plan.operation != OperationType.Subscription)
      )(
        _.fold[URIO[R, RequestResult]](
          error =>
            hooks
              .observeCompletion(failPreparation(error))
              .map(RequestResult.NotExecuted(_, preparationOutcome(error))),
          prepared =>
            executePrepared(prepared).map { response =>
              RequestResult.Executed(
                response,
                if (response.errors.isEmpty) Outcome.Success else Outcome.GraphQLError,
                prepared.plan.plan.operation,
                prepared.document,
                prepared.executionRequest
              )
            }
        )
      )(completeFailure(ServerFailure.TimedOut, requestTimeoutResponse, Outcome.Timeout))(
        completeFailure(ServerFailure.Unavailable, requestShutdownResponse, Outcome.RequestError)
      )(classifyRequestResult)

    hooks.observeOperation
      .run(Event.ObserveOperation(request = request))(execution)(operationEvent)
      .map(_.response)
  }

  /**
   * Every request yields exactly one observation, whatever its outcome. Preparation failures, timeouts, shutdowns and
   * interruptions carry no document or execution request; the outcome says which one it was.
   */
  private def operationEvent(exit: Exit[Nothing, RequestResult]): OperationEvent =
    exit match {
      case Exit.Success(RequestResult.Executed(response, outcome, operation, document, execution)) =>
        OperationEvent(Some(document), Some(execution), Some(operation), response.errors, outcome)
      case Exit.Success(RequestResult.NotExecuted(response, outcome))                              =>
        OperationEvent(None, None, None, response.errors, outcome)
      case Exit.Failure(cause)                                                                     =>
        OperationEvent(None, None, None, Nil, if (cause.isInterrupted) Outcome.Cancelled else Outcome.InternalError)
    }

  private def failPreparation(error: CalibanError)(implicit trace: Trace): UIO[GraphQLResponse[CalibanError]] =
    (if (OperationHooks.isInternalFailure(error))
       GraphQLResponseContext.markServerError(ServerFailure.Internal)
     else GraphQLResponseContext.markRequestError(error)) *> Executor.fail(error)

  private def executePrepared(prepared: OperationPreparation.Prepared)(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] =
    GraphQLResponseContext.markExecuted *> (prepared.plan match {
      case subscription: PreparedPlan.Subscription =>
        (for {
          frozen  <- executor
                       .forSubscription(subscription)
                       .mapError(_ => CalibanError.ExecutionError("Subscription headers could not be prepared."))
          env     <- ZIO.environment[R]
          headers <- IncomingRequestHeaders.get
        } yield {
          val events = ZStream.unwrapScoped(
            IncomingRequestHeaders
              .locallyScoped(headers)
              .as(
                control.subscriptions
                  .stream(frozen.subscribe(subscription, prepared.executionRequest, prepared.request))(response =>
                    frozen.executeEvent(subscription, prepared.request, response)
                  )
                  .provideEnvironment(env)
              )
          )
          GraphQLResponse(StreamValue(events.map(_.toResponseValue)), Nil)
        }).catchAll(failPreparation)
      case request: PreparedPlan.Request           =>
        executor.execute(request, prepared.executionRequest, prepared.request)
    })

  private def preparationOutcome(error: CalibanError): Outcome =
    if (OperationHooks.isInternalFailure(error)) Outcome.InternalError else Outcome.RequestError

  private def classifyRequestResult(exit: Exit[Nothing, RequestResult]): Result =
    Result.fromExit(exit)(
      {
        case RequestResult.Executed(response, outcome, operation, _, _) =>
          Result(outcome, Some(operation), response.errors.size)
        case RequestResult.NotExecuted(response, outcome)               => Result(outcome, None, response.errors.size)
      },
      _ => Result(Outcome.InternalError)
    )

}

private[gateway] object GatewayInterpreterImpl {
  private val requestTimeoutError = CalibanError.ExecutionError("Gateway request timed out.")

  val requestShutdownError = CalibanError.ExecutionError("Gateway is shutting down.")

  private val requestTimeoutResponse =
    GraphQLResponse(NullValue, requestTimeoutError :: Nil)

  val requestShutdownResponse =
    GraphQLResponse(NullValue, requestShutdownError :: Nil)

  private sealed trait RequestResult {
    def response: GraphQLResponse[CalibanError]
  }

  private object RequestResult {
    final case class Executed(
      response: GraphQLResponse[CalibanError],
      outcome: Outcome,
      operation: OperationType,
      document: Document,
      executionRequest: ExecutionRequest
    ) extends RequestResult

    final case class NotExecuted(response: GraphQLResponse[CalibanError], outcome: Outcome) extends RequestResult
  }

}

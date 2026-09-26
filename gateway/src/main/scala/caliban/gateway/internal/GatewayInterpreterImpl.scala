package caliban.gateway.internal

import caliban.GraphQLResponseContext.ServerFailure
import caliban.ResponseValue.StreamValue
import caliban.Value.NullValue
import caliban.execution.{ ExecutionRequest, Executor }
import caliban.gateway.PhaseHooks.{ Event, Outcome, Result }
import caliban.gateway.internal.GatewayInterpreterImpl._
import caliban.gateway.internal.execution.PlanExecutor
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
    runRequest(operations.check(query))

  /**
   * A single-use view. The caller must release it even if execution is interrupted before it starts.
   */
  def reserve(implicit trace: Trace): UIO[Option[GatewayInterpreterImpl[R]]] =
    control.reserve.map(_.map(lease => new GatewayInterpreterImpl(operations, executor, control, hooks, Some(lease))))

  def retireSubscriptions(implicit trace: Trace): UIO[Unit] = control.subscriptions.stop(SubscriptionTermination.Reload)

  def release(implicit trace: Trace): UIO[Unit] = ZIO.foreachDiscard(reservation)(_ => control.release)

  def explain(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, String] =
    runRequest(operations.prepare(request).map(_.plan.render))

  def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    if (hooks.enabled) executeObservedRequest(request)
    else
      withLease[R, Nothing, GraphQLResponse[CalibanError]](shutdownResponse) { lease =>
        control
          .runWithin(lease)(operations.prepare(request).foldZIO(failPreparation, executeOperation))
          .someOrElseZIO(timeoutResponse)
      }

  private def withLease[R1, E, A](onRejected: => ZIO[R1, E, A])(body: GatewayExecutionControl.Lease => ZIO[R1, E, A])(
    implicit trace: Trace
  ): ZIO[R1, E, A] =
    reservation.fold(control.withLease(onRejected)(body))(body)

  private def runRequest[R1, A](effect: ZIO[R1, CalibanError, A])(implicit trace: Trace): ZIO[R1, CalibanError, A] =
    withLease[R1, CalibanError, A](ZIO.fail(requestShutdownError))(
      control.runWithin(_)(effect).someOrFail(requestTimeoutError)
    )

  private def executeObservedRequest(request: GraphQLRequest)(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] = {
    def observe(effect: URIO[R, RequestResult]): URIO[R, RequestResult] =
      hooks.execution.run(Event.Execution(request.operationName))(effect)(classifyRequestResult)

    def completeFailure(response: UIO[GraphQLResponse[CalibanError]], outcome: Outcome): URIO[R, RequestResult] =
      hooks.completion.run(Event.Completion)(response.map(RequestResult.NotExecuted(_, outcome)))(classifyRequestResult)

    val timedOut = completeFailure(timeoutResponse, Outcome.Timeout)

    val preparation = hooks.preparation
      .run(Event.Preparation)(operations.prepare(request))(
        Result.fromExit(_)(_ => Result(Outcome.Success), error => Result(preparationOutcome(error)))
      )
      .either

    def execute(prepared: Either[CalibanError, OperationPreparation.ExecutableOperation]): URIO[R, RequestResult] =
      prepared.fold(
        error =>
          hooks
            .observeCompletion(failPreparation(error))
            .map(RequestResult.NotExecuted(_, preparationOutcome(error))),
        operation =>
          executeOperation(operation).map { response =>
            RequestResult.Executed(
              response,
              Outcome.fromResponse(response),
              operation.plan.operationType,
              operation.document,
              operation.executionRequest
            )
          }
      )

    val execution = withLease(observe(completeFailure(shutdownResponse, Outcome.RequestError))) { lease =>
      // Classify the resolved operation before opening finite-request metrics/spans, while one
      // deadline and drain lease cover preparation and execution together.
      control.runWithin(lease)(preparation).flatMap {
        case None           => observe(timedOut)
        case Some(prepared) =>
          val response = control.runWithin(lease)(execute(prepared)).someOrElseZIO(timedOut)
          if (prepared.fold(_ => true, _.plan.operationType != OperationType.Subscription)) observe(response)
          else response
      }
    }

    hooks.operation
      .run(Event.Operation(request = request))(execution)(operationEvent)
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
    (if (OperationPreparation.isInternalFailure(error))
       GraphQLResponseContext.markServerError(ServerFailure.Internal)
     else GraphQLResponseContext.markRequestError(error)) *> Executor.fail(error)

  private def executeOperation(operation: OperationPreparation.ExecutableOperation)(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] =
    GraphQLResponseContext.markExecuted *> (operation.plan.operationType match {
      case OperationType.Subscription =>
        (for {
          frozen  <- executor
                       .forSubscription(operation.plan)
                       .mapError(_ => CalibanError.ExecutionError("Subscription headers could not be prepared."))
          env     <- ZIO.environment[R]
          headers <- IncomingRequestHeaders.get
        } yield {
          val events = ZStream.unwrapScoped(
            IncomingRequestHeaders
              .locallyScoped(headers)
              .as(
                control.subscriptions
                  .stream(frozen.subscribe(operation.plan, operation.executionRequest, operation.request))(response =>
                    frozen.executeEvent(operation.plan, response)
                  )
                  .provideEnvironment(env)
              )
          )
          GraphQLResponse(StreamValue(events.map(_.toResponseValue)), Nil)
        }).catchAll(failPreparation)
      case _                          =>
        executor.execute(operation.plan, operation.executionRequest, operation.request)
    })

  private def preparationOutcome(error: CalibanError): Outcome =
    if (OperationPreparation.isInternalFailure(error)) Outcome.InternalError else Outcome.RequestError

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

  def timeoutResponse(implicit trace: Trace): UIO[GraphQLResponse[CalibanError]] =
    GraphQLResponseContext
      .markServerError(ServerFailure.TimedOut)
      .as(GraphQLResponse(NullValue, requestTimeoutError :: Nil))

  def shutdownResponse(implicit trace: Trace): UIO[GraphQLResponse[CalibanError]] =
    GraphQLResponseContext
      .markServerError(ServerFailure.Unavailable)
      .as(GraphQLResponse(NullValue, requestShutdownError :: Nil))

  private sealed trait RequestResult {
    def response: GraphQLResponse[CalibanError]
  }

  private object RequestResult {
    final case class Executed(
      response: GraphQLResponse[CalibanError],
      outcome: Outcome,
      operationType: OperationType,
      document: Document,
      executionRequest: ExecutionRequest
    ) extends RequestResult

    final case class NotExecuted(response: GraphQLResponse[CalibanError], outcome: Outcome) extends RequestResult
  }

}

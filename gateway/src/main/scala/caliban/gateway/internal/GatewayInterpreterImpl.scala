package caliban.gateway.internal

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, GraphQLResponseContext, IncomingRequestHeaders }
import caliban.execution.Executor
import caliban.gateway.{ GatewayInterpreter, GatewayWrapper, SubscriptionIdentity, SubscriptionTermination }
import caliban.gateway.GatewayWrapper.{ Event, Outcome, Result }
import caliban.gateway.internal.execution.{ PlanExecutor, SubgraphExecutor }
import caliban.gateway.internal.GatewayInterpreterImpl._
import caliban.GraphQLResponseContext.ServerFailure
import caliban.parsing.adt.OperationType
import caliban.ResponseValue.StreamValue
import caliban.Value.NullValue
import zio.{ Exit, IO, Trace, UIO, URIO, ZIO }
import zio.stream.ZStream

private[gateway] final class GatewayInterpreterImpl[-R](
  operations: OperationPreparation[R],
  executor: PlanExecutor[R],
  control: GatewayExecutionControl,
  wrapper: GatewayWrapper[R],
  subscriptions: SubscriptionControl[R],
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
    control.reserve.map(
      _.map(lease => new GatewayInterpreterImpl(operations, executor, control, wrapper, subscriptions, Some(lease)))
    )

  def subscriptionStatus(implicit trace: Trace)  = subscriptions.status
  def retireSubscriptions(implicit trace: Trace) = subscriptions.stop(SubscriptionTermination.Reload)

  def executeStream(request: GraphQLRequest)(implicit
    trace: Trace
  ): ZStream[R, Throwable, GraphQLResponse[CalibanError]] =
    ZStream.unwrap(executeRequest(request).map(SubgraphExecutor.responses))

  def release(implicit trace: Trace): UIO[Unit] = reservation.fold[UIO[Unit]](ZIO.unit)(control.release(_))

  def status(implicit trace: Trace): UIO[GatewayInterpreter.Status] =
    operations.cacheStatus.flatMap(control.status)

  def explain(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, String] =
    control
      .runRequest(operations.prepare(request).map(prepared => prepared.plan.render), reservation)(
        ZIO.fail(requestTimeoutError)
      )(
        ZIO.fail(requestShutdownError)
      )

  def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    if (wrapper.enabled) executeObservedRequest(request)
    else
      control.runRequest(
        operations.prepare(request).foldZIO(failPreparation, executePrepared),
        reservation
      )(
        GraphQLResponseContext.markServerError(ServerFailure.TimedOut).as(requestTimeoutResponse)
      )(
        GraphQLResponseContext.markServerError(ServerFailure.Unavailable).as(requestShutdownResponse)
      )

  private def executeObservedRequest(request: GraphQLRequest)(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] = {
    val preparation = wrapper
      .wrap(Event.Routing)(operations.prepare(request))(
        Result.fromExit(_)(_ => Result(Outcome.Success), error => Result(preparationOutcome(error)))
      )
      .either

    control
      .runObservedRequest(wrapper, Event.Request(request.operationName), reservation)(preparation)(
        _.fold(_ => true, _.plan.operation != OperationType.Subscription)
      )(
        _.fold(
          error =>
            wrapper.observeCompletion(failPreparation(error)).map(RequestResult(_, preparationOutcome(error), None)),
          prepared =>
            executePrepared(prepared).map { response =>
              RequestResult(
                response,
                if (response.errors.isEmpty) Outcome.Success else Outcome.GraphQLError,
                Some(prepared.plan.operation)
              )
            }
        )
      )(
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

  private def executePrepared(prepared: OperationPreparation.Prepared)(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] =
    GraphQLResponseContext.markExecuted *> (
      if (prepared.plan.operation == OperationType.Subscription)
        (for {
          frozen  <- executor
                       .forSubscription(prepared.plan)
                       .mapError(_ => CalibanError.ExecutionError("Subscription headers could not be prepared."))
          env     <- ZIO.environment[R]
          headers <- IncomingRequestHeaders.get
          expiry  <- SubscriptionIdentity.expiresAt
        } yield {
          val events = ZStream.unwrapScoped(
            IncomingRequestHeaders
              .locallyScoped(headers)
              .as(
                subscriptions
                  .stream(expiry)(frozen.subscribe(prepared.plan, prepared.executionRequest, prepared.request))(
                    response => frozen.executeEvent(prepared.plan, prepared.request, response)
                  )
                  .provideEnvironment(env)
              )
          )
          GraphQLResponse(StreamValue(events.map(_.toResponseValue)), Nil)
        }).catchAll(failPreparation)
      else executor.execute(prepared.plan, prepared.executionRequest, prepared.request)
    )

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

  private[gateway] val requestShutdownError = CalibanError.ExecutionError("Gateway is shutting down.")

  private val requestTimeoutResponse =
    GraphQLResponse(NullValue, requestTimeoutError :: Nil)

  private[gateway] val requestShutdownResponse =
    GraphQLResponse(NullValue, requestShutdownError :: Nil)

  private final case class RequestResult(
    response: GraphQLResponse[CalibanError],
    outcome: Outcome,
    operationType: Option[OperationType]
  )

}

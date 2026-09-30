package caliban.gateway.internal

import caliban.GraphQLResponseContext.{ markExecuted, markServerError, markSubscribed, ServerFailure }
import caliban.ResponseValue.StreamValue
import caliban.Value.NullValue
import caliban.execution.Executor
import caliban.gateway.PhaseHooks.{ Event, Outcome, Result }
import caliban.gateway.internal.GatewayInterpreterImpl._
import caliban.gateway.internal.OperationPreparation.ExecutableOperation
import caliban.gateway.internal.execution.PlanExecutor
import caliban.gateway.{ GatewayInterpreter, OperationEvent, PhaseHooks }
import caliban.parsing.adt.OperationType
import caliban._
import zio.stm.USTM
import zio.stream.ZStream
import zio.{ Clock, Exit, IO, Trace, UIO, URIO, ZIO }

private[gateway] final class GatewayInterpreterImpl[-R](
  operations: OperationPreparation[R],
  executor: PlanExecutor[R],
  control: GatewayExecutionControl,
  subscriptions: SubscriptionControl[R],
  hooks: PhaseHooks[R]
) extends Admitting[R] {
  def admit(startedAt: Long): USTM[View[R]] = control.reserve(startedAt).map(new Leased(_))

  def rejected: View[R] = new Leased(None)

  def retireSubscriptions(implicit trace: Trace): UIO[Unit] = subscriptions.stop(SubscriptionTermination.Reload)

  private final class Leased(lease: Option[control.Lease]) extends View[R] {
    def release(implicit trace: Trace): UIO[Unit] = ZIO.foreachDiscard(lease)(_.release)

    def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
      runRequest(operations.check(query))

    def explain(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, String] =
      runRequest(operations.prepare(request).mapBoth(_.error, _.plan.render))

    private def runRequest[R1, A](effect: ZIO[R1, CalibanError, A])(implicit trace: Trace): ZIO[R1, CalibanError, A] =
      lease.fold[ZIO[R1, CalibanError, A]](ZIO.fail(requestShutdownError))(
        _.runWithin(effect).someOrFail(requestTimeoutError)
      )

    def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] = {
      val observed =
        hooks.operation.runWith(Event.Operation(request))(event => run(event.request))(operationEvent).map(_.response)
      lease.fold(observed)(
        _.runWithin(observed).someOrElseZIO(markServerError(ServerFailure.TimedOut).as(timeoutResponse))
      )
    }

    private def run(request: GraphQLRequest)(implicit trace: Trace): URIO[R, RequestResult] = {
      def observe(effect: URIO[R, RequestResult]): URIO[R, RequestResult] =
        hooks.execution.run(Event.Execution(request.operationName))(effect)(operationEvent(_).result)

      def failed(error: CalibanError, mark: UIO[Unit], outcome: Outcome): URIO[R, RequestResult] =
        hooks.completion.run(Event.Completion)(
          mark.as(RequestResult(GraphQLResponse(NullValue, error :: Nil), OperationEvent(None, error :: Nil, outcome)))
        )(operationEvent(_).result)

      def executed(operation: ExecutableOperation, mark: UIO[Unit])(execute: URIO[R, GraphQLResponse[CalibanError]]) =
        (mark *> execute).map { response =>
          val prepared = OperationEvent.Prepared(operation.document, operation.executionRequest)
          RequestResult(response, OperationEvent(Some(prepared), response.errors, Outcome.fromResponse(response)))
        }

      val preparation = hooks.preparation
        .run(Event.Preparation)(operations.prepare(request))(
          Result.fromExit(_)(_ => Result(Outcome.Success), failure => Result(failure.outcome, errorCount = 1))
        )

      val shutdown = failed(requestShutdownError, markServerError(ServerFailure.Unavailable), Outcome.RequestError)

      lease.fold(observe(shutdown)) { _ =>
        // Classify the resolved operation before opening finite-request metrics/spans, while one
        // deadline and drain lease cover preparation and execution together.
        preparation.foldZIO(
          failure => observe(failed(failure.error, failure.mark, failure.outcome)),
          operation =>
            operation.plan.operationType match {
              case OperationType.Subscription => executed(operation, markSubscribed)(subscribe(operation))
              case _                          =>
                val execute = executor.execute(operation.plan, operation.executionRequest, operation.request)
                observe(executed(operation, markExecuted)(execute))
            }
        )
      }
    }
  }

  /**
   * Every request yields exactly one observation, whatever its outcome. Preparation failures, timeouts, shutdowns and
   * interruptions carry no document or execution request; the outcome says which one it was.
   */
  private def operationEvent(exit: Exit[Nothing, RequestResult]): OperationEvent =
    exit match {
      case Exit.Success(result)                                          => result.event
      case Exit.Failure(cause) if GatewayExecutionControl.expired(cause) => timeoutEvent
      case Exit.Failure(cause)                                           => OperationEvent(None, Nil, Outcome.fromCause(cause))
    }

  private def subscribe(operation: ExecutableOperation)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    (for {
      subscription <- executor
                        .subscription(operation.plan, operation.request)
                        .mapError(_ => CalibanError.ExecutionError("Subscription headers could not be prepared."))
      env          <- ZIO.environment[R]
      headers      <- IncomingRequestHeaders.get
    } yield {
      val events = ZStream.unwrapScoped(
        IncomingRequestHeaders
          .locallyScoped(headers)
          .as(subscriptions.stream(subscription.open)(subscription.process).provideEnvironment(env))
      )
      GraphQLResponse(StreamValue(events.map(_.toResponseValue)), Nil)
    }).catchAll(Executor.fail)

}

private[gateway] object GatewayInterpreterImpl {
  private val requestTimeoutError = CalibanError.ExecutionError("Gateway request timed out.")

  private val requestShutdownError = CalibanError.ExecutionError("Gateway is shutting down.")

  private val timeoutResponse = GraphQLResponse(NullValue, requestTimeoutError :: Nil)

  private val timeoutEvent = OperationEvent(None, requestTimeoutError :: Nil, Outcome.Timeout)

  private final case class RequestResult(response: GraphQLResponse[CalibanError], event: OperationEvent)

  trait View[-R] extends GatewayInterpreter[R] {
    def release(implicit trace: Trace): UIO[Unit]
  }

  abstract class Admitting[-R] extends GatewayInterpreter[R] {
    def admit(startedAt: Long): USTM[View[R]]

    def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] = use(_.check(query))

    def explain(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, String] = use(_.explain(request))

    def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
      use(_.executeRequest(request))

    /**
     * A single-use view. It holds the lease only while `f` runs, so it must not escape `f`.
     */
    def use[R0, E, A](f: View[R] => ZIO[R0, E, A])(implicit trace: Trace): ZIO[R0, E, A] =
      ZIO.acquireReleaseWith(Clock.nanoTime.flatMap(admit(_).commit))(_.release)(f)
  }

}

package caliban.gateway

import caliban.gateway.PhaseHooks.{ Event, Outcome, Result }
import caliban.parsing.adt.OperationType
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import zio.http.Header
import zio.{ Cause, Exit, Trace, ZIO }

final case class PhaseHooks[-R] private (
  subscriptionSetup: PhaseHandler[R, Event.SubscriptionSetup.type, Nothing, Result] =
    PhaseHandler.empty[Event.SubscriptionSetup.type],
  subscriptionEvent: PhaseHandler[R, Event.SubscriptionEvent.type, Nothing, Result] =
    PhaseHandler.empty[Event.SubscriptionEvent.type],
  subscriptionTerminated: PhaseHandler[R, Event.SubscriptionTerminated, Nothing, Result] =
    PhaseHandler.empty[Event.SubscriptionTerminated],
  subscriptionAdmission: PhaseHandler[R, Event.SubscriptionAdmission, Nothing, Result] =
    PhaseHandler.empty[Event.SubscriptionAdmission],
  subscriptionOverflow: PhaseHandler[R, Event.SubscriptionOverflow.type, Nothing, Result] =
    PhaseHandler.empty[Event.SubscriptionOverflow.type],
  request: PhaseHandler[R, Event.Request, Nothing, Result] = PhaseHandler.empty[Event.Request],
  routing: PhaseHandler[R, Event.Routing.type, Nothing, Result] = PhaseHandler.empty[Event.Routing.type],
  subgraphCall: PhaseHandler[R, Event.SubgraphCall, Nothing, Result] = PhaseHandler.empty[Event.SubgraphCall],
  attempt: PhaseHandler[R, Event.Attempt, Nothing, Result] = PhaseHandler.empty[Event.Attempt],
  retry: PhaseHandler[R, Event.Retry, Nothing, Result] = PhaseHandler.empty[Event.Retry],
  completion: PhaseHandler[R, Event.Completion.type, Nothing, Result] = PhaseHandler.empty[Event.Completion.type],
  cacheAccess: PhaseHandler[R, Event.CacheAccess, Nothing, Result] = PhaseHandler.empty[Event.CacheAccess],
  overrideLabels: PhaseHandler[R, Event.OverrideLabels, Throwable, Any] = PhaseHandler.empty[Event.OverrideLabels],
  outboundHeaders: PhaseHandler[R, Event.OutboundHeaders, Nothing, Any] = PhaseHandler.empty[Event.OutboundHeaders],
  attemptHeaders: PhaseHandler[R, Event.AttemptHeaders, Nothing, Any] = PhaseHandler.empty[Event.AttemptHeaders],
  observeOperation: PhaseHandler[R, Event.ObserveOperation, Nothing, OperationEvent] =
    PhaseHandler.empty[Event.ObserveOperation]
) { self =>
  val enabled: Boolean =
    self.subscriptionSetup.enabled ||
      self.subscriptionEvent.enabled ||
      self.subscriptionTerminated.enabled ||
      self.subscriptionAdmission.enabled ||
      self.subscriptionOverflow.enabled ||
      self.request.enabled ||
      self.routing.enabled ||
      self.subgraphCall.enabled ||
      self.attempt.enabled ||
      self.retry.enabled ||
      self.completion.enabled ||
      self.cacheAccess.enabled ||
      self.overrideLabels.enabled ||
      self.outboundHeaders.enabled ||
      self.attemptHeaders.enabled ||
      self.observeOperation.enabled

  def ++[R1 <: R](that: PhaseHooks[R1]): PhaseHooks[R1] =
    copy(
      subscriptionSetup = self.subscriptionSetup ++ that.subscriptionSetup,
      subscriptionEvent = self.subscriptionEvent ++ that.subscriptionEvent,
      subscriptionTerminated = self.subscriptionTerminated ++ that.subscriptionTerminated,
      subscriptionAdmission = self.subscriptionAdmission ++ that.subscriptionAdmission,
      subscriptionOverflow = self.subscriptionOverflow ++ that.subscriptionOverflow,
      request = self.request ++ that.request,
      routing = self.routing ++ that.routing,
      subgraphCall = self.subgraphCall ++ that.subgraphCall,
      attempt = self.attempt ++ that.attempt,
      retry = self.retry ++ that.retry,
      completion = self.completion ++ that.completion,
      cacheAccess = self.cacheAccess ++ that.cacheAccess,
      overrideLabels = self.overrideLabels ++ that.overrideLabels,
      outboundHeaders = self.outboundHeaders ++ that.outboundHeaders,
      attemptHeaders = self.attemptHeaders ++ that.attemptHeaders,
      observeOperation = self.observeOperation ++ that.observeOperation
    )

  private[gateway] def observeCompletion[R0 <: R, E](effect: ZIO[R0, E, GraphQLResponse[CalibanError]])(implicit
    trace: Trace
  ): ZIO[R0, E, GraphQLResponse[CalibanError]] =
    self.completion.run(Event.Completion)(effect)(Result.classifyResponse(_))
}

/**
 * Constructors for single-phase [[PhaseHooks]], one per phase of the gateway lifecycle.
 *
 * Each builds hooks that attach a [[PhaseHandler]] to one phase and leave the rest empty. Combine them with `++` and
 * attach the result with `Gateway#withPhaseHooks` or `Gateway#@@`. Combining accumulates
 * rather than replaces, so several integrations can observe the same phase. Within a phase incoming sides run in
 * combination order and outgoing sides in reverse, so the outgoing side of the first handler runs last.
 *
 * Every phase except [[overrideLabels]] takes handlers that cannot fail, so a handler can never fail the request it
 * observes. Handlers run on the request path, inside whatever timeout the phase they wrap is subject to.
 *
 * A handler's outgoing side receives the value the phase produces: the phase's own [[PhaseHooks.Result]] for most
 * hooks, an [[OperationEvent]] for [[observeOperation]]. Hooks that only transform their event produce nothing
 * useful for an outgoing side, and are noted as such below.
 */
object PhaseHooks {

  /**
   * Hooks with every phase empty. The identity of `++`, and what a gateway starts with.
   */
  val empty: PhaseHooks[Any] = new PhaseHooks[Any]()

  /**
   * Brackets opening the source of one subscription: the subscribe call to the owning subgraph and the upstream
   * connection it needs. Runs once per subscription, and handler work counts against
   * [[GatewaySubscriptionConfig]]`.setupTimeout`. The outgoing [[PhaseHooks.Result]] says whether the source
   * opened.
   */
  def subscriptionSetup[R](handler: PhaseHandler[R, Event.SubscriptionSetup.type, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](subscriptionSetup = handler)

  /**
   * Brackets the work that turns one buffered source event into the response delivered to the client. Runs once per
   * event, and handler work counts against [[GatewaySubscriptionConfig]]`.eventTimeout`. The outgoing
   * [[PhaseHooks.Result]] carries that event's outcome and error count.
   */
  def subscriptionEvent[R](handler: PhaseHandler[R, Event.SubscriptionEvent.type, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](subscriptionEvent = handler)

  /**
   * Notified once per admitted subscription when its slot is released, from the finalizer of the subscription's scope,
   * whatever ended it. The event carries the termination reason and the subscription's lifetime in nanoseconds. No work
   * is bracketed, so use the incoming-only constructors of [[PhaseHandler]].
   */
  def subscriptionTerminated[R](
    handler: PhaseHandler[R, Event.SubscriptionTerminated, Nothing, Result]
  ): PhaseHooks[R] =
    new PhaseHooks[R](subscriptionTerminated = handler)

  /**
   * Notified when a subscription is admitted or turned away, before a rejection fails its caller; `accepted`
   * distinguishes the two. Pairs with [[subscriptionTerminated]] to track how many subscriptions are active. No work is
   * bracketed.
   */
  def subscriptionAdmission[R](handler: PhaseHandler[R, Event.SubscriptionAdmission, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](subscriptionAdmission = handler)

  /**
   * Notified when a subscription's buffer overflows, just before the subscription is terminated for it. No work is
   * bracketed.
   */
  def subscriptionOverflow[R](
    handler: PhaseHandler[R, Event.SubscriptionOverflow.type, Nothing, Result]
  ): PhaseHooks[R] =
    new PhaseHooks[R](subscriptionOverflow = handler)

  /**
   * Brackets execution of one query or mutation once [[routing]] has resolved it, and the responses produced when a
   * request is rejected during drain or exhausts its deadline. A subscription reaches this
   * phase only on those failure paths; its streaming work is covered by [[subscriptionSetup]] and
   * [[subscriptionEvent]]. The event carries the operation name the client supplied.
   */
  def request[R](handler: PhaseHandler[R, Event.Request, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](request = handler)

  /**
   * Brackets one HTTP attempt against a remote subgraph: the [[attemptHeaders]] handler, the round trip, and decoding
   * of the response. The event carries the subgraph, the zero-based attempt number, the request size and the resolved
   * server address; the outgoing [[PhaseHooks.Result]] adds the status code and response size whenever the
   * transport got that far.
   */
  def attempt[R](handler: PhaseHandler[R, Event.Attempt, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](attempt = handler)

  /**
   * Brackets a retried attempt against a remote subgraph, attempt 1 onwards, so it fires once per retry rather than
   * once per call. Each occurrence sits inside the same [[subgraphCall]] and around its own [[attempt]].
   */
  def retry[R](handler: PhaseHandler[R, Event.Retry, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](retry = handler)

  /**
   * Brackets assembling the response returned to the client: merging subgraph and introspection data, completing a
   * passthrough response, or producing the canned response for a preparation failure, timeout or shutdown. It covers
   * that assembly rather than the request as a whole. Use [[observeOperation]] for one observation per request.
   */
  def completion[R](handler: PhaseHandler[R, Event.Completion.type, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](completion = handler)

  /**
   * Brackets one lookup in the prepared-operation cache during [[routing]]. The event's result says whether it was a
   * hit, a miss whose preparation this request runs, or a wait on preparation already in flight for another request, so
   * only the last two bracket significant work.
   */
  def cacheAccess[R](handler: PhaseHandler[R, Event.CacheAccess, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](cacheAccess = handler)

  /**
   * Brackets preparation of an incoming request: parsing, validation, policy checks, and planning, including the
   * [[cacheAccess]] lookup that may serve it. Preparation runs alongside [[request]] rather than inside it, so this
   * phase is a sibling of that one. The outgoing [[PhaseHooks.Result]] says whether preparation succeeded.
   */
  def routing[R](handler: PhaseHandler[R, Event.Routing.type, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](routing = handler)

  /**
   * Brackets one logical call to a subgraph, remote or local, enclosing deduplication and every
   * [[attempt]] and [[retry]] the call makes. It fires per logical call even when deduplication serves the response
   * from a call another request is already making, so it does not count transport round trips. Opening a subscription
   * does not go through it. The event carries the subgraph name and the operation type.
   */
  def subgraphCall[R](handler: PhaseHandler[R, Event.SubgraphCall, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](subgraphCall = handler)

  /**
   * Selects the headers sent to a subgraph, once per header computation: per call, or once per subscription, whose
   * headers are then frozen for its lifetime. The event carries the forwarded and configured headers already resolved,
   * and the headers of the event the handler returns are the ones used. This phase only transforms its event, so an
   * outgoing side receives nothing useful.
   */
  def outboundHeaders[R](handler: PhaseHandler[R, Event.OutboundHeaders, Nothing, Any]): PhaseHooks[R] =
    new PhaseHooks[R](outboundHeaders = handler)

  /**
   * Adjusts the headers of a single attempt, once [[outboundHeaders]] has settled the call's headers, for values that
   * differ between attempts such as trace context. Runs per attempt, and once with attempt `0` when opening a
   * subscription. As with [[outboundHeaders]], the returned event's headers are the ones used and an outgoing side
   * receives nothing useful.
   */
  def attemptHeaders[R](handler: PhaseHandler[R, Event.AttemptHeaders, Nothing, Any]): PhaseHooks[R] =
    new PhaseHooks[R](attemptHeaders = handler)

  /**
   * The outermost phase: it brackets the whole request and yields exactly one [[OperationEvent]] per request, whatever
   * the outcome, including preparation failures, timeouts, shutdown, and interruption. See [[OperationEvent]] for what
   * each of those carries. A handler that wants timings brackets them itself.
   */
  def observeOperation[R](handler: PhaseHandler[R, Event.ObserveOperation, Nothing, OperationEvent]): PhaseHooks[R] =
    new PhaseHooks[R](observeOperation = handler)

  /**
   * Selects the custom progressive `@override` labels that are active for a request. The gateway resolves built-in
   * `percent(x)` labels itself and ignores unknown labels in the returned set.
   *
   * Runs during [[routing]], and is the one phase whose handler may fail: a failure fails the request with a resolution
   * error. Like the header hooks, it only transforms its event, so an outgoing side receives nothing useful. The
   * labels the returned event has activated are the ones applied.
   */
  def overrideLabels[R](handler: PhaseHandler[R, Event.OverrideLabels, Throwable, Any]): PhaseHooks[R] =
    new PhaseHooks[R](overrideLabels = handler)

  sealed trait Outcome extends Product with Serializable {
    def label: String
  }

  object Outcome {
    case object Success         extends Outcome { val label = "success"          }
    case object GraphQLError    extends Outcome { val label = "graphql_error"    }
    case object RequestError    extends Outcome { val label = "request_error"    }
    case object TransportError  extends Outcome { val label = "transport_error"  }
    case object Timeout         extends Outcome { val label = "timeout"          }
    case object LimitExceeded   extends Outcome { val label = "limit_exceeded"   }
    case object InvalidResponse extends Outcome { val label = "invalid_response" }
    case object Cancelled       extends Outcome { val label = "cancelled"        }
    case object InternalError   extends Outcome { val label = "internal_error"   }

    private[gateway] def fromResponse(response: GraphQLResponse[_]): Outcome =
      if (response.errors.isEmpty) Success else GraphQLError
  }

  sealed trait CacheResult extends Product with Serializable {
    def label: String
  }

  object CacheResult {
    case object Hit  extends CacheResult { val label = "hit"  }
    case object Miss extends CacheResult { val label = "miss" }
    case object Wait extends CacheResult { val label = "wait" }
  }

  final case class Result(
    outcome: Outcome,
    operationType: Option[OperationType] = None,
    errorCount: Int = 0,
    statusCode: Option[Int] = None,
    responseBytes: Option[Long] = None
  )

  object Result {
    private[gateway] def fromResponse(response: GraphQLResponse[_]): Result =
      Result(Outcome.fromResponse(response), errorCount = response.errors.size)

    private[gateway] def classifyExit[E, A](exit: Exit[E, A]): Result =
      fromExit(exit)(_ => Result(Outcome.Success), _ => Result(Outcome.InternalError))

    private[gateway] def classifyResponse[E](exit: Exit[E, GraphQLResponse[_]]): Result =
      fromExit(exit)(fromResponse, _ => Result(Outcome.InternalError))

    private[gateway] def fromExit[E, A](exit: Exit[E, A])(success: A => Result, failure: E => Result): Result =
      exit match {
        case Exit.Success(value) => success(value)
        case Exit.Failure(cause) => cause.failureOption.fold(fromCause(cause))(failure)
      }

    private def fromCause(cause: Cause[_]): Result =
      Result(if (cause.isInterrupted) Outcome.Cancelled else Outcome.InternalError)
  }

  sealed trait Event extends Product with Serializable

  object Event {
    case object SubscriptionSetup                                                 extends Event
    case object SubscriptionEvent                                                 extends Event
    final case class SubscriptionTerminated(reason: String, durationNanos: Long)  extends Event
    final case class SubscriptionAdmission(accepted: Boolean)                     extends Event
    case object SubscriptionOverflow                                              extends Event
    final case class Request(operationName: Option[String])                       extends Event
    case object Routing                                                           extends Event
    final case class SubgraphCall(subgraph: String, operationType: OperationType) extends Event
    final case class Attempt(
      subgraph: String,
      number: Int,
      requestBytes: Long,
      serverAddress: Option[String],
      serverPort: Option[Int]
    ) extends Event
    final case class Retry(subgraph: String, attempt: Int)                        extends Event
    case object Completion                                                        extends Event
    final case class CacheAccess(result: CacheResult)                             extends Event

    /**
     * The custom progressive `@override` labels the selected operation reached, and the subset a handler has
     * activated so far. Labels stay inactive unless a handler activates them, so several handlers can each
     * contribute without one clearing another's selection.
     *
     * The gateway resolves built-in `percent(x)` labels itself and ignores anything activated that the operation
     * did not reach.
     */
    final case class OverrideLabels(
      request: GraphQLRequest,
      reached: Set[String],
      active: Set[String] = Set.empty
    ) extends Event {
      def activate(labels: Set[String]): OverrideLabels = copy(active = active ++ labels)
    }
    final case class OutboundHeaders(subgraph: String, headers: List[Header])              extends Event
    final case class AttemptHeaders(subgraph: String, attempt: Int, headers: List[Header]) extends Event
    final case class ObserveOperation(request: GraphQLRequest)                             extends Event
  }

  private[gateway] def operationTypeLabel(operationType: OperationType): String =
    operationType match {
      case OperationType.Query        => "query"
      case OperationType.Mutation     => "mutation"
      case OperationType.Subscription => "subscription"
    }
}

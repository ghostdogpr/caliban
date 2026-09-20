package caliban.gateway

import caliban.execution.ExecutionRequest
import caliban.parsing.adt.Document
import caliban.gateway.PhaseHooks.{ Event, Outcome, Result }
import caliban.parsing.adt.OperationType
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import zio.http.Header
import zio.{ Cause, Exit, Trace, ZIO }

import scala.util.control.NoStackTrace

/**
 * Gateway handlers grouped by phase. Use named arguments to attach full [[PhaseHandler]] values,
 * or the companion helpers for individual phases. Combine bundles with `++`.
 */
final case class PhaseHooks[-R](
  operation: PhaseHandler[R, Event.Operation, Nothing, OperationEvent] = PhaseHandler.empty[Event.Operation],
  preparation: PhaseHandler[R, Event.Preparation.type, Nothing, Result] = PhaseHandler.empty[Event.Preparation.type],
  resolution: PhaseHandler[R, Event.Resolution, Throwable, Any] = PhaseHandler.empty[Event.Resolution],
  overrideLabels: PhaseHandler[R, Event.OverrideLabels, Throwable, Any] = PhaseHandler.empty[Event.OverrideLabels],
  cacheAccess: PhaseHandler[R, Event.CacheAccess, Nothing, Result] = PhaseHandler.empty[Event.CacheAccess],
  authorization: PhaseHandler[R, Event.Authorization, Throwable, Any] = PhaseHandler.empty[Event.Authorization],
  execution: PhaseHandler[R, Event.Execution, Nothing, Result] = PhaseHandler.empty[Event.Execution],
  subgraphCall: PhaseHandler[R, Event.SubgraphCall, Nothing, Result] = PhaseHandler.empty[Event.SubgraphCall],
  attempt: PhaseHandler[R, Event.Attempt, Nothing, Result] = PhaseHandler.empty[Event.Attempt],
  completion: PhaseHandler[R, Event.Completion.type, Nothing, Result] = PhaseHandler.empty[Event.Completion.type],
  subscriptionAdmission: PhaseHandler[R, Event.SubscriptionAdmission, Nothing, Result] =
    PhaseHandler.empty[Event.SubscriptionAdmission],
  subscriptionSetup: PhaseHandler[R, Event.SubscriptionSetup.type, Nothing, Result] =
    PhaseHandler.empty[Event.SubscriptionSetup.type],
  subscriptionEvent: PhaseHandler[R, Event.SubscriptionEvent.type, Nothing, Result] =
    PhaseHandler.empty[Event.SubscriptionEvent.type],
  subscriptionTerminated: PhaseHandler[R, Event.SubscriptionTerminated, Nothing, Result] =
    PhaseHandler.empty[Event.SubscriptionTerminated]
) { self =>
  val enabled: Boolean =
    self.operation.enabled ||
      self.preparation.enabled ||
      self.resolution.enabled ||
      self.overrideLabels.enabled ||
      self.cacheAccess.enabled ||
      self.authorization.enabled ||
      self.execution.enabled ||
      self.subgraphCall.enabled ||
      self.attempt.enabled ||
      self.completion.enabled ||
      self.subscriptionAdmission.enabled ||
      self.subscriptionSetup.enabled ||
      self.subscriptionEvent.enabled ||
      self.subscriptionTerminated.enabled

  def ++[R1 <: R](that: PhaseHooks[R1]): PhaseHooks[R1] =
    copy(
      operation = self.operation ++ that.operation,
      preparation = self.preparation ++ that.preparation,
      resolution = self.resolution ++ that.resolution,
      overrideLabels = self.overrideLabels ++ that.overrideLabels,
      cacheAccess = self.cacheAccess ++ that.cacheAccess,
      authorization = self.authorization ++ that.authorization,
      execution = self.execution ++ that.execution,
      subgraphCall = self.subgraphCall ++ that.subgraphCall,
      attempt = self.attempt ++ that.attempt,
      completion = self.completion ++ that.completion,
      subscriptionAdmission = self.subscriptionAdmission ++ that.subscriptionAdmission,
      subscriptionSetup = self.subscriptionSetup ++ that.subscriptionSetup,
      subscriptionEvent = self.subscriptionEvent ++ that.subscriptionEvent,
      subscriptionTerminated = self.subscriptionTerminated ++ that.subscriptionTerminated
    )

  private[gateway] def observeCompletion[R0 <: R, E](effect: ZIO[R0, E, GraphQLResponse[CalibanError]])(implicit
    trace: Trace
  ): ZIO[R0, E, GraphQLResponse[CalibanError]] =
    self.completion.run(Event.Completion)(effect)(Result.classifyResponse(_))
}

/**
 * Hooks for preparing and executing gateway operations.
 *
 * Each constructor attaches a handler to one phase. Combine hooks with `++` and attach them with
 * `Gateway#withPhaseHooks` or `Gateway#@@`. Incoming callbacks run in combination order. Outgoing callbacks
 * run in reverse order.
 *
 * Constructors follow entry order for queries and mutations. [[operation]] wraps [[preparation]] and
 * [[execution]]. Subscriptions have separate admission, setup, event, and termination hooks. Failures can
 * skip later phases.
 *
 * `resolution`, [[overrideLabels]], and `authorization` allow typed failures. Other handlers cannot fail
 * through the typed error channel. Hook work counts toward the timeout of its enclosing phase.
 *
 * Most outgoing callbacks receive a [[Result]] for that phase. [[operation]] receives an [[OperationEvent]].
 * `resolution`, [[overrideLabels]], and `authorization` receive `Unit`.
 */
object PhaseHooks {

  /**
   * No handlers. Combining these hooks with another bundle leaves that bundle unchanged.
   */
  val empty: PhaseHooks[Any] = new PhaseHooks[Any]()

  /**
   * Wraps the whole request, including preparation, execution, and response assembly.
   * The outgoing callback receives one [[OperationEvent]], including on failure, timeout, shutdown, or interruption.
   * For subscriptions, this phase ends when the gateway returns the stream. Stream processing has its own hooks.
   */
  def operation[R](handler: PhaseHandler[R, Event.Operation, Nothing, OperationEvent]): PhaseHooks[R] =
    new PhaseHooks[R](operation = handler)

  /**
   * Wraps document resolution, parsing, validation, authorization, and query planning, including [[cacheAccess]].
   * Runs before [[execution]]. The result reports whether preparation succeeded.
   */
  def preparation[R](handler: PhaseHandler[R, Event.Preparation.type, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](preparation = handler)

  /**
   * Supplies query text before parsing and cache lookup, including on cache hits. Other request fields stay unchanged.
   * Setting `cacheable` to false disables document and plan caching for this request. Later resolution handlers
   * still run, and this helper preserves any earlier decision to disable caching.
   *
   * Runs for execution and `explain(request)`, but not `check(query)`. Fail with [[Rejection]] to return a public
   * message and error code. The gateway masks unexpected failures and defects.
   * Use `PhaseHooks(resolution = handler)` to change other request fields or make caching decisions per request.
   */
  def resolution[R](
    resolve: GraphQLRequest => ZIO[R, Throwable, String],
    cacheable: Boolean = true
  ): PhaseHooks[R] =
    new PhaseHooks[R](resolution = PhaseHandler.incoming[R, Event.Resolution, Throwable] { event =>
      resolve(event.request).map(query =>
        event.copy(request = event.request.copy(query = Some(query)), cacheable = event.cacheable && cacheable)
      )
    })

  /**
   * Selects active custom `@override` labels before plan lookup, when the operation uses those labels.
   * Return an event with the chosen labels in `active`. The gateway ignores labels outside `reached` and handles
   * built-in `percent(x)` labels itself. A handler failure rejects the request with a resolution error.
   */
  def overrideLabels[R](handler: PhaseHandler[R, Event.OverrideLabels, Throwable, Any]): PhaseHooks[R] =
    new PhaseHooks[R](overrideLabels = handler)

  /**
   * Wraps a prepared-operation cache lookup, including computation on a miss or waiting for another request's
   * computation. The event identifies a hit, miss, or wait. Skipped when caching is disabled.
   */
  def cacheAccess[R](handler: PhaseHandler[R, Event.CacheAccess, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](cacheAccess = handler)

  /**
   * Allows or rejects a validated operation after variable coercion, cost checks, and planning.
   * Runs on cache hits and for `explain(request)`, but not `check(query)`. Changes to the event do not alter
   * execution. Every handler must succeed. A failure stops authorization and prevents execution.
   *
   * Fail with [[Denial]] to return a public reason. The gateway masks unexpected failures and defects.
   * Use `PhaseHooks(authorization = handler)` to attach a full [[PhaseHandler]].
   */
  def authorization[R](authorize: Event.Authorization => ZIO[R, Throwable, Unit]): PhaseHooks[R] =
    new PhaseHooks[R](authorization = PhaseHandler.incomingDiscard(authorize))

  /**
   * Wraps query or mutation execution and response assembly after [[preparation]].
   * Also covers error responses for preparation failures, request timeouts, and shutdown rejections.
   * Successful subscriptions use [[subscriptionSetup]] and [[subscriptionEvent]] instead.
   * The event carries the operation name supplied by the client.
   */
  def execution[R](handler: PhaseHandler[R, Event.Execution, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](execution = handler)

  /**
   * Wraps one local or remote subgraph call, including deduplication and retries, or opening a subscription.
   * Runs for each caller, even when callers share a single remote request. Use [[attempt]] to count transport requests.
   *
   * For remote calls, runs after configured headers are resolved. The returned event supplies the headers used for
   * deduplication and retries. Local calls have no transport headers and ignore header changes.
   * Callbacks use the enclosing gateway deadline. The remote timeout bounds header acquisition and transport.
   * Subscription connections keep their opening headers. Calls made while processing events run this hook separately.
   */
  def subgraphCall[R](handler: PhaseHandler[R, Event.SubgraphCall, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](subgraphCall = handler)

  /**
   * Wraps one remote request and response decoding, or opening a remote subscription.
   * Runs after query deduplication. Attempt number zero is the first request. Higher numbers are retries.
   * The returned event supplies the headers to send. Other event fields describe the attempt.
   *
   * The result includes the HTTP status code and response size when available for queries and mutations.
   * Opening a subscription uses attempt zero, and its headers remain fixed for the stream's lifetime.
   */
  def attempt[R](handler: PhaseHandler[R, Event.Attempt, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](attempt = handler)

  /**
   * Wraps client response assembly, including merging subgraph results and producing gateway error responses.
   * Use [[operation]] to observe the whole request.
   */
  def completion[R](handler: PhaseHandler[R, Event.Completion.type, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](completion = handler)

  /**
   * Reports whether a subscription was admitted. The event's `accepted` flag is false for a rejection.
   * Together with [[subscriptionTerminated]], this hook can track active subscriptions.
   * This is a notification with no work to wrap. Use `PhaseHandler.incomingDiscard`.
   */
  def subscriptionAdmission[R](handler: PhaseHandler[R, Event.SubscriptionAdmission, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](subscriptionAdmission = handler)

  /**
   * Wraps opening a subscription source, including any upstream connection.
   * Runs once per subscription. Hook work counts toward `setupTimeout` in [[GatewaySubscriptionConfig]].
   * The result reports whether the source opened.
   */
  def subscriptionSetup[R](handler: PhaseHandler[R, Event.SubscriptionSetup.type, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](subscriptionSetup = handler)

  /**
   * Wraps processing one source event into a client response, including any subgraph calls it needs.
   * Hook work counts toward `eventTimeout` in [[GatewaySubscriptionConfig]].
   * The result reports the event's outcome and GraphQL error count.
   */
  def subscriptionEvent[R](handler: PhaseHandler[R, Event.SubscriptionEvent.type, Nothing, Result]): PhaseHooks[R] =
    new PhaseHooks[R](subscriptionEvent = handler)

  /**
   * Reports when an admitted subscription releases its slot, once per subscription during scope cleanup.
   * The event carries its lifetime in nanoseconds and termination reason, including `SUBSCRIPTION_OVERFLOW`.
   * This is a notification with no work to wrap. Use `PhaseHandler.incomingDiscard`.
   */
  def subscriptionTerminated[R](
    handler: PhaseHandler[R, Event.SubscriptionTerminated, Nothing, Result]
  ): PhaseHooks[R] =
    new PhaseHooks[R](subscriptionTerminated = handler)

  /**
   * Resolves document IDs from a fixed registry. Registered text replaces any query text in the request.
   * The gateway never registers new documents or falls back to client query text.
   *
   * Missing, malformed, or empty IDs fail with `TRUSTED_DOCUMENT_ID_INVALID`.
   * Unknown IDs fail with `TRUSTED_DOCUMENT_NOT_FOUND`. Operations still pass through `authorization`.
   */
  def trustedDocuments(documents: Map[String, String])(extractId: GraphQLRequest => Option[String]): PhaseHooks[Any] =
    resolution[Any] { request =>
      extractId(request).filter(_.nonEmpty) match {
        case None     =>
          ZIO.fail(Rejection("A non-empty trusted document ID is required.", "TRUSTED_DOCUMENT_ID_INVALID"))
        case Some(id) =>
          documents.get(id) match {
            case Some(document) => ZIO.succeed(document)
            case None           => ZIO.fail(Rejection("Trusted document not found.", "TRUSTED_DOCUMENT_NOT_FOUND"))
          }
      }
    }

  /**
   * Checks `@authenticated` and `@requiresScopes` using claims already verified by the application.
   * `None` means anonymous. `Some` means authenticated, even when `scopes` returns an empty set.
   * Reads claims once per protected authorization check, including cache hits. Unprotected operations skip the lookup.
   *
   * A scope requirement passes when the caller has every scope in at least one alternative. An empty list of
   * alternatives or an empty alternative requires only authentication. All requirements must pass, including
   * those on possible runtime branches.
   */
  def fromClaims[R, C](readClaims: ZIO[R, Throwable, Option[C]])(scopes: C => Set[String]): PhaseHooks[R] =
    authorization[R] { operation =>
      if (operation.securityRequirements.isEmpty) ZIO.unit
      else
        readClaims.flatMap {
          case None         => ZIO.fail(Denial())
          case Some(claims) =>
            val granted = scopes(claims)
            val allowed = operation.securityRequirements.forall(_.directives.forall {
              case SecurityDirective.UnsupportedPolicy            => false
              case SecurityDirective.Authenticated                => true
              case SecurityDirective.RequiresScopes(alternatives) =>
                alternatives.isEmpty || alternatives.exists(_.forall(granted.contains))
            })
            if (allowed) ZIO.unit else ZIO.fail(Denial())
        }
    }

  /**
   * A public rejection from `resolution`. Fail with `ZIO.fail` to expose `message` and `extensions.code`.
   * Throwing this exception produces a defect, which the gateway masks.
   */
  final case class Rejection(message: String, code: String) extends Exception(message) with NoStackTrace

  /**
   * A public rejection from `authorization`. Fail with `ZIO.fail` to expose `reason`.
   * The gateway masks thrown exceptions and failures accompanied by defects.
   */
  final case class Denial(reason: String = "Operation rejected by gateway policy.")
      extends Exception(reason)
      with NoStackTrace

  /**
   * Security directives that apply to a type or field selected by the operation.
   * Every requirement and directive must pass. `fieldName = None` means the directives apply to the type.
   */
  final case class SecurityRequirement(typeName: String, fieldName: Option[String], directives: List[SecurityDirective])

  sealed trait SecurityDirective

  object SecurityDirective {
    case object Authenticated extends SecurityDirective

    /**
     * An unsupported `@policy` directive. The gateway rejects operations that select it.
     */
    case object UnsupportedPolicy extends SecurityDirective

    /**
     * Alternative sets of required scopes. The caller needs every scope in at least one inner list.
     */
    final case class RequiresScopes(scopes: List[List[String]]) extends SecurityDirective
  }

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
    final case class Operation(request: GraphQLRequest)                             extends Event
    case object Preparation                                                         extends Event
    final case class Resolution(request: GraphQLRequest, cacheable: Boolean = true) extends Event

    /**
     * Custom `@override` labels used by the operation and the labels activated by earlier handlers.
     * Call `activate` to add labels without clearing earlier choices. Labels outside `reached` are ignored.
     * The gateway handles built-in `percent(x)` labels separately.
     */
    final case class OverrideLabels(
      request: GraphQLRequest,
      reached: Set[String],
      active: Set[String] = Set.empty
    ) extends Event {
      def activate(labels: Set[String]): OverrideLabels = copy(active = active ++ labels)
    }
    final case class CacheAccess(result: CacheResult)                            extends Event
    final case class Authorization(
      request: GraphQLRequest,
      document: Document,
      executionRequest: ExecutionRequest,
      securityRequirements: List[SecurityRequirement]
    ) extends Event
    final case class Execution(operationName: Option[String])                    extends Event
    final case class SubgraphCall(
      subgraph: String,
      operationType: OperationType,
      headers: List[Header] = Nil
    ) extends Event
    final case class Attempt(
      subgraph: String,
      number: Int,
      requestBytes: Long,
      serverAddress: Option[String],
      serverPort: Option[Int],
      headers: List[Header],
      method: String = "POST"
    ) extends Event
    case object Completion                                                       extends Event
    final case class SubscriptionAdmission(accepted: Boolean)                    extends Event
    case object SubscriptionSetup                                                extends Event
    case object SubscriptionEvent                                                extends Event
    final case class SubscriptionTerminated(reason: String, durationNanos: Long) extends Event
  }

  private[gateway] def operationTypeLabel(operationType: OperationType): String =
    operationType match {
      case OperationType.Query        => "query"
      case OperationType.Mutation     => "mutation"
      case OperationType.Subscription => "subscription"
    }
}

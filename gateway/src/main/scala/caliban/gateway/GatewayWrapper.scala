package caliban.gateway

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import caliban.gateway.GatewayWrapper.{ Event, Outcome, Result }
import caliban.parsing.adt.OperationType
import sttp.model.Header
import zio.{ Cause, Exit, Trace, URIO, ZIO }

/**
 * An integration seam around the gateway execution lifecycle.
 *
 * Events passed to [[wrap]] contain bounded metadata only. Specialized hooks document any request data they receive.
 * Wrappers can be combined with [[|+|]] and attached to a [[Gateway]] with `@@`.
 */
abstract class GatewayWrapper[-R] { self =>

  private[gateway] def enabled: Boolean = true

  /**
   * Wraps one gateway lifecycle event.
   *
   * `result` is evaluated inside the wrapper, before its scope closes. This lets span, metric, logging, and other
   * integrations observe the same typed completion without a separate callback protocol.
   */
  def wrap[R0 <: R, E, A](event: GatewayWrapper.Event)(effect: ZIO[R0, E, A])(
    result: Exit[E, A] => GatewayWrapper.Result
  )(implicit trace: Trace): ZIO[R0, E, A]

  private[gateway] final def observeCompletion[R0 <: R, E](effect: ZIO[R0, E, GraphQLResponse[CalibanError]])(implicit
    trace: Trace
  ): ZIO[R0, E, GraphQLResponse[CalibanError]] =
    if (!enabled) effect
    else
      wrap(Event.Completion)(effect)(
        Result.fromExit(_)(Result.fromResponse, _ => Result(Outcome.InternalError))
      )

  /**
   * Transforms semantic remote-call headers before in-flight identity is selected.
   */
  def outboundHeaders(subgraph: String, headers: List[Header])(implicit trace: Trace): URIO[R, List[Header]] =
    ZIO.succeed(headers)

  /**
   * Adds per-attempt transport context after in-flight identity is selected.
   */
  def attemptHeaders(subgraph: String, attempt: Int, headers: List[Header])(implicit
    trace: Trace
  ): URIO[R, List[Header]] =
    ZIO.succeed(headers)

  /**
   * Selects custom progressive `@override` labels that are active for a request.
   *
   * The gateway calls this hook once per request when the selected operation reaches at least one custom label. The
   * request includes the query text, operation name, variables, and extensions. An [[OperationResolver]] can replace
   * the query text before this hook runs. The supplied set contains only the custom labels reached by the operation.
   * The gateway resolves built-in `percent(x)` labels itself and ignores unknown labels in the returned set. Custom
   * labels remain inactive unless a wrapper activates them.
   */
  def activeOverrideLabels(request: GraphQLRequest, labels: Set[String])(implicit
    trace: Trace
  ): ZIO[R, Throwable, Set[String]] =
    ZIO.succeed(Set.empty)

  /**
   * Combines wrappers. Effects and outbound header transforms are applied from left to right.
   */
  final def |+|[R1 <: R](that: GatewayWrapper[R1]): GatewayWrapper[R1] =
    if (!self.enabled) that
    else if (!that.enabled) self
    else
      new GatewayWrapper[R1] {
        def wrap[R0 <: R1, E, A](event: GatewayWrapper.Event)(effect: ZIO[R0, E, A])(
          result: Exit[E, A] => GatewayWrapper.Result
        )(implicit trace: Trace): ZIO[R0, E, A] =
          self.wrap(event)(that.wrap(event)(effect)(result))(result)

        override def outboundHeaders(subgraph: String, headers: List[Header])(implicit
          trace: Trace
        ): URIO[R1, List[Header]] =
          self.outboundHeaders(subgraph, headers).flatMap(that.outboundHeaders(subgraph, _))

        override def attemptHeaders(subgraph: String, attempt: Int, headers: List[Header])(implicit
          trace: Trace
        ): URIO[R1, List[Header]] =
          self.attemptHeaders(subgraph, attempt, headers).flatMap(that.attemptHeaders(subgraph, attempt, _))

        override def activeOverrideLabels(request: GraphQLRequest, labels: Set[String])(implicit
          trace: Trace
        ): ZIO[R1, Throwable, Set[String]] =
          self.activeOverrideLabels(request, labels).zipWith(that.activeOverrideLabels(request, labels))(_ ++ _)
      }
}

object GatewayWrapper {
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
  }

  sealed trait CacheResult extends Product with Serializable {
    def label: String
  }

  object CacheResult {
    case object Hit  extends CacheResult { val label = "hit"  }
    case object Miss extends CacheResult { val label = "miss" }
    case object Wait extends CacheResult { val label = "wait" }
  }

  sealed trait AdmissionKind extends Product with Serializable {
    def label: String
  }

  object AdmissionKind {
    case object Request           extends AdmissionKind { val label = "request"            }
    case object Subgraph          extends AdmissionKind { val label = "subgraph"           }
    case object SubscriptionSetup extends AdmissionKind { val label = "subscription_setup" }
    case object SubscriptionEvent extends AdmissionKind { val label = "subscription_event" }
  }

  final case class Result(
    outcome: Outcome,
    operationType: Option[OperationType] = None,
    errorCount: Int = 0,
    statusCode: Option[Int] = None,
    responseBytes: Option[Long] = None
  )

  object Result {
    private[gateway] def fromResponse(response: caliban.GraphQLResponse[_]): Result =
      Result(
        if (response.errors.isEmpty) Outcome.Success else Outcome.GraphQLError,
        errorCount = response.errors.size
      )

    private[gateway] def classifyExit[E, A](exit: Exit[E, A]): Result =
      fromExit(exit)(_ => Result(Outcome.Success), _ => Result(Outcome.InternalError))

    private[gateway] def fromExit[E, A](exit: Exit[E, A])(success: A => Result, failure: E => Result): Result =
      exit match {
        case Exit.Success(value) => success(value)
        case Exit.Failure(cause) => cause.failureOption.fold(fromCause(cause))(failure)
      }

    private[gateway] def fromCause(cause: Cause[_]): Result =
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
    final case class Admission(kind: AdmissionKind)                               extends Event
  }

  private[gateway] def operationTypeLabel(operationType: OperationType): String =
    operationType match {
      case OperationType.Query        => "query"
      case OperationType.Mutation     => "mutation"
      case OperationType.Subscription => "subscription"
    }

  /**
   * Creates a wrapper that activates custom progressive `@override` labels per request.
   * Resolver failures are masked as gateway execution errors. When several such wrappers are combined, their active
   * label sets are unioned.
   */
  def overrideLabels[R](
    resolve: (GraphQLRequest, Set[String]) => ZIO[R, Throwable, Set[String]]
  ): GatewayWrapper[R] =
    new GatewayWrapper[R] {
      def wrap[R0 <: R, E, A](event: Event)(effect: ZIO[R0, E, A])(result: Exit[E, A] => Result)(implicit
        trace: Trace
      ): ZIO[R0, E, A] = effect

      override def activeOverrideLabels(request: GraphQLRequest, labels: Set[String])(implicit
        trace: Trace
      ): ZIO[R, Throwable, Set[String]] =
        resolve(request, labels)
    }

  val empty: GatewayWrapper[Any] = new GatewayWrapper[Any] {
    override private[gateway] val enabled: Boolean = false

    def wrap[R0, E, A](event: Event)(effect: ZIO[R0, E, A])(result: Exit[E, A] => Result)(implicit
      trace: Trace
    ): ZIO[R0, E, A] = effect
  }
}

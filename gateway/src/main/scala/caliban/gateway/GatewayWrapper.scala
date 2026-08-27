package caliban.gateway

import caliban.parsing.adt.OperationType
import sttp.model.Header
import zio.{ Cause, Exit, Trace, URIO, ZIO }

/**
 * An integration seam around the gateway execution lifecycle.
 *
 * Lifecycle events contain bounded metadata only: raw GraphQL documents and variables are never exposed. Wrappers can
 * be combined with [[|+|]] and attached to a [[Gateway]] with `@@`.
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
    case object Http4xx         extends Outcome { val label = "http_4xx"         }
    case object Http5xx         extends Outcome { val label = "http_5xx"         }
    case object HttpError       extends Outcome { val label = "http_error"       }
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
    case object Request  extends AdmissionKind { val label = "request"  }
    case object Subgraph extends AdmissionKind { val label = "subgraph" }
  }

  sealed trait DeduplicationResult extends Product with Serializable {
    def label: String
  }

  object DeduplicationResult {
    case object Start extends DeduplicationResult { val label = "start" }
    case object Join  extends DeduplicationResult { val label = "join"  }
    case object Wait  extends DeduplicationResult { val label = "wait"  }
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
    final case class AdmissionWait(kind: AdmissionKind)                           extends Event
    final case class Admission(kind: AdmissionKind)                               extends Event
    final case class Deduplication(result: DeduplicationResult)                   extends Event
    case object RequestOverdue                                                    extends Event
  }

  private[gateway] def operationTypeLabel(operationType: OperationType): String =
    operationType match {
      case OperationType.Query        => "query"
      case OperationType.Mutation     => "mutation"
      case OperationType.Subscription => "subscription"
    }

  val empty: GatewayWrapper[Any] = new GatewayWrapper[Any] {
    override private[gateway] val enabled: Boolean = false

    def wrap[R0, E, A](event: Event)(effect: ZIO[R0, E, A])(result: Exit[E, A] => Result)(implicit
      trace: Trace
    ): ZIO[R0, E, A] = effect
  }
}

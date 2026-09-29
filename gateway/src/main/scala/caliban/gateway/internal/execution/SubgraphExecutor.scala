package caliban.gateway.internal.execution

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.execution.Field
import caliban.gateway.PhaseHooks
import caliban.gateway.PhaseHooks.{ Event, Outcome, Result }
import caliban.gateway.internal.execution.SubgraphExecutor.ErrorPolicy
import caliban.parsing.adt.OperationType
import caliban._
import zio.stream.ZStream
import zio.{ Exit, Scope, Trace, ZIO }

import scala.util.control.NoStackTrace

/**
 * Executes GraphQL work against one composed subgraph.
 */
private[gateway] trait SubgraphExecutor[-R] {
  def errorPolicy: ErrorPolicy

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]]

  def forSubscription(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, SubgraphExecutor[R]] = ZIO.succeed(this)

  def subscribe(request: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]]

}

private[gateway] object SubgraphExecutor {
  private[execution] val resultFromExit: Exit[Failure, GraphQLResponse[CalibanError]] => Result =
    Result.fromExit(_)(Result.fromResponse, failure => Result(failureOutcome(failure)))

  // Adapt native field-value streams and gateway response envelopes without changing core execution.
  def subscriptionResponses(
    response: GraphQLResponse[CalibanError]
  ): ZStream[Any, Throwable, GraphQLResponse[CalibanError]] =
    response.data match {
      // Top-level streams with hasNext (even false) are incremental; without it, elements are full subscription responses.
      case StreamValue(stream) if response.hasNext.isEmpty =>
        stream.mapZIO(value =>
          ZIO
            .fromOption(GraphQLResponse.fromResponseValue(value))
            .orElseFail(CalibanError.ExecutionError("Invalid subscription response."))
        )
      case ObjectValue((name, StreamValue(stream)) :: Nil) =>
        stream.map(value => response.copy(data = ObjectValue(List(name -> value))))
      case _                                               => ZStream.succeed(response)
    }

  def failureOutcome(failure: Failure): Outcome =
    failure match {
      case TransportFailure(_)                                         => Outcome.TransportError
      case TimeoutFailure                                              => Outcome.Timeout
      case HeaderFailure(_) | InvalidRequest                           => Outcome.RequestError
      case RequestTooLarge | ResponseTooLarge | ResponseNestingTooDeep =>
        Outcome.LimitExceeded
      case HttpFailure(status) if status >= 400 && status < 500        => Outcome.RequestError
      case HttpFailure(_)                                              => Outcome.TransportError
      case RedirectResponse | UnsupportedMediaType | InvalidResponse   => Outcome.InvalidResponse
    }

  sealed trait ErrorPolicy {
    def forFetch(fields: List[Field], errors: List[CalibanError]): List[CalibanError]

    def entityFallback(error: CalibanError.ExecutionError, path: List[PathValue]): CalibanError.ExecutionError

    def place(
      fields: List[Field],
      base: List[PathValue],
      error: CalibanError.ExecutionError,
      tail: List[PathValue]
    ): Option[CalibanError.ExecutionError] =
      tail match {
        case (key @ PathValue.Key(name)) :: rest =>
          fields.find(_.aliasedName == name).map { field =>
            if (RemoteError.hasClientPath(field, rest, field.fieldType)) error.copy(path = base ::: tail)
            else entityFallback(error, base ::: key :: Nil)
          }
        case _                                   => None
      }
  }

  object ErrorPolicy {
    case object Local extends ErrorPolicy {
      def forFetch(fields: List[Field], errors: List[CalibanError]): List[CalibanError] =
        errors.map {
          case error: CalibanError.ExecutionError => place(fields, Nil, error, error.path).getOrElse(error)
          case error                              => error
        }

      def entityFallback(error: CalibanError.ExecutionError, path: List[PathValue]): CalibanError.ExecutionError =
        error.copy(path = path, locationInfo = None)
    }

    case object Remote extends ErrorPolicy {
      def forFetch(fields: List[Field], errors: List[CalibanError]): List[CalibanError] = {
        val placed = errors.map {
          case error: CalibanError.ExecutionError => place(fields, Nil, error, error.path)
          case error                              => Some(error)
        }

        placed.flatten ::: (if (placed.exists(_.isEmpty)) RemoteError.forFields(fields) else Nil)
      }

      def entityFallback(error: CalibanError.ExecutionError, path: List[PathValue]): CalibanError.ExecutionError =
        RemoteError.at(path)
    }
  }

  final class Unavailable(name: String) extends SubgraphExecutor[Any] {
    private val error            = CalibanError.ExecutionError(s"No executor is configured for subgraph '$name'.")
    val errorPolicy: ErrorPolicy = ErrorPolicy.Local

    def execute(request: GraphQLRequest, operationType: OperationType)(implicit
      trace: Trace
    ): ZIO[Any, Failure, GraphQLResponse[CalibanError]] =
      ZIO.succeed(GraphQLResponse(Value.NullValue, List(error)))

    def subscribe(request: GraphQLRequest)(implicit
      trace: Trace
    ): ZIO[Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]] =
      ZIO.fail(error)
  }

  sealed trait Failure                                extends NoStackTrace
  final case class TransportFailure(error: Throwable) extends Failure {
    override def getCause: Throwable = error
  }
  case object TimeoutFailure                          extends Failure
  final case class HeaderFailure(error: Throwable)    extends Failure {
    override def getCause: Throwable = error
  }
  case object InvalidRequest                          extends Failure
  case object RequestTooLarge                         extends Failure
  final case class HttpFailure(statusCode: Int)       extends Failure
  case object RedirectResponse                        extends Failure
  case object UnsupportedMediaType                    extends Failure
  case object ResponseTooLarge                        extends Failure
  case object ResponseNestingTooDeep                  extends Failure
  case object InvalidResponse                         extends Failure
}

private[gateway] final class LocalSubgraphExecutor[-R](
  name: String,
  interpreter: GraphQLInterpreter[R, CalibanError],
  hooks: PhaseHooks[R]
) extends SubgraphExecutor[R] {
  val errorPolicy: ErrorPolicy = ErrorPolicy.Local

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
    hooks.subgraphCall.run(Event.SubgraphCall(name, operationType))(
      GraphQLResponseContext.capture(interpreter.executeRequest(request.copy(extensions = None))).map(_.value)
    )(SubgraphExecutor.resultFromExit)

  override def subscribe(request: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]] =
    hooks.subgraphCall.run(Event.SubgraphCall(name, OperationType.Subscription))(
      interpreter.executeRequest(request.copy(extensions = None)).map(SubgraphExecutor.subscriptionResponses)
    )(Result.classifyExit(Outcome.InternalError))
}

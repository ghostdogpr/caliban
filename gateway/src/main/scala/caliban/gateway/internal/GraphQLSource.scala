package caliban.gateway.internal

import caliban.execution.Field
import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayWrapper.{ Event, Outcome, Result }
import caliban.gateway.internal.GraphQLSource.ErrorPolicy
import caliban.parsing.adt.OperationType
import caliban.{ CalibanError, GraphQLInterpreter, GraphQLRequest, GraphQLResponse, PathValue }
import zio.{ Trace, ZIO }

import scala.util.control.NoStackTrace

/**
 * Executes GraphQL work against one composed subgraph.
 */
private[gateway] trait GraphQLSource[-R] {
  def errorPolicy: ErrorPolicy

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, GraphQLSource.Failure, GraphQLResponse[CalibanError]]

  def admittedBy[R1](gate: ExecutionGate, wrapper: GatewayWrapper[R1]): GraphQLSource[R with R1] =
    new GatedGraphQLSource(this, gate, wrapper)
}

private[gateway] final class ObservedGraphQLSource[R](
  name: String,
  underlying: GraphQLSource[R],
  wrapper: GatewayWrapper[R]
) extends GraphQLSource[R] {
  val errorPolicy: ErrorPolicy = underlying.errorPolicy

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
    if (!wrapper.enabled) underlying.execute(request, operationType)
    else
      wrapper.wrap(Event.SourceCall(name, operationType))(underlying.execute(request, operationType))(
        Result.fromExit(_)(
          response =>
            Result(
              if (response.errors.isEmpty) Outcome.Success else Outcome.GraphQLError,
              errorCount = response.errors.size
            ),
          failure => Result(GraphQLSource.failureOutcome(failure))
        )
      )

}

private[gateway] final class GatedGraphQLSource[-R](
  underlying: GraphQLSource[R],
  gate: ExecutionGate,
  wrapper: GatewayWrapper[R]
) extends GraphQLSource[R] {
  val errorPolicy: ErrorPolicy = underlying.errorPolicy

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
    gate.observed(wrapper)(underlying.execute(request, operationType))
}

private[gateway] object GraphQLSource {
  def failureOutcome(failure: Failure): Outcome =
    failure match {
      case TransportFailure                                                                        => Outcome.TransportError
      case TimeoutFailure                                                                          => Outcome.Timeout
      case HeaderFailure | InvalidRequest                                                          => Outcome.RequestError
      case RequestTooLarge | ResponseTooLarge | ResponseNestingTooDeep | ResponseStructureTooLarge =>
        Outcome.LimitExceeded
      case HttpFailure(status) if status >= 400 && status < 500                                    => Outcome.Http4xx
      case HttpFailure(status) if status >= 500 && status < 600                                    => Outcome.Http5xx
      case HttpFailure(_)                                                                          => Outcome.HttpError
      case RedirectResponse | UnsupportedMediaType | InvalidResponse                               => Outcome.InvalidResponse
    }

  sealed trait ErrorPolicy {
    def passthrough(fields: List[Field], errors: List[CalibanError]): List[CalibanError]

    def routed(fields: List[Field], errors: List[CalibanError]): List[CalibanError]

    def unusableEntity(error: CalibanError.ExecutionError, path: List[PathValue]): CalibanError.ExecutionError
  }

  object ErrorPolicy {
    case object Local extends ErrorPolicy {
      def passthrough(fields: List[Field], errors: List[CalibanError]): List[CalibanError] = errors

      def routed(fields: List[Field], errors: List[CalibanError]): List[CalibanError] =
        errors.map {
          case error: CalibanError.ExecutionError => error.copy(locationInfo = None)
          case error                              => error
        }

      def unusableEntity(error: CalibanError.ExecutionError, path: List[PathValue]): CalibanError.ExecutionError =
        error.copy(path = path, locationInfo = None)
    }

    case object Remote extends ErrorPolicy {
      def passthrough(fields: List[Field], errors: List[CalibanError]): List[CalibanError] = routed(fields, errors)

      def routed(fields: List[Field], errors: List[CalibanError]): List[CalibanError] =
        errors.flatMap {
          case error: CalibanError.ExecutionError if RemoteError.hasClientPath(fields, error.path) =>
            error.copy(locationInfo = None) :: Nil
          case error: CalibanError.ExecutionError                                                  =>
            error.path match {
              case PathValue.Key(name) :: _ if fields.exists(_.aliasedName == name) =>
                RemoteError.at(List(PathValue.Key(name))) :: Nil
              case _                                                                =>
                fields.map(field => RemoteError.at(List(PathValue.Key(field.aliasedName))))
            }
          case error                                                                               => error :: Nil
        }

      def unusableEntity(error: CalibanError.ExecutionError, path: List[PathValue]): CalibanError.ExecutionError =
        RemoteError.at(path)
    }
  }

  sealed trait Failure                          extends NoStackTrace
  case object TransportFailure                  extends Failure
  case object TimeoutFailure                    extends Failure
  case object HeaderFailure                     extends Failure
  case object InvalidRequest                    extends Failure
  case object RequestTooLarge                   extends Failure
  final case class HttpFailure(statusCode: Int) extends Failure
  case object RedirectResponse                  extends Failure
  case object UnsupportedMediaType              extends Failure
  case object ResponseTooLarge                  extends Failure
  case object ResponseNestingTooDeep            extends Failure
  case object ResponseStructureTooLarge         extends Failure
  case object InvalidResponse                   extends Failure
}

private[gateway] final class LocalGraphQLSource[-R](interpreter: GraphQLInterpreter[R, CalibanError])
    extends GraphQLSource[R] {
  val errorPolicy: ErrorPolicy = ErrorPolicy.Local

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
    interpreter.executeRequest(request)
}

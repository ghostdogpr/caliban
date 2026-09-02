package caliban.gateway.internal.execution

import caliban.{ CalibanError, GraphQLInterpreter, GraphQLRequest, GraphQLResponse, GraphQLResponseContext, PathValue }
import caliban.execution.Field
import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayWrapper.{ Event, Outcome, Result }
import caliban.gateway.internal.SubscriptionTermination
import caliban.gateway.internal.execution.SubgraphExecutor.ErrorPolicy
import caliban.parsing.adt.OperationType
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.schema.Types
import zio.{ Exit, Scope, Trace, ZIO }
import zio.stream.ZStream

import scala.util.control.NoStackTrace

/**
 * Executes GraphQL work against one composed subgraph.
 */
private[gateway] trait SubgraphExecutor[-R] {
  def errorPolicy: ErrorPolicy

  def forSubscription(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, SubgraphExecutor[R]] = ZIO.succeed(this)

  def subscribe(request: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]] =
    ZIO.fail(SubscriptionTermination.Source)

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]]
}

private[gateway] final class ObservedSubgraphExecutor[R](
  name: String,
  underlying: SubgraphExecutor[R],
  wrapper: GatewayWrapper[R]
) extends SubgraphExecutor[R] {
  val errorPolicy: ErrorPolicy = underlying.errorPolicy

  override def forSubscription(implicit trace: Trace)                    =
    underlying.forSubscription.map(new ObservedSubgraphExecutor(name, _, wrapper))
  override def subscribe(request: GraphQLRequest)(implicit trace: Trace) = underlying.subscribe(request)

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
    if (!wrapper.enabled) underlying.execute(request, operationType)
    else
      wrapper.wrap(Event.SubgraphCall(name, operationType))(underlying.execute(request, operationType))(
        SubgraphExecutor.resultFromExit
      )

}

private[gateway] object SubgraphExecutor {
  private[execution] val resultFromExit: Exit[Failure, GraphQLResponse[CalibanError]] => Result =
    Result.fromExit(_)(Result.fromResponse, failure => Result(failureOutcome(failure)))

  // Adapt native field-value streams and gateway response envelopes without changing core execution.
  def responses(response: GraphQLResponse[CalibanError]): ZStream[Any, Throwable, GraphQLResponse[CalibanError]] =
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
      case TransportFailure(_)                                                                     => Outcome.TransportError
      case TimeoutFailure                                                                          => Outcome.Timeout
      case HeaderFailure(_) | InvalidRequest                                                       => Outcome.RequestError
      case RequestTooLarge | ResponseTooLarge | ResponseNestingTooDeep | ResponseStructureTooLarge =>
        Outcome.LimitExceeded
      case HttpFailure(status) if status >= 400 && status < 500                                    => Outcome.RequestError
      case HttpFailure(_)                                                                          => Outcome.TransportError
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

      def routed(fields: List[Field], errors: List[CalibanError]): List[CalibanError] = {
        val (routedErrors, needsFallback) = errors.foldLeft((List.empty[CalibanError], false)) {
          case ((routed, fallback), error: CalibanError.ExecutionError)
              if RemoteError.hasClientPath(fields, error.path) =>
            (error.copy(locationInfo = None) :: routed, fallback)
          case ((routed, fallback), error: CalibanError.ExecutionError) =>
            error.path match {
              case PathValue.Key(name) :: _ if fields.exists(_.aliasedName == name) =>
                (RemoteError.at(List(PathValue.Key(name))) :: routed, fallback)
              case _                                                                =>
                (routed, true)
            }
          case ((routed, fallback), error)                              =>
            (error :: routed, fallback)
        }

        routedErrors.reverse :::
          (if (needsFallback) RemoteError.forFields(fields) else Nil)
      }

      def unusableEntity(error: CalibanError.ExecutionError, path: List[PathValue]): CalibanError.ExecutionError =
        RemoteError.at(path)
    }
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
  case object ResponseStructureTooLarge               extends Failure
  case object InvalidResponse                         extends Failure
}

private[gateway] final class LocalSubgraphExecutor[-R](interpreter: GraphQLInterpreter[R, CalibanError])
    extends SubgraphExecutor[R] {
  val errorPolicy: ErrorPolicy = ErrorPolicy.Local

  override def subscribe(request: GraphQLRequest)(implicit trace: Trace) =
    interpreter.executeRequest(request.copy(extensions = None)).map(SubgraphExecutor.responses)

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
    GraphQLResponseContext.capture(interpreter.executeRequest(request.copy(extensions = None))).map(_.value)
}

private[gateway] object RemoteError {

  private val Message = "Remote GraphQL request failed."

  def at(path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError(Message, path = path)

  def nullObject(fields: List[Field]): ObjectValue =
    ObjectValue(fields.map(field => field.aliasedName -> caliban.Value.NullValue))

  def forFields(fields: List[Field]): List[CalibanError.ExecutionError] =
    fields.map(field => at(List(PathValue.Key(field.aliasedName))))

  def disclose(
    error: CalibanError,
    remoteErrorMessages: Boolean
  ): CalibanError.ExecutionError =
    error match {
      case value: CalibanError.ExecutionError =>
        val extensions = value.extensions.flatMap { current =>
          val retained = current.fields.filter { case (name, _) => name == "code" }
          if (retained.isEmpty) None else Some(ObjectValue(retained))
        }
        value.copy(
          msg = if (remoteErrorMessages) value.msg else Message,
          locationInfo = None,
          innerThrowable = None,
          extensions = extensions
        )
      case _                                  => at(Nil)
    }

  def hasClientPath(fields: List[Field], path: List[PathValue]): Boolean =
    path match {
      case PathValue.Key(name) :: tail =>
        fields.find(_.aliasedName == name).exists(field => hasClientSubpath(field, tail))
      case _                           => false
    }

  private def hasClientSubpath(field: Field, path: List[PathValue]): Boolean = {
    def loop(current: Field, remaining: List[PathValue], currentType: caliban.introspection.adt.__Type): Boolean =
      remaining match {
        case Nil                                          => true
        case PathValue.Index(index) :: tail if index >= 0 =>
          Types.listOf(currentType).exists(itemType => loop(current, tail, itemType))
        case PathValue.Key(name) :: tail                  =>
          if (Types.listOf(currentType).nonEmpty) false
          else
            current.fields
              .find(_.aliasedName == name)
              .exists(child => loop(child, tail, child.fieldType))
        case _                                            => false
      }

    loop(field, path, field.fieldType)
  }
}

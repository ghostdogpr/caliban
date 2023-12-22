package caliban.federation.tracing

import caliban.CalibanError.ExecutionError
import caliban.InternalUtils.syntax._
import caliban.ResponseValue.ObjectValue
import caliban.Value.{ IntValue, StringValue }
import caliban._
import caliban.execution.FieldInfo
import caliban.wrappers.Wrapper.{ EffectfulWrapper, FieldWrapper, OverallWrapper }
import com.google.protobuf.timestamp.Timestamp
import mdg.engine.proto.reports.Trace
import mdg.engine.proto.reports.Trace.{ Error, Location, Node }
import zio._
import zio.query.ZQuery

import java.util.Base64
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

/**
 * Implements the federated tracing specification detailed here:
 * https://www.apollographql.com/docs/federation/metrics/#how-tracing-data-is-exposed-from-a-federated-service
 */
object ApolloFederatedTracing {

  /**
   * @param excludePureFields Optionally disable tracing of pure fields.
   *                          Setting this to true can help improve performance at the cost of generating incomplete traces.
   *                          WARNING: Use this with caution as it could potentially cause issues if the tracing client expects all queried fields to be included in the traces
   */
  def wrapper(excludePureFields: Boolean = false): EffectfulWrapper[Any] =
    EffectfulWrapper(
      for {
        tracing <- InternalUtils.newAtomicRef(Tracing(NodeTrie.empty))
        enabled <- InternalUtils.newAtomicRef(false)
        clock   <- ZIO.clock
      } yield apolloTracingOverall(clock, tracing, enabled) |+|
        Unsafe.unsafe(implicit u => apolloTracingField(clock.unsafe.nanoTime(), tracing, enabled, !excludePureFields))
    )

  private def toTimestamp(epochMilli: Long): Timestamp =
    Timestamp.of(
      epochMilli / 1000,
      (epochMilli % 1000).toInt * 1000000
    )

  private def apolloTracingOverall(
    clock: Clock,
    ref: AtomicReference[Tracing],
    enabled: AtomicReference[Boolean]
  ): OverallWrapper[Any] =
    new OverallWrapper[Any] {
      def wrap[R1](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          ZIO.ifZIO(
            ZIO.succeed(
              enabled.updateAndGet(_ =>
                request.extensions.exists(
                  _.get(GraphQLRequest.`apollo-federation-include-trace`).contains(StringValue(GraphQLRequest.ftv1))
                )
              )
            )
          )(
            for {
              startNano             <- clock.nanoTime
              _                     <- ZIO.succeed(ref.update(_.copy(startTime = startNano)))
              response              <- process(request).summarized(clock.currentTime(TimeUnit.MILLISECONDS))((_, _))
              ((start, end), result) = response
              endNano               <- clock.nanoTime
              tracing               <- ZIO.succeed(ref.get)
            } yield {
              val root = Trace(
                startTime = Some(toTimestamp(start)),
                endTime = Some(toTimestamp(end)),
                durationNs = endNano - startNano,
                root = Some(tracing.root.reduce())
              )

              result.copy(
                extensions = Some(
                  ObjectValue(
                    (
                      "ftv1" -> (StringValue(new String(Base64.getEncoder.encode(root.toByteArray))): ResponseValue)
                    ) :: result.extensions.fold(List.empty[(String, ResponseValue)])(_.fields)
                  )
                )
              )
            },
            process(request)
          )
    }

  private def apolloTracingField(
    nanoTime: => Long,
    ref: AtomicReference[Tracing],
    enabled: AtomicReference[Boolean],
    wrapPureValues: Boolean
  ): FieldWrapper[Any] =
    new FieldWrapper[Any](wrapPureValues) {
      def wrap[R1](
        query: ZQuery[R1, CalibanError.ExecutionError, ResponseValue],
        fieldInfo: FieldInfo
      ): ZQuery[R1, CalibanError.ExecutionError, ResponseValue] =
        if (enabled.get())
          ZQuery.suspend {
            val startTime = nanoTime
            query.either.flatMap { result =>
              ZQuery.fromEither {
                val endTime = nanoTime
                val path    = (PathValue.Key(fieldInfo.name) :: fieldInfo.path).toVector
                ref.update(state =>
                  state.copy(
                    root = state.root.insert(
                      path,
                      Node(
                        id = Node.Id.ResponseName(fieldInfo.name),
                        startTime = startTime - state.startTime,
                        endTime = endTime - state.startTime,
                        `type` = fieldInfo.details.fieldType.toType().toString,
                        parentType = fieldInfo.details.parentType.map(_.toType().toString) getOrElse "",
                        originalFieldName = fieldInfo.details.alias.map(_ => fieldInfo.details.name) getOrElse "",
                        error = result.left.toOption.collectFirst { case e: ExecutionError =>
                          Error(
                            e.getMessage(),
                            location = e.locationInfo.map(l => Location(l.line, l.column)).toSeq
                          )
                        }.toSeq
                      )
                    )
                  )
                )
                result
              }
            }
          }
        else query

    }

  private type VPath = Vector[PathValue]
  private final case class Tracing(root: NodeTrie, startTime: Long = 0)
  private final case class NodeTrie(node: Option[Node], children: Map[PathValue, NodeTrie]) {
    def insert(path: VPath, node: Node): NodeTrie = NodeTrie.loopInsert(this, path, node, path.length - 1)
    def reduce(initial: Node = Node()): Node      =
      node.getOrElse(initial).copy(child = children.values.map(_.reduce(Node())).toList)
  }

  private object NodeTrie {
    val empty: NodeTrie = NodeTrie(None, Map.empty[PathValue, NodeTrie])

    private def newEmptyNode(id: PathValue) =
      Node(id = id match {
        case StringValue(s)            => Node.Id.ResponseName(s)
        case IntValue.IntNumber(value) => Node.Id.Index(value)
      })

    private def loopInsert(trie: NodeTrie, path: VPath, value: Node, step: Int): NodeTrie =
      if (step == -1) {
        trie.copy(node = Some(value))
      } else {
        val index    = path(step)
        val nextItem = trie.children.getOrElse(index, NodeTrie.empty.copy(node = Some(newEmptyNode(index))))
        val newNode  = loopInsert(nextItem, path, value, step - 1)
        trie.copy(children = trie.children.updated(index, newNode))
      }
  }

}

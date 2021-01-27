package caliban.federation.tracing

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import caliban.wrappers.Wrapper.{ EffectfulWrapper, FieldWrapper, OverallWrapper }
import caliban.{ GraphQLRequest, ResponseValue }
import com.google.protobuf.timestamp.Timestamp
import mdg.engine.proto.reports.Trace
import mdg.engine.proto.reports.Trace.{ Error, Location, Node }
import zio.clock.Clock
import zio.query.ZQuery
import zio.{ clock, Ref, ZIO }

import java.util.Base64
import java.util.concurrent.TimeUnit

/**
 * Implements the federated tracing specification detailed here:
 * https://www.apollographql.com/docs/federation/metrics/#how-tracing-data-is-exposed-from-a-federated-service
 */
object ApolloFederatedTracing {

  val wrapper: EffectfulWrapper[Clock] =
    EffectfulWrapper(
      for {
        tracing <- Ref.make(Tracing(NodeTrie.empty))
        enabled <- Ref.make(false)
      } yield apolloTracingOverall(tracing, enabled) |+| apolloTracingField(tracing, enabled)
    )

  private def toTimestamp(epochMilli: Long): Timestamp =
    Timestamp.of(
      epochMilli / 1000,
      (epochMilli % 1000).toInt * 1000000
    )

  private def apolloTracingOverall(ref: Ref[Tracing], enabled: Ref[Boolean]): OverallWrapper[Clock] =
    OverallWrapper { process => (request: GraphQLRequest) =>
      ZIO.ifM(
        enabled.updateAndGet(_ =>
          request.extensions.exists(
            _.get(GraphQLRequest.`apollo-federation-include-trace`).contains(StringValue(GraphQLRequest.ftv1))
          )
        )
      )(
        for {
          startNano              <- clock.nanoTime
          _                      <- ref.update(_.copy(startTime = startNano))
          ((start, end), result) <- process(request).summarized(clock.currentTime(TimeUnit.MILLISECONDS))((_, _))
          endNano                <- clock.nanoTime
          tracing                <- ref.get
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

  private def apolloTracingField(ref: Ref[Tracing], enabled: Ref[Boolean]): FieldWrapper[Clock] =
    FieldWrapper(
      { case (query, fieldInfo) =>
        ZQuery
          .fromEffect(enabled.get)
          .flatMap(
            if (_)
              for {
                ((startTime, endTime), summarized) <- query.either.summarized(clock.nanoTime)((_, _))
                id                                  = Node.Id.ResponseName(fieldInfo.name)
                result                             <- ZQuery.fromEffect(
                                                        ref.update(state =>
                                                          state.copy(
                                                            root = state.root.insert(
                                                              (Left(fieldInfo.name) :: fieldInfo.path).toVector,
                                                              Node(
                                                                id = id,
                                                                startTime = startTime - state.startTime,
                                                                endTime = endTime - state.startTime,
                                                                `type` = fieldInfo.details.fieldType.toType().toString,
                                                                parentType = fieldInfo.details.parentType.map(_.toType().toString) getOrElse "",
                                                                originalFieldName = fieldInfo.details.alias
                                                                  .map(_ => fieldInfo.details.name) getOrElse "",
                                                                error = summarized.left.toOption.collectFirst { case e: ExecutionError =>
                                                                  Error(
                                                                    e.getMessage(),
                                                                    location = e.locationInfo.map(l => Location(l.line, l.column)).toSeq
                                                                  )
                                                                }.toSeq
                                                              )
                                                            )
                                                          )
                                                        ) *> ZIO.fromEither(summarized)
                                                      )
              } yield result
            else query
          )
      },
      wrapPureValues = true
    )

  private type VPath = Vector[Either[String, Int]]
  private final case class Tracing(root: NodeTrie, startTime: Long = 0)
  private final case class NodeTrie(node: Option[Node], children: Map[Either[String, Int], NodeTrie]) {
    def insert(path: VPath, node: Node): NodeTrie = NodeTrie.loopInsert(this, path, node, path.length - 1)
    def reduce(initial: Node = Node()): Node      =
      node.getOrElse(initial).copy(child = children.values.map(_.reduce(Node())).toList)
  }

  private object NodeTrie {
    val empty: NodeTrie = NodeTrie(None, Map.empty[Either[String, Int], NodeTrie])

    private def newEmptyNode(id: Either[String, Int]) =
      Node(id = id.fold(Node.Id.ResponseName, Node.Id.Index))

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

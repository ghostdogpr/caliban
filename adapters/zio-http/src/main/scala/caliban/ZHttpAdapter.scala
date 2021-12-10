package caliban

import caliban.Value.StringValue
import caliban.execution.QueryExecution
import caliban.interop.tapir.{ TapirAdapter, WebSocketHooks }
import caliban.interop.tapir.TapirAdapter._
import io.circe.parser._
import io.circe.syntax._
import sttp.tapir.json.circe._
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import zhttp.http._
import zhttp.socket.WebSocketFrame.Text
import zhttp.socket.{ SocketApp, _ }
import zio._
import zio.clock.Clock
import zio.duration._
import zio.stream._

object ZHttpAdapter {
  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): HttpApp[R, Throwable] = {
    val endpoints = TapirAdapter.makeHttpService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution
    )
    ZioHttpInterpreter().toHttp(endpoints)
  }

  def makeWebSocketService[R <: Has[_], E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty
  ): HttpApp[R with Clock, E] =
    Http.responseM(
      for {
        ref <- Ref.make(Map.empty[String, Promise[Any, Unit]])
      } yield Response.socket(
        socketHandler[R, E](
          ref,
          interpreter,
          skipValidation,
          enableIntrospection,
          keepAliveTime,
          queryExecution,
          webSocketHooks
        )
      )
    )

  private def socketHandler[R <: Has[_], E](
    subscriptions: Subscriptions,
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    keepAliveTime: Option[Duration],
    queryExecution: QueryExecution,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty
  ): SocketApp[R with Clock, E] = {
    val routes = Socket.collect[WebSocketFrame] { case Text(text) =>
      ZStream
        .fromEffect(ZIO.fromEither(decode[GraphQLWSInput](text)))
        .collect {
          case GraphQLWSInput("connection_init", id, payload) =>
            val before = (webSocketHooks.beforeInit, payload) match {
              case (Some(beforeInit), Some(payload)) =>
                ZStream.fromEffect(beforeInit(payload)).drain.catchAll(toStreamError(id, _))
              case _                                 => Stream.empty
            }

            val response = connectionAck ++ keepAlive(keepAliveTime)

            val after = webSocketHooks.afterInit match {
              case Some(afterInit) => ZStream.fromEffect(afterInit).drain.catchAll(toStreamError(id, _))
              case _               => Stream.empty
            }

            before ++ ZStream.mergeAllUnbounded()(response, after)

          case GraphQLWSInput("connection_terminate", _, _) =>
            ZStream.fromEffect(ZIO.interrupt)
          case GraphQLWSInput("start", id, payload)         =>
            val request = payload.collect { case InputValue.ObjectValue(fields) =>
              val query         = fields.get("query").collect { case StringValue(v) => v }
              val operationName = fields.get("operationName").collect { case StringValue(v) => v }
              val variables     = fields.get("variables").collect { case InputValue.ObjectValue(v) => v }
              val extensions    = fields.get("extensions").collect { case InputValue.ObjectValue(v) => v }
              GraphQLRequest(query, operationName, variables, extensions)
            }
            request match {
              case Some(req) =>
                val stream = generateGraphQLResponse(
                  req,
                  id.getOrElse(""),
                  interpreter,
                  skipValidation,
                  enableIntrospection,
                  queryExecution,
                  subscriptions
                )

                webSocketHooks.onMessage.map(_.transform(stream)).getOrElse(stream).catchAll(toStreamError(id, _))

              case None => connectionError
            }
          case GraphQLWSInput("stop", id, _)                =>
            removeSubscription(id, subscriptions) *> ZStream.empty

        }
        .flatten
        .catchAll(_ => connectionError)
        .map(output => WebSocketFrame.Text(output.asJson.noSpaces))
    }

    SocketApp.message(routes) ++ SocketApp.protocol(protocol)
  }

  private val protocol = SocketProtocol.subProtocol("graphql-ws")
}

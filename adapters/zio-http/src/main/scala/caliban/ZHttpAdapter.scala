package caliban

import caliban.Value.StringValue
import caliban.execution.QueryExecution
import caliban.interop.tapir.{ RequestInterceptor, TapirAdapter, WebSocketHooks }
import caliban.interop.tapir.TapirAdapter._
import io.circe.parser._
import io.circe.syntax._
import sttp.tapir.json.circe._
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }
import zhttp.http._
import zhttp.socket.WebSocketFrame.Text
import zhttp.socket.{ SocketApp, _ }
import zio._
import zio.stream._

object ZHttpAdapter {
  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]): HttpApp[R, Throwable] = {
    val endpoints = TapirAdapter.makeHttpService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    ZioHttpInterpreter(serverOptions).toHttp(endpoints)
  }

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty
  ): HttpApp[R, E] =
    Http.responseZIO(
      for {
        ref <- Ref.make(Map.empty[String, Promise[Any, Unit]])
        app <- Response.fromSocketApp(
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
      } yield app
    )

  private def socketHandler[R, E](
    subscriptions: Subscriptions,
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    keepAliveTime: Option[Duration],
    queryExecution: QueryExecution,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty
  ): SocketApp[R] = {
    val routes = Socket.collect[WebSocketFrame] { case Text(text) =>
      ZStream
        .fromZIO(ZIO.fromEither(decode[GraphQLWSInput](text)))
        .collect {
          case GraphQLWSInput("connection_init", id, payload) =>
            val before = (webSocketHooks.beforeInit, payload) match {
              case (Some(beforeInit), Some(payload)) =>
                ZStream.fromZIO(beforeInit(payload)).drain.catchAll(toStreamError(id, _))
              case _                                 => ZStream.empty
            }

            val response = ZStream.succeed(connectionAck) ++ keepAlive(keepAliveTime)

            val after = webSocketHooks.afterInit match {
              case Some(afterInit) => ZStream.fromZIO(afterInit).drain.catchAll(toStreamError(id, _))
              case _               => ZStream.empty
            }

            before ++ ZStream.mergeAllUnbounded()(response, after)

          case GraphQLWSInput("connection_terminate", _, _) =>
            ZStream.fromZIO(ZIO.interrupt)
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

              case None => ZStream.succeed(connectionError)
            }
          case GraphQLWSInput("stop", id, _)                =>
            ZStream.fromZIO(removeSubscription(id, subscriptions)) *> ZStream.empty

        }
        .flatten
        .catchAll(_ => ZStream.succeed(connectionError))
        .map(output => WebSocketFrame.Text(output.asJson.noSpaces))
    }

    SocketApp(routes).withProtocol(protocol)
  }

  private val protocol = SocketProtocol.subProtocol("graphql-ws")
}

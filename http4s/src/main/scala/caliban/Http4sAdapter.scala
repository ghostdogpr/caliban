package caliban

import caliban.parsing.adt.Value
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import fs2.{ Pipe, Stream }
import io.circe.derivation.deriveDecoder
import io.circe.parser.{ decode, parse }
import io.circe.{ Decoder, Json }
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import zio.interop.catz._
import zio.{ Fiber, IO, RIO, Ref, Runtime, Task }

object Http4sAdapter {

  case class GraphQLRequest(query: String, operationName: Option[String], variables: Option[Json] = None)

  implicit val queryDecoder: Decoder[GraphQLRequest] = deriveDecoder[GraphQLRequest]

  private def jsonToValue(json: Json): Value =
    json.fold(
      Value.NullValue,
      Value.BooleanValue,
      number => number.toInt.map(Value.IntValue) getOrElse Value.FloatValue(number.toFloat),
      Value.StringValue,
      array => Value.ListValue(array.toList.map(jsonToValue)),
      obj => Value.ObjectValue(obj.toMap.map { case (k, v) => k -> jsonToValue(v) })
    )

  private def jsonToVariables(json: Json): Map[String, Value] = jsonToValue(json) match {
    case Value.ObjectValue(fields) => fields
    case _                         => Map()
  }

  private def execute[Q, M, S](interpreter: GraphQL[Q, M, S], query: GraphQLRequest): IO[CalibanError, ResponseValue] =
    interpreter.execute(query.query, query.operationName, query.variables.map(jsonToVariables).getOrElse(Map()))

  def makeRestService[R, Q, M, S](
    interpreter: GraphQL[Q, M, S]
  )(implicit runtime: Runtime[R]): HttpRoutes[RIO[R, ?]] = {
    object dsl extends Http4sDsl[RIO[R, ?]]
    import dsl._

    HttpRoutes.of[RIO[R, ?]] {
      case req @ POST -> Root =>
        for {
          query <- req.attemptAs[GraphQLRequest].value.absolve
          result <- execute(interpreter, query)
                     .fold(
                       err => s"""{"errors":["${err.toString.replace("\"", "'")}"]}""",
                       result => s"""{"data":$result}"""
                     )
          json     <- Task.fromEither(parse(result))
          response <- Ok(json)
        } yield response
    }
  }

  def makeWebSocketService[R, Q, M, S](
    interpreter: GraphQL[Q, M, S]
  )(implicit runtime: Runtime[R]): HttpRoutes[RIO[R, ?]] = {

    object dsl extends Http4sDsl[RIO[R, ?]]
    import dsl._

    def sendMessage(
      sendQueue: fs2.concurrent.Queue[RIO[R, ?], WebSocketFrame],
      id: String,
      data: String
    ): RIO[R, Unit] =
      sendQueue.enqueue1(WebSocketFrame.Text(s"""{"id":"$id","type":"data","payload":{"data":$data}}"""))

    def processMessage(
      sendQueue: fs2.concurrent.Queue[RIO[R, ?], WebSocketFrame],
      subscriptions: Ref[Map[String, Fiber[Throwable, Unit]]]
    ): Pipe[RIO[R, ?], WebSocketFrame, Unit] =
      _.collect { case Text(text, _) => text }.flatMap { text =>
        Stream.eval {
          for {
            msg     <- Task.fromEither(decode[Json](text))
            msgType = msg.hcursor.downField("type").success.flatMap(_.value.asString).getOrElse("")
            _ <- IO.whenCase(msgType) {
                  case "connection_init"      => sendQueue.enqueue1(WebSocketFrame.Text("""{"type":"connection_ack"}"""))
                  case "connection_terminate" => Task.fromEither(WebSocketFrame.Close(1000)) >>= sendQueue.enqueue1
                  case "start" =>
                    val payload = msg.hcursor.downField("payload")
                    val id      = msg.hcursor.downField("id").success.flatMap(_.value.asString).getOrElse("")
                    Task.whenCase(payload.downField("query").success.flatMap(_.value.asString)) {
                      case Some(query) =>
                        val operationName = payload.downField("operationName").success.flatMap(_.value.asString)
                        (for {
                          result <- execute(interpreter, GraphQLRequest(query, operationName))
                          _ <- result match {
                                case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                                  stream.foreach { item =>
                                    sendMessage(sendQueue, id, ObjectValue(List(fieldName -> item)).toString)
                                  }.fork.flatMap(fiber => subscriptions.update(_.updated(id, fiber)))
                                case other =>
                                  sendMessage(sendQueue, id, other.toString) *> sendQueue.enqueue1(
                                    WebSocketFrame.Text(s"""{"type":"complete","id":"$id"}""")
                                  )
                              }
                        } yield ()).catchAll(
                          error =>
                            sendQueue.enqueue1(
                              WebSocketFrame.Text(s"""{"type":"complete","id":"$id","payload":"${error.toString}"}""")
                            )
                        )
                    }
                  case "stop" =>
                    val id = msg.hcursor.downField("id").success.flatMap(_.value.asString).getOrElse("")
                    subscriptions.get.flatMap(map => IO.whenCase(map.get(id)) { case Some(fiber) => fiber.interrupt })
                }
          } yield ()
        }
      }

    HttpRoutes.of[RIO[R, ?]] {
      case GET -> Root =>
        for {
          sendQueue     <- fs2.concurrent.Queue.unbounded[RIO[R, ?], WebSocketFrame]
          subscriptions <- Ref.make(Map.empty[String, Fiber[Throwable, Unit]])
          builder <- WebSocketBuilder[RIO[R, ?]].build(
                      sendQueue.dequeue,
                      processMessage(sendQueue, subscriptions),
                      headers = Headers.of(Header("Sec-WebSocket-Protocol", "graphql-ws")),
                      onClose = subscriptions.get.flatMap(m => IO.foreach(m.values)(_.interrupt).unit)
                    )
        } yield builder
    }
  }
}

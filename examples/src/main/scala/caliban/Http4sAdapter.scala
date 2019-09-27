package caliban

import caliban.schema.ResponseValue
import caliban.schema.ResponseValue.{ ObjectValue, StreamValue }
import fs2.{ Pipe, Stream }
import io.circe.magnolia.derivation.decoder.semiauto._
import io.circe.parser.{ decode, parse }
import io.circe.{ Decoder, Json }
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.{ CORS, Logger }
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.server.{ Router, Server }
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.{ Fiber, IO, Managed, Ref, Runtime, Task }

object Http4sAdapter {

  case class GraphQLRequest(query: String, operationName: Option[String])

  implicit val queryDecoder: Decoder[GraphQLRequest] = deriveMagnoliaDecoder[GraphQLRequest]

  def make[Q, M, S](interpreter: GraphQL[Q, M, S])(implicit runtime: Runtime[Any]): Managed[Throwable, Server[Task]] = {

    object dsl extends Http4sDsl[Task]
    import dsl._

    def execute(query: GraphQLRequest): IO[CalibanError, ResponseValue] =
      interpreter.execute(query.query, query.operationName)

    val restService: HttpRoutes[Task] = HttpRoutes.of[Task] {
      case req @ POST -> Root / "graphql" =>
        for {
          query    <- req.attemptAs[GraphQLRequest].value.absolve
          result   <- execute(query).fold(err => s"""{"errors":["${err.toString}"]}""", result => s"""{"data":$result}""")
          json     <- Task.fromEither(parse(result))
          response <- Ok(json)
        } yield response
    }

    def sendMessage(sendQueue: fs2.concurrent.Queue[Task, WebSocketFrame], id: String, data: String) =
      sendQueue.enqueue1(WebSocketFrame.Text(s"""{"id":"$id","type":"data","payload":{"data":$data}}"""))

    def processMessage(
      sendQueue: fs2.concurrent.Queue[Task, WebSocketFrame],
      subscriptions: Ref[Map[String, Fiber[Throwable, Unit]]]
    ): Pipe[Task, WebSocketFrame, Unit] =
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
                          result <- execute(GraphQLRequest(query, operationName))
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

    val wsService: HttpRoutes[Task] = HttpRoutes.of[Task] {
      case GET -> Root / "graphql" =>
        for {
          sendQueue     <- fs2.concurrent.Queue.unbounded[Task, WebSocketFrame]
          subscriptions <- Ref.make(Map.empty[String, Fiber[Throwable, Unit]])
          builder <- WebSocketBuilder[Task].build(
                      sendQueue.dequeue,
                      processMessage(sendQueue, subscriptions),
                      headers = Headers.of(Header("Sec-WebSocket-Protocol", "graphql-ws")),
                      onClose = subscriptions.get.flatMap(m => IO.foreach(m.values)(_.interrupt).unit)
                    )
        } yield builder
    }

    val httpApp: HttpApp[Task] = Router(
      "/api" -> Logger.httpRoutes(logHeaders = true, logBody = false)(CORS(restService)),
      "/ws"  -> Logger.httpRoutes(logHeaders = true, logBody = false)(CORS(wsService))
    ).orNotFound

    BlazeServerBuilder[Task].bindHttp(8088, "localhost").withHttpApp(httpApp).resource.toManaged[Any]
  }
}

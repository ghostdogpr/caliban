package caliban

import caliban.ResponseValue._
import caliban.Value._
import cats.data.OptionT
import cats.effect.Effect
import cats.effect.syntax.all._
import cats.~>
import fs2.{ Pipe, Stream }
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import zio._
import zio.interop.catz._

object Http4sAdapter {

  private def execute[R, Q, M, S, E](
    interpreter: GraphQL[R, Q, M, S, E],
    query: GraphQLRequest
  ): URIO[R, GraphQLResponse[E]] =
    interpreter.execute(query.query, query.operationName, query.variables.getOrElse(Map()))

  def makeRestService[R, Q, M, S, E](interpreter: GraphQL[R, Q, M, S, E]): HttpRoutes[RIO[R, *]] = {
    object dsl extends Http4sDsl[RIO[R, *]]
    import dsl._

    HttpRoutes.of[RIO[R, *]] {
      case req @ POST -> Root =>
        for {
          query <- req.attemptAs[GraphQLRequest].value.absolve
          result <- execute(interpreter, query)
                     .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
          response <- Ok(result)
        } yield response
    }
  }

  def makeWebSocketService[R, Q, M, S, E](interpreter: GraphQL[R, Q, M, S, E]): HttpRoutes[RIO[R, *]] = {

    object dsl extends Http4sDsl[RIO[R, *]]
    import dsl._

    def sendMessage(
      sendQueue: fs2.concurrent.Queue[RIO[R, *], WebSocketFrame],
      id: String,
      data: ResponseValue,
      errors: List[E]
    ): RIO[R, Unit] =
      sendQueue.enqueue1(
        WebSocketFrame.Text(
          Json
            .obj(
              "id"      -> Json.fromString(id),
              "type"    -> Json.fromString("data"),
              "payload" -> GraphQLResponse(data, errors).asJson
            )
            .noSpaces
        )
      )

    def processMessage(
      sendQueue: fs2.concurrent.Queue[RIO[R, *], WebSocketFrame],
      subscriptions: Ref[Map[String, Fiber[Throwable, Unit]]]
    ): Pipe[RIO[R, *], WebSocketFrame, Unit] =
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
                          result <- execute(interpreter, GraphQLRequest(query, operationName, None))
                          _ <- result.data match {
                                case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                                  stream.foreach { item =>
                                    sendMessage(sendQueue, id, ObjectValue(List(fieldName -> item)), result.errors)
                                  }.fork.flatMap(fiber => subscriptions.update(_.updated(id, fiber)))
                                case other =>
                                  sendMessage(sendQueue, id, other, result.errors) *> sendQueue.enqueue1(
                                    WebSocketFrame.Text(s"""{"type":"complete","id":"$id"}""")
                                  )
                              }
                        } yield ()).catchAll(
                          error =>
                            sendQueue.enqueue1(
                              WebSocketFrame.Text(
                                Json
                                  .obj(
                                    "id"      -> Json.fromString(id),
                                    "type"    -> Json.fromString("complete"),
                                    "payload" -> Json.fromString(error.toString)
                                  )
                                  .noSpaces
                              )
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

    HttpRoutes.of[RIO[R, *]] {
      case GET -> Root =>
        for {
          sendQueue     <- fs2.concurrent.Queue.unbounded[RIO[R, *], WebSocketFrame]
          subscriptions <- Ref.make(Map.empty[String, Fiber[Throwable, Unit]])
          builder <- WebSocketBuilder[RIO[R, *]].build(
                      sendQueue.dequeue,
                      processMessage(sendQueue, subscriptions),
                      headers = Headers.of(Header("Sec-WebSocket-Protocol", "graphql-ws")),
                      onClose = subscriptions.get.flatMap(m => IO.foreach(m.values)(_.interrupt).unit)
                    )
        } yield builder
    }
  }

  private def wrapRoute[F[_]: Effect](route: HttpRoutes[Task])(implicit runtime: Runtime[Any]): HttpRoutes[F] = {
    val toF: Task ~> F    = λ[Task ~> F](_.toIO.to[F])
    val toTask: F ~> Task = λ[F ~> Task](_.toIO.to[Task])

    route
      .mapK(λ[OptionT[Task, *] ~> OptionT[F, *]](_.mapK(toF)))
      .dimap((req: Request[F]) => req.mapK(toTask))((res: Response[Task]) => res.mapK(toF))
  }

  def makeWebSocketServiceF[F[_], Q, M, S, E](
    interpreter: GraphQL[Any, Q, M, S, E]
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpRoutes[F] =
    wrapRoute(makeWebSocketService[Any, Q, M, S, E](interpreter))

  def makeRestServiceF[F[_], Q, M, S, E](
    interpreter: GraphQL[Any, Q, M, S, E]
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpRoutes[F] =
    wrapRoute(makeRestService[Any, Q, M, S, E](interpreter))

}

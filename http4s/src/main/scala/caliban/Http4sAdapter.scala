package caliban

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.parsing.adt.Value
import cats.data.OptionT
import cats.effect.Effect
import cats.effect.syntax.all._
import cats.~>
import fs2.{ Pipe, Stream }
import io.circe.derivation.deriveDecoder
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import zio.interop.catz._
import zio._

object Http4sAdapter {

  case class GraphQLRequest(query: String, operationName: Option[String], variables: Option[Json] = None)

  implicit val queryDecoder: Decoder[GraphQLRequest] = deriveDecoder[GraphQLRequest]

  private def responseToJson(responseValue: ResponseValue): Json =
    responseValue match {
      case ResponseValue.NullValue           => Json.Null
      case ResponseValue.IntValue(value)     => Json.fromLong(value)
      case ResponseValue.FloatValue(value)   => Json.fromDoubleOrString(value)
      case ResponseValue.StringValue(value)  => Json.fromString(value)
      case ResponseValue.BooleanValue(value) => Json.fromBoolean(value)
      case ResponseValue.EnumValue(value)    => Json.fromString(value)
      case ResponseValue.ListValue(values)   => Json.arr(values.map(responseToJson): _*)
      case ObjectValue(fields)               => Json.obj(fields.map { case (k, v) => k -> responseToJson(v) }: _*)
      case s: StreamValue                    => Json.fromString(s.toString)
    }

  implicit val resultEncoder: Encoder[ResponseValue] = (response: ResponseValue) =>
    Json.obj("data" -> responseToJson(response))
  implicit val errorEncoder: Encoder[CalibanError] = (err: CalibanError) =>
    Json.obj("errors" -> Json.arr(Json.fromString(err.toString)))

  private def jsonToValue(json: Json): Value =
    json.fold(
      Value.NullValue,
      Value.BooleanValue,
      number => number.toLong.map(Value.IntValue) getOrElse Value.FloatValue(number.toDouble),
      Value.StringValue,
      array => Value.ListValue(array.toList.map(jsonToValue)),
      obj => Value.ObjectValue(obj.toMap.map { case (k, v) => k -> jsonToValue(v) })
    )

  private def jsonToVariables(json: Json): Map[String, Value] = jsonToValue(json) match {
    case Value.ObjectValue(fields) => fields
    case _                         => Map()
  }

  private def execute[R, Q, M, S](
    interpreter: GraphQL[R, Q, M, S],
    query: GraphQLRequest
  ): ZIO[R, CalibanError, ResponseValue] =
    interpreter.execute(query.query, query.operationName, query.variables.map(jsonToVariables).getOrElse(Map()))

  def makeRestService[R, Q, M, S](interpreter: GraphQL[R, Q, M, S]): HttpRoutes[RIO[R, *]] = {
    object dsl extends Http4sDsl[RIO[R, *]]
    import dsl._

    HttpRoutes.of[RIO[R, *]] {
      case req @ POST -> Root =>
        for {
          query    <- req.attemptAs[GraphQLRequest].value.absolve
          result   <- execute(interpreter, query).fold(_.asJson, _.asJson)
          response <- Ok(result)
        } yield response
    }
  }

  def makeWebSocketService[R, Q, M, S](interpreter: GraphQL[R, Q, M, S]): HttpRoutes[RIO[R, *]] = {

    object dsl extends Http4sDsl[RIO[R, *]]
    import dsl._

    def sendMessage(
      sendQueue: fs2.concurrent.Queue[RIO[R, *], WebSocketFrame],
      id: String,
      data: ResponseValue
    ): RIO[R, Unit] =
      sendQueue.enqueue1(
        WebSocketFrame.Text(
          Json
            .obj(
              "id"      -> Json.fromString(id),
              "type"    -> Json.fromString("data"),
              "payload" -> Json.obj("data" -> responseToJson(data))
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
                          result <- execute(interpreter, GraphQLRequest(query, operationName))
                          _ <- result match {
                                case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                                  stream.foreach { item =>
                                    sendMessage(sendQueue, id, ObjectValue(List(fieldName -> item)))
                                  }.fork.flatMap(fiber => subscriptions.update(_.updated(id, fiber)))
                                case other =>
                                  sendMessage(sendQueue, id, other) *> sendQueue.enqueue1(
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

  def makeWebSocketServiceF[F[_], Q, M, S](
    interpreter: GraphQL[Any, Q, M, S]
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpRoutes[F] =
    wrapRoute(makeWebSocketService[Any, Q, M, S](interpreter))

  def makeRestServiceF[F[_], Q, M, S](
    interpreter: GraphQL[Any, Q, M, S]
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpRoutes[F] =
    wrapRoute(makeRestService[Any, Q, M, S](interpreter))

}

package caliban

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import cats.data.{ Kleisli, OptionT }
import cats.effect.Effect
import cats.effect.syntax.all._
import cats.~>
import fs2.{ Pipe, Stream }
import io.circe.Decoder.Result
import io.circe.Json
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
import com.github.ghik.silencer.silent

object Http4sAdapter {

  private def executeToJson[R, E](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest,
    skipValidation: Boolean
  ): URIO[R, Json] =
    interpreter
      .executeRequest(request, skipValidation)
      .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)

  @deprecated("Use makeHttpService instead", "0.4.0")
  def makeRestService[R, E](interpreter: GraphQLInterpreter[R, E]): HttpRoutes[RIO[R, *]] =
    makeHttpService(interpreter)

  private def getGraphQLRequest(
    query: String,
    op: Option[String],
    vars: Option[String],
    exts: Option[String]
  ): Result[GraphQLRequest] = {
    val variablesJs  = vars.flatMap(parse(_).toOption)
    val extensionsJs = exts.flatMap(parse(_).toOption)
    val fields = List("query" -> Json.fromString(query)) ++
      op.map(o => "operationName"         -> Json.fromString(o)) ++
      variablesJs.map(js => "variables"   -> js) ++
      extensionsJs.map(js => "extensions" -> js)
    Json
      .fromFields(fields)
      .as[GraphQLRequest]
  }

  private def getGraphQLRequest(params: Map[String, String]): Result[GraphQLRequest] =
    getGraphQLRequest(
      params.getOrElse("query", ""),
      params.get("operationName"),
      params.get("variables"),
      params.get("extensions")
    )

  @silent def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false
  ): HttpRoutes[RIO[R, *]] = {
    object dsl extends Http4sDsl[RIO[R, *]]
    import dsl._

    HttpRoutes.of[RIO[R, *]] {
      case req @ POST -> Root =>
        for {
          query    <- req.attemptAs[GraphQLRequest].value.absolve
          result   <- executeToJson(interpreter, query, skipValidation)
          response <- Ok(result)
        } yield response
      case req @ GET -> Root =>
        for {
          query    <- Task.fromEither(getGraphQLRequest(req.params))
          result   <- executeToJson(interpreter, query, skipValidation)
          response <- Ok(result)
        } yield response
    }
  }

  def executeRequest[R0, R, E](
    interpreter: GraphQLInterpreter[R, E],
    provideEnv: R0 => R,
    skipValidation: Boolean = false
  ): HttpApp[RIO[R0, *]] =
    Kleisli { req =>
      object dsl extends Http4sDsl[RIO[R0, *]]
      import dsl._
      for {
        query    <- req.attemptAs[GraphQLRequest].value.absolve
        result   <- executeToJson(interpreter, query, skipValidation).provideSome[R0](provideEnv)
        response <- Ok(result)
      } yield response
    }

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false
  ): HttpRoutes[RIO[R, *]] = {

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
                          result <- interpreter.execute(query, operationName, skipValidation = skipValidation)
                          _ <- result.data match {
                                case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                                  stream.foreach { item =>
                                    sendMessage(sendQueue, id, ObjectValue(List(fieldName -> item)), result.errors)
                                  }.forkDaemon.flatMap(fiber => subscriptions.update(_.updated(id, fiber)))
                                case other =>
                                  sendMessage(sendQueue, id, other, result.errors) *> sendQueue.enqueue1(
                                    WebSocketFrame.Text(s"""{"type":"complete","id":"$id"}""")
                                  )
                              }
                        } yield ()).catchAll(error =>
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
                    subscriptions
                      .modify(map => (map.get(id), map - id))
                      .flatMap(fiber => IO.whenCase(fiber) { case Some(fiber) => fiber.interrupt })
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

  private def wrapApp[F[_]: Effect](app: HttpApp[Task])(implicit runtime: Runtime[Any]): HttpApp[F] = {
    val toF: Task ~> F    = λ[Task ~> F](_.toIO.to[F])
    val toTask: F ~> Task = λ[F ~> Task](_.toIO.to[Task])

    app
      .mapK(toF)
      .dimap((req: Request[F]) => req.mapK(toTask))((res: Response[Task]) => res.mapK(toF))
  }

  def makeWebSocketServiceF[F[_], E](
    interpreter: GraphQLInterpreter[Any, E],
    skipValidation: Boolean = false
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpRoutes[F] =
    wrapRoute(makeWebSocketService[Any, E](interpreter, skipValidation))

  @deprecated("Use makeHttpServiceF instead", "0.4.0")
  def makeRestServiceF[F[_], E](
    interpreter: GraphQLInterpreter[Any, E]
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpRoutes[F] =
    makeHttpServiceF(interpreter)

  def makeHttpServiceF[F[_], E](
    interpreter: GraphQLInterpreter[Any, E],
    skipValidation: Boolean = false
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpRoutes[F] =
    wrapRoute(makeHttpService[Any, E](interpreter, skipValidation))

  def executeRequestF[F[_], E](
    interpreter: GraphQLInterpreter[Any, E],
    skipValidation: Boolean = false
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpApp[F] =
    wrapApp(executeRequest[Any, Any, E](interpreter, identity, skipValidation))
}

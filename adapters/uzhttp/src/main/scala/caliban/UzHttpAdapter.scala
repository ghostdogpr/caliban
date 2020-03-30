package caliban

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import io.circe.Json
import io.circe.parser._
import io.circe.syntax._
import uzhttp.HTTPError.BadRequest
import uzhttp.Request.Method
import uzhttp.Status.Ok
import uzhttp.websocket.{ Close, Frame, Text }
import uzhttp.{ HTTPError, Request, Response }
import zio.stream.{ Take, ZSink, ZStream }
import zio.{ Fiber, IO, Queue, Ref, Task, UIO, URIO, ZIO }

object UzHttpAdapter {

  def makeHttpService[R, E](
    path: String,
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false
  ): PartialFunction[Request, ZIO[R, HTTPError, Response]] = {

    // POST case
    case req if req.method == Method.POST && req.uri.getPath == path =>
      for {
        body <- req.body match {
                 case Some(value) => value.run(ZSink.utf8DecodeChunk)
                 case None        => ZIO.fail(BadRequest("Missing body"))
               }
        req <- ZIO.fromEither(decode[GraphQLRequest](body)).mapError(e => BadRequest(e.getMessage))
        res <- executeHttpResponse(interpreter, req, skipValidation)
      } yield res

    // GET case
    case req if req.method == Method.GET && req.uri.getPath == path =>
      val params = Option(req.uri.getQuery)
        .getOrElse("")
        .split("&")
        .toList
        .flatMap(_.split("=").toList match {
          case key :: value :: Nil => Some(key -> URLDecoder.decode(value, "UTF-8"))
          case _                   => None
        })
        .toMap

      for {
        variables <- ZIO
                      .foreach(params.get("variables"))(s => ZIO.fromEither(decode[Map[String, InputValue]](s)))
                      .mapError(e => BadRequest(e.getMessage))
        extensions <- ZIO
                       .foreach(params.get("extensions"))(s => ZIO.fromEither(decode[Map[String, InputValue]](s)))
                       .mapError(e => BadRequest(e.getMessage))
        req = GraphQLRequest(params.get("query"), params.get("operationName"), variables, extensions)
        res <- executeHttpResponse(interpreter, req, skipValidation)
      } yield res
  }

  def makeWebSocketService[R, E](
    path: String,
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false
  ): PartialFunction[Request, ZIO[R, HTTPError, Response]] = {
    case req @ Request.WebsocketRequest(_, uri, _, _, inputFrames) if uri.getPath == path =>
      for {
        subscriptions <- Ref.make(Map.empty[String, Fiber[Throwable, Unit]])
        sendQueue     <- Queue.unbounded[Take[Nothing, Frame]]
        _ <- inputFrames.collect { case Text(text, _) => text }.mapM { text =>
              for {
                msg     <- Task.fromEither(decode[Json](text))
                msgType = msg.hcursor.downField("type").success.flatMap(_.value.asString).getOrElse("")
                _ <- IO.whenCase(msgType) {
                      case "connection_init"      => sendQueue.offer(Take.Value(Text("""{"type":"connection_ack"}""")))
                      case "connection_terminate" => sendQueue.offerAll(List(Take.Value(Close), Take.End))
                      case "start" =>
                        val payload = msg.hcursor.downField("payload")
                        val id      = msg.hcursor.downField("id").success.flatMap(_.value.asString).getOrElse("")
                        Task.whenCase(payload.downField("query").success.flatMap(_.value.asString)) {
                          case Some(query) =>
                            val operationName = payload.downField("operationName").success.flatMap(_.value.asString)
                            for {
                              result <- interpreter.executeRequest(
                                         GraphQLRequest(Some(query), operationName),
                                         skipValidation
                                       )
                              _ <- result.data match {
                                    case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                                      stream.foreach { item =>
                                        sendMessage(
                                          sendQueue,
                                          id,
                                          ObjectValue(List(fieldName -> item)),
                                          result.errors
                                        )
                                      }.forkDaemon.flatMap(fiber => subscriptions.update(_.updated(id, fiber)))
                                    case other =>
                                      sendMessage(sendQueue, id, other, result.errors) *> sendQueue.offer(
                                        Take.Value(Text(s"""{"type":"complete","id":"$id"}"""))
                                      )
                                  }
                            } yield ()
                        }
                      case "stop" =>
                        val id = msg.hcursor.downField("id").success.flatMap(_.value.asString).getOrElse("")
                        subscriptions
                          .modify(map => (map.get(id), map - id))
                          .flatMap(fiber => IO.whenCase(fiber) { case Some(fiber) => fiber.interrupt })
                    }
              } yield ()
            }.runDrain
              .mapError(e => BadRequest(e.getMessage))
              .forkDaemon
        ws <- Response
               .websocket(req, ZStream.fromQueue(sendQueue).unTake)
               .map(_.addHeaders("Sec-WebSocket-Protocol" -> "graphql-ws"))
      } yield ws
  }

  private def sendMessage[E](
    sendQueue: Queue[Take[Nothing, Frame]],
    id: String,
    data: ResponseValue,
    errors: List[E]
  ): UIO[Unit] =
    sendQueue
      .offer(
        Take.Value(
          Text(
            Json
              .obj(
                "id"      -> Json.fromString(id),
                "type"    -> Json.fromString("data"),
                "payload" -> GraphQLResponse(data, errors).asJson
              )
              .noSpaces
          )
        )
      )
      .unit

  private def executeHttpResponse[R, E](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest,
    skipValidation: Boolean
  ): URIO[R, Response] =
    interpreter
      .executeRequest(request, skipValidation)
      .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
      .map(gqlResult =>
        Response.const(
          gqlResult.noSpaces.getBytes(StandardCharsets.UTF_8),
          Ok,
          contentType = s"application/json; charset=${StandardCharsets.UTF_8.name()}"
        )
      )

}

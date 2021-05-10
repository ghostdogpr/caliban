package caliban

import zio._
import zio.clock.Clock
import zio.duration._
import zio.stream._

import caliban.{ GraphQLInterpreter, GraphQLRequest, GraphQLResponse }
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.ResponseValue
import caliban.Value.NullValue
import caliban.execution.QueryExecution
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import zhttp.http._
import zhttp.socket.SocketApp
import zhttp.socket.WebSocketFrame.Text
import zhttp.socket._

object ZHttpAdapter {
  case class GraphQLWSRequest(`type`: String, id: Option[String], payload: Option[GraphQLRequest])
  object GraphQLWSRequest {
    import io.circe._
    import io.circe.generic.semiauto._

    implicit val decodeGraphQLWSRequest: Decoder[GraphQLWSRequest] = deriveDecoder[GraphQLWSRequest]
  }

  type Subscriptions = Ref[Map[String, Promise[Any, Unit]]]

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): HttpApp[R, HttpError] =
    Http.collectM {
      case req @ Method.POST -> _ =>
        (for {
          query           <- ZIO.fromEither(decode[GraphQLRequest](req.getBodyAsString.getOrElse("")))
          queryWithTracing = req.headers
                               .find(r => r.name == "apollo-federation-include-trace" && r.value == "ftv1")
                               .foldLeft(query)((q, _) => q.withFederatedTracing)
          resp            <- executeToJson(interpreter, queryWithTracing, skipValidation, enableIntrospection, queryExecution)
        } yield Response.jsonString(resp.toString)).handleHTTPError
      case req @ Method.GET -> _  =>
        val params = List("query", "operationName", "variables", "extensions")
          .collect({ case k =>
            k -> req.url.queryParams.get(k).flatMap(_.headOption).getOrElse("")
          })
          .toMap

        (for {
          query <- ZIO.fromEither(decode[GraphQLRequest](params.asJson.noSpaces))
          resp  <- executeToJson(interpreter, query, skipValidation, enableIntrospection, queryExecution)
        } yield Response.jsonString(resp.toString())).handleHTTPError

      case _ @Method.OPTIONS -> _ =>
        ZIO.succeed(Response.http(Status.NO_CONTENT))
    }

  def makeWebSocketService[R <: Has[_], E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ) =
    HttpApp.responseM(
      for {
        ref <- Ref.make(Map.empty[String, Promise[Any, Unit]])
      } yield Response.socket(
        socketHandler(
          ref,
          interpreter,
          skipValidation,
          enableIntrospection,
          keepAliveTime,
          queryExecution
        )
      )
    )

  private def socketHandler[R <: Has[_], E](
    subscriptions: Subscriptions,
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    keepAliveTime: Option[Duration],
    queryExecution: QueryExecution
  ): SocketApp[R with Clock, E] = {
    val routes = Socket.collect[WebSocketFrame] { case Text(text) =>
      ZStream
        .fromEffect(ZIO.fromEither(decode[GraphQLWSRequest](text)))
        .collect({
          case GraphQLWSRequest("connection_init", _, _)      => connectionAck ++ keepAlive(keepAliveTime)
          case GraphQLWSRequest("connection_terminate", _, _) => close
          case GraphQLWSRequest("start", id, payload)         =>
            payload match {
              case Some(req) =>
                generateGraphQLResponse(
                  req,
                  id.getOrElse(""),
                  interpreter,
                  skipValidation,
                  enableIntrospection,
                  queryExecution,
                  subscriptions
                ).catchAll(toStreamError(id, _))
              case None      => connectionError
            }
          case GraphQLWSRequest("stop", id, _)                =>
            removeSubscription(id, subscriptions)
              .flatMap(_ => ZStream.empty)

        })
        .flatten
        .catchAll(_ => connectionError)
    }

    SocketApp.message(routes) ++ SocketApp.protocol(protocol)
  }

  private def generateGraphQLResponse[R, E](
    payload: GraphQLRequest,
    id: String,
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    queryExecution: QueryExecution,
    subscriptions: Subscriptions
  ): ZStream[R, E, Text] = {
    val resp = ZStream
      .fromEffect(
        interpreter
          .executeRequest(payload, skipValidation, enableIntrospection, queryExecution)
      )
      .flatMap(res =>
        res.data match {
          case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
            trackSubscription(id, subscriptions).flatMap { p =>
              stream.map(toResponse(id, fieldName, _, res.errors)).interruptWhen(p)
            }
          case other                                                =>
            ZStream.succeed(toResponse(id, GraphQLResponse(other, res.errors)))
        }
      )

    (resp ++ complete(id)).catchAll { _ =>
      connectionError
    }
  }

  private def executeToJson[R, E](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest,
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    queryExecution: QueryExecution
  ): URIO[R, Json] =
    interpreter
      .executeRequest(
        request,
        skipValidation = skipValidation,
        enableIntrospection = enableIntrospection,
        queryExecution
      )
      .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)

  implicit class HttpErrorOps[R, E <: Throwable, A](private val zio: ZIO[R, io.circe.Error, A]) extends AnyVal {
    def handleHTTPError: ZIO[R, HttpError, A] = zio.mapError({
      case DecodingFailure(error, _)  =>
        HttpError.BadRequest.apply(s"Invalid json: $error")
      case ParsingFailure(message, _) =>
        HttpError.BadRequest.apply(message)
      case t: Throwable               => HttpError.InternalServerError.apply("Internal Server Error", Some(t.getCause()))
    })
  }

  private val protocol = SocketProtocol.subProtocol("graphql-ws")

  private val keepAlive = (keepAlive: Option[Duration]) =>
    keepAlive match {
      case None           => ZStream.empty
      case Some(duration) =>
        ZStream
          .succeed(Text("""{"type":"ka"}"""))
          .repeat(Schedule.spaced(duration))
    }

  private val connectionError                            = ZStream.succeed(Text("""{"type":"connection_error"}"""))
  private val connectionAck                              = ZStream.succeed(Text("""{"type":"connection_ack"}"""))
  private val close                                      = ZStream.succeed(WebSocketFrame.close(1000))
  private def toStreamError[E](id: Option[String], e: E) = ZStream.succeed(
    Text(
      Json
        .obj(
          "id"      -> Json.fromString(id.getOrElse("")),
          "type"    -> Json.fromString("error"),
          "message" -> Json.fromString(e.toString())
        )
        .toString()
    )
  )

  private def complete(id: String) = ZStream.succeed(WebSocketFrame.Text(s"""{"type":"complete","id":"$id"}"""))

  private def toResponse[E](id: String, fieldName: String, r: ResponseValue, errors: List[E]): WebSocketFrame.Text =
    toResponse(
      id,
      GraphQLResponse(
        ObjectValue(List(fieldName -> r)),
        errors
      )
    )

  private def toResponse[E](id: String, r: GraphQLResponse[E]) = Text(
    Json
      .obj("id" -> Json.fromString(id), "type" -> Json.fromString("data"), "payload" -> r.asJson)
      .toString()
  )

  private def trackSubscription(id: String, subs: Subscriptions) =
    ZStream
      .fromEffect(for {
        p <- Promise.make[Any, Unit]
        _ <- subs.update(m => m.updated(id, p))
      } yield p)

  private def removeSubscription(id: Option[String], subs: Subscriptions) =
    ZStream
      .fromEffect(
        id.map(id =>
          subs
            .modify(map => (map.get(id), map - id))
            .flatMap { p =>
              IO.whenCase(p) { case Some(p) =>
                p.succeed(())
              }
            }
        ).getOrElse(ZIO.unit)
      )
}

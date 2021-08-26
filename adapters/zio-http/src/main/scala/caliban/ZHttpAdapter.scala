package caliban

import zio._
import zio.clock.Clock
import zio.duration._
import zio.stream._

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import caliban.execution.QueryExecution
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import zhttp.http._
import zhttp.socket.SocketApp
import zhttp.socket.WebSocketFrame.Text
import zhttp.socket._
import io.netty.handler.codec.http.HttpHeaderNames

object ZHttpAdapter {
  case class GraphQLWSRequest(`type`: String, id: Option[String], payload: Option[Json])
  object GraphQLWSRequest {
    import io.circe._
    import io.circe.generic.semiauto._

    implicit val decodeGraphQLWSRequest: Decoder[GraphQLWSRequest] = deriveDecoder[GraphQLWSRequest]
  }

  case class Callbacks[R, E](
    onInit: Option[io.circe.Json => ZIO[R, E, Any]] = None,
    onMessage: Option[ZStream[R, E, Text] => ZStream[R, E, Text]] = None
  ) { self =>
    def ++(other: Callbacks[R, E]): Callbacks[R, E] =
      Callbacks(
        onInit = (self.onInit, other.onInit) match {
          case (None, Some(f))      => Some(f)
          case (Some(f), None)      => Some(f)
          case (Some(f1), Some(f2)) => Some((x: io.circe.Json) => f1(x) *> f2(x))
          case _                    => None
        },
        onMessage = (self.onMessage, other.onMessage) match {
          case (None, Some(f))      => Some(f)
          case (Some(f), None)      => Some(f)
          case (Some(f1), Some(f2)) => Some(f1 andThen f2)
          case _                    => None
        }
      )
  }

  object Callbacks {
    def empty[R, E] = Callbacks[R, E](None, None)

    def init[R, E](f: io.circe.Json => ZIO[R, E, Any]): Callbacks[R, E]               = Callbacks(Some(f), None)
    def message[R, E](f: ZStream[R, E, Text] => ZStream[R, E, Text]): Callbacks[R, E] = Callbacks(None, Some(f))
  }

  type Subscriptions = Ref[Map[String, Promise[Any, Unit]]]

  private val contentTypeApplicationGraphQL: Header =
    Header.custom(HttpHeaderNames.CONTENT_TYPE.toString(), "application/graphql")

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): HttpApp[R, HttpError] =
    Http.collectM {
      case req @ Method.POST -> _ =>
        (for {
          query           <- queryFromRequest(req)
          queryWithTracing =
            req.headers
              .find(r => r.name == GraphQLRequest.`apollo-federation-include-trace` && r.value == GraphQLRequest.ftv1)
              .foldLeft(query)((q, _) => q.withFederatedTracing)
          resp            <- executeToJson(interpreter, queryWithTracing, skipValidation, enableIntrospection, queryExecution)
        } yield Response.jsonString(resp.toString)).handleHTTPError
      case req @ Method.GET -> _  =>
        (for {
          query <- queryFromQueryParams(req)
          resp  <- executeToJson(interpreter, query, skipValidation, enableIntrospection, queryExecution)
        } yield Response.jsonString(resp.toString())).handleHTTPError

      case _ @Method.OPTIONS -> _ =>
        ZIO.succeed(Response.http(Status.NO_CONTENT))
    }

  def makeWebSocketHandler[R <: Has[_], E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    callbacks: Callbacks[R, E] = Callbacks.empty
  ) = for {
    ref <- Ref.make(Map.empty[String, Promise[Any, Unit]])
  } yield socketHandler(
    ref,
    interpreter,
    skipValidation,
    enableIntrospection,
    keepAliveTime,
    queryExecution,
    callbacks
  )

  def makeWebSocketService[R <: Has[_], E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): HttpApp[R with Clock, E] =
    HttpApp.responseM(
      for {
        ref <- Ref.make(Map.empty[String, Promise[Any, Unit]])
      } yield Response.socket(
        socketHandler[R, E](
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
    queryExecution: QueryExecution,
    callbacks: Callbacks[R, E] = Callbacks.empty[R, E]
  ): SocketApp[R with Clock, E] = {
    val routes = Socket.collect[WebSocketFrame] { case Text(text) =>
      ZStream
        .fromEffect(ZIO.fromEither(decode[GraphQLWSRequest](text)))
        .collect({
          case GraphQLWSRequest("connection_init", id, payload) =>
            val response = connectionAck ++ keepAlive(keepAliveTime)
            val callback = (callbacks.onInit, payload) match {
              case (Some(onInit), Some(payload)) =>
                ZStream.fromEffect(onInit(payload)).drain.catchAll(toStreamError(id, _))
              case _                             => Stream.empty
            }
            callback ++ response

          case GraphQLWSRequest("connection_terminate", _, _) => close
          case GraphQLWSRequest("start", id, payload)         =>
            val request = payload.flatMap(_.as[GraphQLRequest].toOption)
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

                callbacks.onMessage.map(_(stream)).getOrElse(stream).catchAll(toStreamError(id, _))

              case None => connectionError
            }
          case GraphQLWSRequest("stop", id, _)                =>
            removeSubscription(id, subscriptions) *> ZStream.empty

        })
        .flatten
        .catchAll(_ => connectionError)
    }

    SocketApp.message(routes) ++ SocketApp.protocol(protocol)
  }

  private def queryFromQueryParams(req: Request) = {
    val params = List("query", "operationName", "variables", "extensions")
      .collect({ case k =>
        k -> req.url.queryParams.get(k).flatMap(_.headOption).getOrElse("")
      })
      .toMap
    ZIO.fromEither(decode[GraphQLRequest](params.asJson.noSpaces))
  }

  private def queryFromRequest(req: Request) =
    if (req.url.queryParams.contains("query")) {
      queryFromQueryParams(req)
    } else if (req.headers.contains(contentTypeApplicationGraphQL)) {
      ZIO.succeed(GraphQLRequest(query = req.getBodyAsString))
    } else {
      ZIO.fromEither(decode[GraphQLRequest](req.getBodyAsString.getOrElse("")))
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

    (resp ++ complete(id)).catchAll(toStreamError(Option(id), _))
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
      case t: Throwable               => HttpError.InternalServerError.apply("Internal Server Error", Some(t.getCause))
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
          "message" -> Json.fromString(e.toString)
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
      .fromEffect(IO.whenCase(id) { case Some(id) =>
        subs.modify(map => (map.get(id), map - id)).flatMap { p =>
          IO.whenCase(p) { case Some(p) =>
            p.succeed(())
          }
        }
      })
}

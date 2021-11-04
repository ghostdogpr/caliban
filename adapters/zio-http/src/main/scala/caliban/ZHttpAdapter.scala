package caliban

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.execution.QueryExecution
import caliban.interop.tapir.TapirAdapter
import caliban.interop.tapir.TapirAdapter._
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import sttp.tapir.Schema
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
  case class GraphQLWSRequest(`type`: String, id: Option[String], payload: Option[Json])
  object GraphQLWSRequest {
    import io.circe._
    import io.circe.generic.semiauto._

    implicit val decodeGraphQLWSRequest: Decoder[GraphQLWSRequest] = deriveDecoder[GraphQLWSRequest]
  }

  case class Callbacks[R, E](
    beforeInit: Option[io.circe.Json => ZIO[R, E, Any]] = None,
    afterInit: Option[ZIO[R, E, Any]] = None,
    onMessage: Option[ZStream[R, E, Text] => ZStream[R, E, Text]] = None
  ) { self =>
    def ++(other: Callbacks[R, E]): Callbacks[R, E] =
      Callbacks(
        beforeInit = (self.beforeInit, other.beforeInit) match {
          case (None, Some(f))      => Some(f)
          case (Some(f), None)      => Some(f)
          case (Some(f1), Some(f2)) => Some((x: io.circe.Json) => f1(x) *> f2(x))
          case _                    => None
        },
        afterInit = (self.afterInit, other.afterInit) match {
          case (None, Some(f))      => Some(f)
          case (Some(f), None)      => Some(f)
          case (Some(f1), Some(f2)) => Some(f1 &> f2)
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
    def empty[R, E]: Callbacks[R, E] = Callbacks[R, E](None, None)

    /**
     * Specifies a callback that will be run before an incoming subscription
     * request is accepted. Useful for e.g authorizing the incoming subscription
     * before accepting it.
     */
    def init[R, E](f: io.circe.Json => ZIO[R, E, Any]): Callbacks[R, E] = Callbacks(Some(f), None, None)

    /**
     * Specifies a callback that will be run after an incoming subscription
     * request has been accepted. Useful for e.g terminating a subscription
     * after some time, such as authorization expiring.
     */
    def afterInit[R, E](f: ZIO[R, E, Any]): Callbacks[R, E] = Callbacks(None, Some(f), None)

    /**
     * Specifies a callback that will be run on the resulting `ZStream`
     * for every active subscription. Useful to e.g modify the environment
     * to inject session information into the `ZStream` handling the
     * subscription.
     */
    def message[R, E](f: ZStream[R, E, Text] => ZStream[R, E, Text]): Callbacks[R, E] = Callbacks(None, None, Some(f))
  }

  type Subscriptions = Ref[Map[String, Promise[Any, Unit]]]

  def makeHttpService[R, E: Schema](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): HttpApp[R, Throwable] = {
    val endpoints = TapirAdapter.makeHttpService[R, E](interpreter, skipValidation, enableIntrospection, queryExecution)
    ZioHttpInterpreter().toHttp(endpoints)
  }

  /**
   * Effectfully creates a `SocketApp`, which can be used from
   * a zio-http router via Http.fromEffectFunction or Http.fromResponseM.
   * This is a lower level API that allows for greater control than
   * `makeWebSocketService` so that it's possible to implement functionality such
   * as intercepting the initial request before the WebSocket upgrade,
   * handling authentication in the connection_init message, or shutdown
   * the websocket after some duration has passed (like a session expiring).
   */
  def makeWebSocketHandler[R <: Has[_], E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    callbacks: Callbacks[R, E] = Callbacks.empty
  ): ZIO[R, Nothing, SocketApp[R with Clock, E]] = for {
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

  /**
   * Creates an `HttpApp` that can handle GraphQL subscriptions.
   * This is a higher level API than `makeWebSocketHandler`. If you need
   * additional control over the websocket lifecycle please use
   * makeWebSocketHandler instead.
   */
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
        .collect {
          case GraphQLWSRequest("connection_init", id, payload) =>
            val before = (callbacks.beforeInit, payload) match {
              case (Some(beforeInit), Some(payload)) =>
                ZStream.fromEffect(beforeInit(payload)).drain.catchAll(toStreamError(id, _))
              case _                                 => Stream.empty
            }

            val response = connectionAck ++ keepAlive(keepAliveTime)

            val after = callbacks.afterInit match {
              case Some(afterInit) => ZStream.fromEffect(afterInit).drain.catchAll(toStreamError(id, _))
              case _               => Stream.empty
            }

            before ++ ZStream.mergeAllUnbounded()(response, after)

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

        }
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

    (resp ++ complete(id)).catchAll(toStreamError(Option(id), _))
  }

  implicit class HttpErrorOps[R, E <: Throwable, A](private val zio: ZIO[R, io.circe.Error, A]) extends AnyVal {
    def handleHTTPError: ZIO[R, HttpError, A] = zio.mapError {
      case DecodingFailure(error, _)  =>
        HttpError.BadRequest.apply(s"Invalid json: $error")
      case ParsingFailure(message, _) =>
        HttpError.BadRequest.apply(message)
      case t: Throwable               => HttpError.InternalServerError.apply("Internal Server Error", Some(t.getCause))
    }
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

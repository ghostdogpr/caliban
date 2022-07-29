package caliban.interop.tapir.ws

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.StringValue
import caliban._
import caliban.execution.QueryExecution
import caliban.interop.tapir.TapirAdapter.CalibanPipe
import caliban.interop.tapir.WebSocketHooks
import zio.stm.TMap
import zio.stream.{ UStream, ZStream }
import zio.{ Duration, Promise, Queue, Ref, Schedule, UIO, URIO, ZIO }

sealed trait Protocol {
  def name: String

  def make[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    keepAliveTime: Option[Duration],
    queryExecution: QueryExecution,
    webSocketHooks: WebSocketHooks[R, E]
  ): URIO[R, CalibanPipe]

}

object Protocol {

  def fromName(name: String): Protocol = name match {
    case GraphQLWS.name => GraphQLWS
    case _              => Legacy
  }

  object GraphQLWS extends Protocol {
    object Ops {
      final val Next           = "next"
      final val Error          = "error"
      final val Complete       = "complete"
      final val Pong           = "pong"
      final val Ping           = "ping"
      final val Subscribe      = "subscribe"
      final val ConnectionInit = "connection_init"
      final val ConnectionAck  = "connection_ack"
    }

    val name = "graphql-transport-ws"

    val handler = new ResponseHandler {
      override def toResponse[E](id: String, r: GraphQLResponse[E]): GraphQLWSOutput =
        GraphQLWSOutput(Ops.Next, Some(id), Some(r.toResponseValue))

      override def complete(id: String): GraphQLWSOutput =
        GraphQLWSOutput(Ops.Complete, Some(id), None)

      override def error[E](id: Option[String], e: E): GraphQLWSOutput =
        GraphQLWSOutput(
          Ops.Error,
          id,
          Some(ResponseValue.ListValue(List(e match {
            case e: CalibanError => e.toResponseValue
            case e               => StringValue(e.toString)
          })))
        )

      override def connectionAck(payload: Option[ResponseValue]): GraphQLWSOutput =
        GraphQLWSOutput(Ops.ConnectionAck, None, payload)
    }

    private def hookOrError[R, E](
      id: Option[String],
      hook: ZIO[R, E, Any]
    ): ZStream[R, Nothing, Either[Nothing, GraphQLWSOutput]] =
      ZStream.unwrap(
        hook.fold(
          e => ZStream.succeed(Right(handler.error(id, e))),
          _ => ZStream.empty
        )
      )

    override def make[R, E](
      interpreter: GraphQLInterpreter[R, E],
      skipValidation: Boolean,
      enableIntrospection: Boolean,
      keepAliveTime: Option[Duration],
      queryExecution: QueryExecution,
      webSocketHooks: WebSocketHooks[R, E]
    ): URIO[R, CalibanPipe] =
      for {
        env           <- ZIO.environment[R]
        subscriptions <- SubscriptionManager.make
        ack           <- Ref.make(false)
        pipe          <- ZIO.succeed[CalibanPipe] { input =>
                           input.flatMap {
                             case GraphQLWSInput(Ops.ConnectionInit, id, payload)  =>
                               val before = ZStream.whenCase((webSocketHooks.beforeInit, payload)) {
                                 case (Some(beforeInit), Some(payload)) =>
                                   hookOrError(id, beforeInit(payload))
                               }

                               val ackPayload = webSocketHooks.onAck.fold[URIO[R, Option[ResponseValue]]](ZIO.none)(_.option)
                               val response   =
                                 ZStream.unwrap(
                                   ack.set(true) *> ackPayload.map(payload =>
                                     ZStream.succeed(Right(handler.connectionAck(payload)))
                                   )
                                 )
                               val ka         = ping(keepAliveTime)
                               val after      = ZStream.whenCase(webSocketHooks.afterInit) { case Some(afterInit) =>
                                 hookOrError(id, afterInit)
                               }

                               before ++ (response.merge(ka).merge(after))
                             case GraphQLWSInput(Ops.Pong, id, payload)            =>
                               ZStream.whenCase(webSocketHooks.onPong -> payload) { case (Some(onPong), Some(payload)) =>
                                 hookOrError(id, onPong(payload))
                               }
                             case GraphQLWSInput(Ops.Subscribe, Some(id), payload) =>
                               val request = payload.collect { case InputValue.ObjectValue(fields) =>
                                 val query         = fields.get("query").collect { case StringValue(v) => v }
                                 val operationName = fields.get("operationName").collect { case StringValue(v) => v }
                                 val variables     = fields.get("variables").collect { case InputValue.ObjectValue(v) => v }
                                 val extensions    = fields.get("extensions").collect { case InputValue.ObjectValue(v) => v }
                                 GraphQLRequest(query, operationName, variables, extensions)
                               }

                               val continue = request match {
                                 case Some(req) =>
                                   val stream = handler.generateGraphQLResponse(
                                     req,
                                     id,
                                     interpreter,
                                     skipValidation,
                                     enableIntrospection,
                                     queryExecution,
                                     subscriptions
                                   )

                                   ZStream.unwrap(subscriptions.isTracking(id).map {
                                     if (_)
                                       ZStream.succeed(Left(GraphQLWSClose(4409, s"Subscriber for $id already exists")))
                                     else
                                       webSocketHooks.onMessage
                                         .map(_.transform(stream))
                                         .getOrElse(stream)
                                         .map(Right(_))
                                         .catchAll(e => ZStream.succeed(Right(handler.error(Some(id), e))))
                                   })

                                 case None => ZStream.succeed(Right(connectionError))
                               }

                               ZStream.unwrap(ack.get.map {
                                 if (_) continue
                                 else ZStream.succeed(Left(GraphQLWSClose(4401, "Unauthorized")))
                               })
                             case GraphQLWSInput(Ops.Complete, Some(id), _)        =>
                               ZStream.fromZIO(subscriptions.untrack(id)) *> ZStream.empty
                             case GraphQLWSInput(unsupported, _, _)                =>
                               ZStream.succeed(Left(GraphQLWSClose(4400, s"Unsupported operation: $unsupported")))
                           }
                             .catchAll(_ => ZStream.succeed(Right(connectionError)))
                             .ensuring(subscriptions.untrackAll)
                             .provideEnvironment(env)
                         }
      } yield pipe

    private val connectionError: GraphQLWSOutput = GraphQLWSOutput(Ops.Error, None, None)

    private def ping(keepAlive: Option[Duration]): UStream[Either[Nothing, GraphQLWSOutput]] =
      keepAlive match {
        case None           => ZStream.empty
        case Some(duration) =>
          ZStream
            .repeatWithSchedule(Right(GraphQLWSOutput(Ops.Ping, None, None)), Schedule.spaced(duration))
      }

  }

  object Legacy extends Protocol {
    object Ops {
      final val ConnectionInit      = "connection_init"
      final val ConnectionAck       = "connection_ack"
      final val ConnectionKeepAlive = "ka"
      final val ConnectionTerminate = "connection_terminate"
      final val Start               = "start"
      final val Stop                = "stop"
      final val Error               = "error"
      final val ConnectionError     = "connection_error"
      final val Complete            = "complete"
      final val Data                = "data"
    }

    val name = "graphql-ws"

    val handler: ResponseHandler = new ResponseHandler {
      override def toResponse[E](id: String, r: GraphQLResponse[E]): GraphQLWSOutput =
        GraphQLWSOutput(Ops.Data, Some(id), Some(r.toResponseValue))

      override def complete(id: String): GraphQLWSOutput =
        GraphQLWSOutput(Ops.Complete, Some(id), None)

      override def error[E](id: Option[String], e: E): GraphQLWSOutput =
        GraphQLWSOutput(
          Ops.Error,
          id,
          Some(ResponseValue.ListValue(List(e match {
            case e: CalibanError => e.toResponseValue
            case e               => StringValue(e.toString)
          })))
        )

      override def connectionAck(payload: Option[ResponseValue]): GraphQLWSOutput =
        GraphQLWSOutput(Ops.ConnectionAck, None, payload)
    }

    private def hookOrError[R, E](
      id: Option[String],
      hook: ZIO[R, E, Any]
    ): ZStream[R, Nothing, Either[Nothing, GraphQLWSOutput]] =
      ZStream.unwrap(
        hook.fold(
          e => ZStream.succeed(Right(handler.error(id, e))),
          _ => ZStream.empty
        )
      )

    override def make[R, E](
      interpreter: GraphQLInterpreter[R, E],
      skipValidation: Boolean,
      enableIntrospection: Boolean,
      keepAliveTime: Option[Duration],
      queryExecution: QueryExecution,
      webSocketHooks: WebSocketHooks[R, E]
    ): URIO[R, CalibanPipe] =
      for {
        env           <- ZIO.environment[R]
        ack           <- Ref.make(false)
        subscriptions <- SubscriptionManager.make
        pipe          <- ZIO.succeed[CalibanPipe] { input =>
                           input.flatMap {
                             case GraphQLWSInput(Ops.ConnectionInit, id, payload) =>
                               val before     = ZStream.whenCase((webSocketHooks.beforeInit, payload)) {
                                 case (Some(beforeInit), Some(payload)) =>
                                   hookOrError(id, beforeInit(payload))
                               }
                               val ackPayload = webSocketHooks.onAck.fold[URIO[R, Option[ResponseValue]]](ZIO.none)(_.option)

                               val response =
                                 ZStream.fromZIO(
                                   ack.set(true) *> ackPayload.map(payload => Right(handler.connectionAck(payload)))
                                 )
                               val ka       = keepAlive(keepAliveTime)
                               val after    = ZStream.whenCase(webSocketHooks.afterInit) { case Some(afterInit) =>
                                 hookOrError(id, afterInit)
                               }

                               before ++ (response.merge(ka).merge(after))
                             case GraphQLWSInput(Ops.Start, id, payload)          =>
                               val request  = payload.collect { case InputValue.ObjectValue(fields) =>
                                 val query         = fields.get("query").collect { case StringValue(v) => v }
                                 val operationName = fields.get("operationName").collect { case StringValue(v) => v }
                                 val variables     = fields.get("variables").collect { case InputValue.ObjectValue(v) => v }
                                 val extensions    = fields.get("extensions").collect { case InputValue.ObjectValue(v) => v }
                                 GraphQLRequest(query, operationName, variables, extensions)
                               }
                               val continue = request match {
                                 case Some(req) =>
                                   val stream = handler.generateGraphQLResponse(
                                     req,
                                     id.getOrElse(""),
                                     interpreter,
                                     skipValidation,
                                     enableIntrospection,
                                     queryExecution,
                                     subscriptions
                                   )
                                   webSocketHooks.onMessage
                                     .map(_.transform(stream))
                                     .getOrElse(stream)
                                     .map(Right(_))
                                     .catchAll(e => ZStream.succeed(Right(handler.error(id, e))))

                                 case None => ZStream.succeed(Right(connectionError))
                               }

                               ZStream.unwrap(ack.get.map {
                                 if (_) continue
                                 else ZStream.succeed(Left(GraphQLWSClose(4401, "Unauthorized")))
                               })

                             case GraphQLWSInput(Ops.Stop, Some(id), _)         =>
                               ZStream.fromZIO(subscriptions.untrack(id)) *> ZStream.empty
                             case GraphQLWSInput(Ops.ConnectionTerminate, _, _) =>
                               ZStream.fromZIO(ZIO.interrupt) *> ZStream.empty
                             case _                                             =>
                               ZStream.empty
                           }
                             .catchAll(_ => ZStream.succeed(Right(connectionError)))
                             .ensuring(subscriptions.untrackAll)
                             .provideEnvironment(env)
                         }
      } yield pipe

    private def keepAlive(keepAlive: Option[Duration]): UStream[Either[Nothing, GraphQLWSOutput]] =
      keepAlive match {
        case None           => ZStream.empty
        case Some(duration) =>
          ZStream
            .repeatWithSchedule(Right(GraphQLWSOutput(Ops.ConnectionKeepAlive, None, None)), Schedule.spaced(duration))
      }

    private val connectionError: GraphQLWSOutput = GraphQLWSOutput(Ops.ConnectionError, None, None)

  }

  private[ws] trait ResponseHandler {
    self =>
    def toResponse[E](id: String, fieldName: String, r: ResponseValue, errors: List[E]): GraphQLWSOutput =
      toResponse(id, GraphQLResponse(ObjectValue(List(fieldName -> r)), errors))

    def toResponse[E](id: String, r: GraphQLResponse[E]): GraphQLWSOutput

    def complete(id: String): GraphQLWSOutput

    def error[E](id: Option[String], e: E): GraphQLWSOutput

    def connectionAck(payload: Option[ResponseValue]): GraphQLWSOutput

    def toStreamComplete(id: String): UStream[GraphQLWSOutput] =
      ZStream.succeed(complete(id))

    def toStreamError[E](id: Option[String], e: E): UStream[GraphQLWSOutput] =
      ZStream.succeed(error(id, e))

    final def generateGraphQLResponse[R, E](
      payload: GraphQLRequest,
      id: String,
      interpreter: GraphQLInterpreter[R, E],
      skipValidation: Boolean,
      enableIntrospection: Boolean,
      queryExecution: QueryExecution,
      subscriptions: SubscriptionManager
    ): ZStream[R, E, GraphQLWSOutput] = {
      val resp =
        ZStream
          .fromZIO(interpreter.executeRequest(payload, skipValidation, enableIntrospection, queryExecution))
          .flatMap(res =>
            res.data match {
              case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                subscriptions.track(id).flatMap { p =>
                  stream.map(self.toResponse(id, fieldName, _, res.errors)).interruptWhen(p)
                }
              case other                                                =>
                ZStream.succeed(self.toResponse(id, GraphQLResponse(other, res.errors)))
            }
          )

      (resp ++ self.toStreamComplete(id)).catchAll(self.toStreamError(Option(id), _))
    }
  }

  private[ws] class SubscriptionManager private (private val tracked: TMap[String, Promise[Any, Unit]]) {
    def track(id: String): UStream[Promise[Any, Unit]] =
      ZStream.fromZIO(Promise.make[Any, Unit].tap(tracked.put(id, _).commit))

    def untrack(id: String): UIO[Unit] =
      (tracked.get(id) <* tracked.delete(id)).commit.flatMap {
        case None    => ZIO.unit
        case Some(p) => p.succeed(()).unit
      }

    def untrackAll: UIO[Unit] =
      tracked.keys.map(ids => ZIO.foreachDiscard(ids)(untrack)).commit.flatten

    def isTracking(id: String): UIO[Boolean] = tracked.contains(id).commit
  }

  private object SubscriptionManager {
    val make = TMap.make[String, Promise[Any, Unit]]().map(new SubscriptionManager(_)).commit
  }

}

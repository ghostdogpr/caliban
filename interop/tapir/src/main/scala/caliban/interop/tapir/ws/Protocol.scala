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
    }

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
        output        <- Queue.unbounded[Either[GraphQLWSClose, GraphQLWSOutput]]
        pipe          <- ZIO.succeed[CalibanPipe] { input =>
                           ZStream.scoped(
                             input.mapZIO {
                               case GraphQLWSInput(Ops.ConnectionInit, id, payload)  =>
                                 val before     = ZIO.whenCase((webSocketHooks.beforeInit, payload)) {
                                   case (Some(beforeInit), Some(payload)) =>
                                     beforeInit(payload).catchAll(e => output.offer(Right(handler.error(id, e))))
                                 }
                                 val ackPayload = webSocketHooks.onAck.fold[URIO[R, Option[ResponseValue]]](ZIO.none)(_.option)
                                 val response   =
                                   ack.set(true) *> ackPayload.flatMap(payload => output.offer(Right(connectionAck(payload))))
                                 val ka         = ping(keepAliveTime).mapZIO(output.offer).runDrain.fork
                                 val after      = ZIO.whenCase(webSocketHooks.afterInit) { case Some(afterInit) =>
                                   afterInit
                                     .catchAllCause(cause =>
                                       ZIO.foreachDiscard(cause.failureOption)(e =>
                                         output.offer(Right(handler.error(id, e)))
                                       ) *> output.offer(Left(GraphQLWSClose(4401, "Unauthorized")))
                                     )
                                     .fork
                                 }

                                 before *> response *> ka *> after
                               case GraphQLWSInput(Ops.Pong, id, payload)            =>
                                 ZIO.whenCase(webSocketHooks.onPong -> payload) { case (Some(onPong), Some(payload)) =>
                                   onPong(payload).catchAll(e => output.offer(Right(handler.error(id, e))))
                                 }
                               case GraphQLWSInput(Ops.Ping, id, payload)            =>
                                 def sendPong(p: Option[ResponseValue]) = output.offer(Right(GraphQLWSOutput(Ops.Pong, id, p)))

                                 webSocketHooks.onPing match {
                                   case Some(onPing) =>
                                     onPing(payload).flatMap(sendPong).catchAll(e => output.offer(Right(handler.error(id, e))))
                                   case _            => sendPong(None)
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

                                     ZIO.ifZIO(subscriptions.isTracking(id))(
                                       output.offer(Left(GraphQLWSClose(4409, s"Subscriber for $id already exists"))).unit,
                                       webSocketHooks.onMessage
                                         .map(_.transform(stream))
                                         .getOrElse(stream)
                                         .map(Right(_))
                                         .runForeachChunk(output.offerAll)
                                         .catchAll(e => output.offer(Right(handler.error(Some(id), e))))
                                         .fork
                                         .interruptible
                                         .unit
                                     )

                                   case None => output.offer(Right(connectionError))
                                 }

                                 ZIO.ifZIO(ack.get)(continue, output.offer(Left(GraphQLWSClose(4401, "Unauthorized"))))
                               case GraphQLWSInput(Ops.Complete, Some(id), _)        =>
                                 subscriptions.untrack(id)
                               case GraphQLWSInput(unsupported, _, _)                =>
                                 output.offer(Left(GraphQLWSClose(4400, s"Unsupported operation: $unsupported")))
                             }.runDrain.interruptible
                               .catchAll(_ => output.offer(Right(connectionError)))
                               .ensuring(subscriptions.untrackAll)
                               .provideEnvironment(env)
                               .forkScoped
                           ) *> ZStream.fromQueueWithShutdown(output)
                         }
      } yield pipe

    private val connectionError: GraphQLWSOutput                               = GraphQLWSOutput(Ops.Error, None, None)
    private def connectionAck(payload: Option[ResponseValue]): GraphQLWSOutput =
      GraphQLWSOutput(Ops.ConnectionAck, None, payload)

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
    }

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
        output        <- Queue.unbounded[Either[GraphQLWSClose, GraphQLWSOutput]]
        pipe          <- ZIO.succeed[CalibanPipe] { input =>
                           ZStream
                             .acquireReleaseWith(
                               input.collectZIO {
                                 case GraphQLWSInput(Ops.ConnectionInit, id, payload) =>
                                   val before     = ZIO.whenCase((webSocketHooks.beforeInit, payload)) {
                                     case (Some(beforeInit), Some(payload)) =>
                                       beforeInit(payload).catchAll(e => output.offer(Right(handler.error(id, e))))
                                   }
                                   val ackPayload = webSocketHooks.onAck.fold[URIO[R, Option[ResponseValue]]](ZIO.none)(_.option)

                                   val response =
                                     ack.set(true) *> ackPayload.flatMap(payload => output.offer(Right(connectionAck(payload))))
                                   val ka       = keepAlive(keepAliveTime).mapZIO(o => output.offer(Right(o))).runDrain.fork
                                   val after    = ZIO.whenCase(webSocketHooks.afterInit) { case Some(afterInit) =>
                                     afterInit
                                       .catchAllCause(cause =>
                                         ZIO.foreachDiscard(cause.failureOption)(e =>
                                           output.offer(Right(handler.error(id, e)))
                                         ) *> output.offer(Left(GraphQLWSClose(4401, "Unauthorized")))
                                       )
                                       .fork
                                   }

                                   before *> response *> ka *> after
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
                                         .runForeachChunk(o => output.offerAll(o.map(Right(_))))
                                         .catchAll(e => output.offer(Right(handler.error(id, e))))
                                         .fork
                                         .interruptible
                                         .unit

                                     case None => output.offer(Right(connectionError))
                                   }

                                   ZIO.ifZIO(ack.get)(continue, output.offer(Left(GraphQLWSClose(4401, "Unauthorized"))))
                                 case GraphQLWSInput(Ops.Stop, Some(id), _)           =>
                                   subscriptions.untrack(id)
                                 case GraphQLWSInput(Ops.ConnectionTerminate, _, _)   =>
                                   ZIO.interrupt
                               }.runDrain.interruptible
                                 .catchAll(_ => output.offer(Right(connectionError)))
                                 .ensuring(subscriptions.untrackAll)
                                 .provideEnvironment(env)
                                 .forkDaemon
                             )(_.interrupt) *> ZStream.fromQueueWithShutdown(output)
                         }
      } yield pipe

    private def keepAlive(keepAlive: Option[Duration]): UStream[GraphQLWSOutput] =
      keepAlive match {
        case None           => ZStream.empty
        case Some(duration) =>
          ZStream
            .repeatWithSchedule(GraphQLWSOutput(Ops.ConnectionKeepAlive, None, None), Schedule.spaced(duration))
      }

    private val connectionError: GraphQLWSOutput                               = GraphQLWSOutput(Ops.ConnectionError, None, None)
    private def connectionAck(payload: Option[ResponseValue]): GraphQLWSOutput =
      GraphQLWSOutput(Ops.ConnectionAck, None, payload)
  }

  private[ws] trait ResponseHandler {
    self =>
    def toResponse[E](id: String, fieldName: String, r: ResponseValue, errors: List[E]): GraphQLWSOutput =
      toResponse(id, GraphQLResponse(ObjectValue(List(fieldName -> r)), errors))

    def toResponse[E](id: String, r: GraphQLResponse[E]): GraphQLWSOutput

    def complete(id: String): GraphQLWSOutput

    def error[E](id: Option[String], e: E): GraphQLWSOutput

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

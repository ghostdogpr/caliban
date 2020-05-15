package caliban

import akka.stream.{ Materializer, OverflowStrategy, QueueOfferResult }
import akka.stream.scaladsl.{ Flow, Sink, Source, SourceQueueWithComplete }
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import caliban.interop.play.json.parsingException
import play.api.http.Writeable
import play.api.libs.json.{ Json, JsValue, Writes, _ }
import play.api.mvc.{ Action, ActionBuilder, AnyContent, PlayBodyParsers, Request, RequestHeader, Result, WebSocket }
import play.api.mvc.Results.Ok
import zio.{ CancelableFuture, Fiber, IO, RIO, Ref, Runtime, Schedule, Task, ZIO }
import zio.clock.Clock
import zio.duration.Duration
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

trait PlayAdapter {

  def actionBuilder: ActionBuilder[Request, AnyContent]
  def parse: PlayBodyParsers

  implicit def writableGraphQLResponse[E](implicit wr: Writes[GraphQLResponse[E]]): Writeable[GraphQLResponse[E]] =
    Writeable.writeableOf_JsValue.map(wr.writes)

  private def parseJson(s: String): Try[JsValue] =
    Try(Json.parse(s))

  private def getGraphQLRequest(
    query: String,
    op: Option[String],
    vars: Option[String],
    exts: Option[String]
  ): Either[Throwable, GraphQLRequest] = {
    val variablesJs  = vars.flatMap(parseJson(_).toOption)
    val extensionsJs = exts.flatMap(parseJson(_).toOption)
    val fields = List("query" -> JsString(query)) ++
      op.map(o => "operationName"         -> JsString(o)) ++
      variablesJs.map(js => "variables"   -> js) ++
      extensionsJs.map(js => "extensions" -> js)
    JsObject(fields)
      .validate[GraphQLRequest]
      .asEither
      .left
      .map(parsingException)
  }

  private def executeRequest[R, E](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest,
    skipValidation: Boolean,
    enableIntrospection: Boolean
  )(implicit runtime: Runtime[R]): CancelableFuture[Result] =
    runtime.unsafeRunToFuture(
      interpreter
        .executeRequest(request, skipValidation = skipValidation, enableIntrospection = enableIntrospection)
        .catchAllCause(cause => ZIO.succeed(GraphQLResponse[Throwable](NullValue, cause.defects)))
        .map(Ok(_))
    )

  def makePostAction[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true
  )(implicit runtime: Runtime[R]): Action[GraphQLRequest] =
    actionBuilder.async(parse.json[GraphQLRequest])(req =>
      executeRequest(
        interpreter,
        req.body,
        skipValidation,
        enableIntrospection
      )
    )

  def makeGetAction[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true
  )(
    query: String,
    variables: Option[String],
    operation: Option[String],
    extensions: Option[String]
  )(implicit runtime: Runtime[R]): Action[AnyContent] =
    actionBuilder.async(
      getGraphQLRequest(
        query,
        variables,
        operation,
        extensions
      ).fold(
        Future.failed,
        executeRequest(interpreter, _, skipValidation, enableIntrospection)
      )
    )

  private def webSocketFlow[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    keepAliveTime: Option[Duration]
  )(
    implicit ec: ExecutionContext,
    materializer: Materializer,
    runtime: Runtime[R]
  ): Flow[PlayWSMessage, PlayWSMessage, Unit] = {
    def sendMessage(
      sendQueue: SourceQueueWithComplete[PlayWSMessage],
      id: Option[String],
      data: ResponseValue,
      errors: List[E]
    ): Task[QueueOfferResult] =
      IO.fromFuture(_ => sendQueue.offer(PlayWSMessage("data", id, GraphQLResponse(data, errors))))

    def startSubscription(
      messageId: Option[String],
      request: GraphQLRequest,
      sendTo: SourceQueueWithComplete[PlayWSMessage],
      subscriptions: Ref[Map[Option[String], Fiber[Throwable, Unit]]]
    ): RIO[R, Unit] =
      for {
        result <- interpreter.executeRequest(
                   request,
                   skipValidation = skipValidation,
                   enableIntrospection = enableIntrospection
                 )
        _ <- result.data match {
              case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                stream
                  .foreach(item => sendMessage(sendTo, messageId, ObjectValue(List(fieldName -> item)), result.errors))
                  .forkDaemon
                  .flatMap(fiber => subscriptions.update(_.updated(messageId, fiber)))
              case other =>
                sendMessage(sendTo, messageId, other, result.errors) *>
                  IO.fromFuture(_ => sendTo.offer(PlayWSMessage("complete", messageId)))
            }
      } yield ()

    val (queue, source) = Source.queue[PlayWSMessage](0, OverflowStrategy.fail).preMaterialize()
    val subscriptions   = runtime.unsafeRun(Ref.make(Map.empty[Option[String], Fiber[Throwable, Unit]]))

    val sink = Sink.foreach[PlayWSMessage] { msg =>
      val io = for {
        _ <- IO.whenCase(msg.messageType) {
              case "connection_init" =>
                Task.fromFuture(_ => queue.offer(PlayWSMessage("connection_ack"))) *>
                  Task.whenCase(keepAliveTime) {
                    case Some(time) =>
                      // Save the keep-alive fiber with a key of None so that it's interrupted later
                      IO.fromFuture(_ => queue.offer(PlayWSMessage("ka")))
                        .repeat(Schedule.spaced(time))
                        .provideLayer(Clock.live)
                        .unit
                        .forkDaemon
                        .flatMap(keepAliveFiber => subscriptions.update(_.updated(None, keepAliveFiber)))
                  }
              case "connection_terminate" =>
                IO.effect(queue.complete())
              case "start" =>
                Task.whenCase(msg.request) {
                  case Some(req) =>
                    startSubscription(msg.id, req, queue, subscriptions)
                      .catchAll(error =>
                        IO.fromFuture(_ => queue.offer(PlayWSMessage("complete", Some(error.toString))))
                      )
                }
              case "stop" =>
                subscriptions
                  .modify(map => (map.get(msg.id), map - msg.id))
                  .flatMap(fiber =>
                    IO.whenCase(fiber) {
                      case Some(fiber) =>
                        fiber.interrupt *>
                          IO.fromFuture(_ => queue.offer(PlayWSMessage("complete", msg.id)))
                    }
                  )
            }
      } yield ()
      runtime.unsafeRun(io)
    }

    Flow.fromSinkAndSource(sink, source).watchTermination() { (_, f) =>
      f.onComplete(_ => runtime.unsafeRun(subscriptions.get.flatMap(m => IO.foreach(m.values)(_.interrupt).unit)))
    }
  }

  def makeWebSocket[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None
  )(implicit ec: ExecutionContext, runtime: Runtime[R], materializer: Materializer): WebSocket =
    WebSocket.accept(_ => webSocketFlow(interpreter, skipValidation, enableIntrospection, keepAliveTime))

  def makeWakeSocketOrResult[R, E](
    interpreter: GraphQLInterpreter[R, E],
    handleRequestHeader: RequestHeader => Future[Either[Result, Unit]],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None
  )(implicit ec: ExecutionContext, runtime: Runtime[R], materializer: Materializer): WebSocket =
    WebSocket
      .acceptOrResult(requestHeader =>
        handleRequestHeader(requestHeader)
          .map(_.map(_ => webSocketFlow(interpreter, skipValidation, enableIntrospection, keepAliveTime)))
      )
}

object PlayAdapter {
  def apply(
    playBodyParsers: PlayBodyParsers,
    _actionBuilder: ActionBuilder[Request, AnyContent]
  ): PlayAdapter =
    new PlayAdapter {
      override def parse: PlayBodyParsers                            = playBodyParsers
      override def actionBuilder: ActionBuilder[Request, AnyContent] = _actionBuilder
    }
}

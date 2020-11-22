package caliban

import akka.stream.scaladsl.{ Flow, Sink, Source, SourceQueueWithComplete }
import akka.stream.{ Materializer, OverflowStrategy, QueueOfferResult }
import caliban.PlayAdapter.RequestWrapper
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import caliban.interop.play.json.parsingException
import play.api.http.Writeable
import play.api.libs.json.{ JsValue, Json, Writes }
import play.api.mvc.Results.Ok
import play.api.mvc._
import zio.Exit.Failure
import zio.clock.Clock
import zio.duration.Duration
import zio.{ CancelableFuture, Fiber, IO, RIO, Ref, Runtime, Schedule, Task, URIO, ZIO }

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

trait PlayAdapter[R] {

  val `application/graphql` = "application/graphql"
  def actionBuilder: ActionBuilder[Request, AnyContent]
  def parse: PlayBodyParsers
  def requestWrapper: RequestWrapper[R]

  implicit def writableGraphQLResponse[E](implicit wr: Writes[GraphQLResponse[E]]): Writeable[GraphQLResponse[E]] =
    Writeable.writeableOf_JsValue.map(wr.writes)

  implicit val graphQLReqReader = Json.reads[GraphQLRequest]

  private def parseJson(s: String): Try[JsValue] =
    Try(Json.parse(s))

  private def getGraphQLRequest(
    query: Option[String],
    op: Option[String],
    vars: Option[String],
    exts: Option[String]
  ): Either[Throwable, GraphQLRequest] = {
    val variablesJs  = vars.flatMap(parseJson(_).toOption)
    val extensionsJs = exts.flatMap(parseJson(_).toOption)
    Json
      .obj(
        "query"         -> query,
        "operationName" -> op,
        "variables"     -> variablesJs,
        "extensions"    -> extensionsJs
      )
      .validate[GraphQLRequest]
      .asEither
      .left
      .map(parsingException)
  }

  private def executeRequest[E](
    interpreter: GraphQLInterpreter[R, E],
    request: Request[GraphQLRequest],
    skipValidation: Boolean,
    enableIntrospection: Boolean
  )(implicit runtime: Runtime[R]): CancelableFuture[Result] =
    runtime.unsafeRunToFuture(
      requestWrapper(request)(
        interpreter
          .executeRequest(request.body, skipValidation = skipValidation, enableIntrospection = enableIntrospection)
          .catchAllCause(cause => ZIO.succeed(GraphQLResponse[Throwable](NullValue, cause.defects)))
          .map(Ok(_))
      )
    )

  private def graphqlBodyParser(implicit ec: ExecutionContext): BodyParser[GraphQLRequest] = parse.using { rh =>
    rh.contentType match {
      case Some(`application/graphql`) => parse.text.map(text => GraphQLRequest(query = Some(text)))
      case _                           => parse.json[GraphQLRequest]
    }
  }

  def makePostAction[E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true
  )(implicit runtime: Runtime[R]): Action[GraphQLRequest] =
    actionBuilder.async(graphqlBodyParser(runtime.platform.executor.asEC)) { req =>
      executeRequest(
        interpreter,
        req,
        skipValidation,
        enableIntrospection
      )
    }

  def makeGetAction[E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true
  )(
    query: Option[String],
    variables: Option[String],
    operation: Option[String],
    extensions: Option[String]
  )(implicit runtime: Runtime[R]): Action[AnyContent] =
    actionBuilder.async(req =>
      getGraphQLRequest(
        query,
        operation,
        variables,
        extensions
      ).fold(
        Future.failed,
        body => executeRequest(interpreter, req.withBody(body), skipValidation, enableIntrospection)
      )
    )

  private def webSocketFlow[E](
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
                  .onExit {
                    case Failure(cause) if !cause.interrupted =>
                      IO.fromFuture(_ =>
                          sendTo.offer(PlayWSMessage("error", messageId, Json.obj("message" -> cause.squash.toString)))
                        )
                        .orDie
                    case _ =>
                      IO.fromFuture(_ => sendTo.offer(PlayWSMessage("complete", messageId))).orDie
                  }
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
        _ <- RIO.whenCase(msg.messageType) {
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
                RIO.whenCase(msg.request) {
                  case Some(req) =>
                    startSubscription(msg.id, req, queue, subscriptions)
                      .catchAll(error =>
                        IO.fromFuture(_ =>
                          queue.offer(PlayWSMessage("error", msg.id, Json.obj("message" -> error.toString)))
                        )
                      )
                }
              case "stop" =>
                subscriptions
                  .modify(map => (map.get(msg.id), map - msg.id))
                  .flatMap(fiber =>
                    IO.whenCase(fiber) {
                      case Some(fiber) => fiber.interrupt
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

  def makeWebSocket[E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None
  )(implicit ec: ExecutionContext, runtime: Runtime[R], materializer: Materializer): WebSocket =
    WebSocket.accept(_ => webSocketFlow(interpreter, skipValidation, enableIntrospection, keepAliveTime))

  def makeWakeSocketOrResult[E](
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
  def apply[R](
    playBodyParsers: PlayBodyParsers,
    _actionBuilder: ActionBuilder[Request, AnyContent],
    wrapper: RequestWrapper[R] = RequestWrapper.empty
  ): PlayAdapter[R] =
    new PlayAdapter[R] {
      override def parse: PlayBodyParsers                            = playBodyParsers
      override def actionBuilder: ActionBuilder[Request, AnyContent] = _actionBuilder
      override def requestWrapper: RequestWrapper[R]                 = wrapper
    }

  trait RequestWrapper[-R] { self =>
    def apply[R1 <: R](ctx: RequestHeader)(e: URIO[R1, Result]): URIO[R1, Result]

    def |+|[R1 <: R](that: RequestWrapper[R1]): RequestWrapper[R1] = new RequestWrapper[R1] {
      override def apply[R2 <: R1](ctx: RequestHeader)(e: URIO[R2, Result]): URIO[R2, Result] =
        that.apply[R2](ctx)(self.apply[R2](ctx)(e))
    }
  }

  object RequestWrapper {
    lazy val empty: RequestWrapper[Any] = new RequestWrapper[Any] {
      override def apply[R](ctx: RequestHeader)(effect: URIO[R, Result]): URIO[R, Result] = effect
    }
  }
}

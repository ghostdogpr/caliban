package caliban

import akka.stream.scaladsl.{ Flow, Sink, Source, SourceQueueWithComplete }
import akka.stream.{ Materializer, OverflowStrategy, QueueOfferResult }
import caliban.PlayAdapter.RequestWrapper
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import caliban.execution.QueryExecution
import caliban.interop.play.json.parsingException
import caliban.uploads._
import play.api.http.Writeable
import play.api.libs.json.{ JsValue, Json, Writes }
import play.api.mvc.Results.Ok
import play.api.mvc._
import play.mvc.Http.MimeTypes
import zio.Exit.Failure
import zio.blocking.Blocking
import zio.clock.Clock
import zio.duration.Duration
import zio.random.Random
import zio.{ random, CancelableFuture, Fiber, Has, IO, RIO, Ref, Runtime, Schedule, Task, URIO, ZIO, ZLayer }

import java.util.Locale
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

trait PlayAdapter[R <: Has[_] with Blocking with Random] {

  val `application/graphql` = "application/graphql"
  def actionBuilder: ActionBuilder[Request, AnyContent]
  def parse: PlayBodyParsers
  def requestWrapper: RequestWrapper[R]

  implicit def writableGraphQLResponse[E](implicit wr: Writes[GraphQLResponse[E]]): Writeable[GraphQLResponse[E]] =
    Writeable.writeableOf_JsValue.map(wr.writes)

  def makePostAction[E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit runtime: Runtime[R]): Action[Either[GraphQLUploadRequest, GraphQLRequest]] =
    actionBuilder.async(makeParser(runtime)) { req =>
      req.body match {
        case Left(value) =>
          executeRequest[E](
            interpreter,
            req.withBody(value.remap),
            skipValidation,
            enableIntrospection,
            queryExecution,
            value.fileHandle.toLayerMany
          )

        case Right(value) =>
          executeRequest[E](
            interpreter,
            req.withBody(value),
            skipValidation,
            enableIntrospection,
            queryExecution
          )
      }
    }

  private def uploadFormParser(
    runtime: Runtime[Random]
  ): BodyParser[GraphQLUploadRequest] =
    parse.multipartFormData.validateM { form =>
      // First bit is always a standard graphql payload, it comes from the `operations` field
      val tryOperations =
        parseJson(form.dataParts("operations").head).map(_.as[GraphQLRequest])
      // Second bit is the mapping field
      val tryMap        = parseJson(form.dataParts("map").head)
        .map(_.as[Map[String, Seq[String]]])

      runtime.unsafeRunToFuture(
        (for {
          operations <- ZIO
                          .fromTry(tryOperations)
                          .orElseFail(Results.BadRequest("Missing multipart field 'operations'"))
          map        <- ZIO
                          .fromTry(tryMap)
                          .orElseFail(Results.BadRequest("Missing multipart field 'map'"))
          filePaths   = map.map { case (key, value) => (key, value.map(parsePath).toList) }.toList
                          .flatMap(kv => kv._2.map(kv._1 -> _))
          fileRef    <- Ref.make(form.files.map(f => f.key -> f).toMap)
          rand       <- ZIO.environment[Random]
        } yield GraphQLUploadRequest(
          operations,
          filePaths,
          Uploads.handler(handle =>
            fileRef.get
              .map(_.get(handle))
              .some
              .flatMap(fp =>
                random
                  .nextString(16)
                  .asSomeError
                  .map(
                    FileMeta(
                      _,
                      fp.ref.path,
                      fp.dispositionType,
                      fp.contentType,
                      fp.filename,
                      fp.fileSize
                    )
                  )
              )
              .optional
              .provide(rand)
          )
        )).either
      )
    }(runtime.platform.executor.asEC)

  private def parsePath(path: String): List[Either[String, Int]] =
    path.split('.').map(c => Try(c.toInt).toEither.left.map(_ => c)).toList

  def makeGetAction[E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
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
        body => executeRequest(interpreter, req.withBody(body), skipValidation, enableIntrospection, queryExecution)
      )
    )

  private def supportFederatedTracing(request: Request[GraphQLRequest]): Request[GraphQLRequest] =
    if (request.headers.get(GraphQLRequest.`apollo-federation-include-trace`).contains(GraphQLRequest.ftv1)) {
      request.map(_.withFederatedTracing)
    } else request

  private def executeRequest[E](
    interpreter: GraphQLInterpreter[R, E],
    request: Request[GraphQLRequest],
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    queryExecution: QueryExecution,
    fileHandle: ZLayer[Any, Nothing, Uploads] = Uploads.empty
  )(implicit runtime: Runtime[R]): CancelableFuture[Result] =
    runtime.unsafeRunToFuture(
      requestWrapper(request)(
        interpreter
          .executeRequest(
            supportFederatedTracing(request).body,
            skipValidation = skipValidation,
            enableIntrospection = enableIntrospection,
            queryExecution
          )
          .catchAllCause(cause => ZIO.succeed(GraphQLResponse[Throwable](NullValue, cause.defects)))
          .map(Ok(_))
          .provideSomeLayer[R](fileHandle)
      )
    )

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

  private def parseJson(s: String): Try[JsValue] =
    Try(Json.parse(s))

  def makeWebSocket[E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit ec: ExecutionContext, runtime: Runtime[R], materializer: Materializer): WebSocket =
    WebSocket.accept(_ =>
      webSocketFlow(interpreter, skipValidation, enableIntrospection, keepAliveTime, queryExecution)
    )

  private def webSocketFlow[E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    keepAliveTime: Option[Duration],
    queryExecution: QueryExecution
  )(implicit
    ec: ExecutionContext,
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
                    enableIntrospection = enableIntrospection,
                    queryExecution
                  )
        _      <- result.data match {
                    case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                      stream
                        .foreach(item => sendMessage(sendTo, messageId, ObjectValue(List(fieldName -> item)), result.errors))
                        .onExit {
                          case Failure(cause) if !cause.interrupted =>
                            IO.fromFuture(_ =>
                              sendTo.offer(PlayWSMessage("error", messageId, Json.obj("message" -> cause.squash.toString)))
                            ).orDie
                          case _                                    =>
                            IO.fromFuture(_ => sendTo.offer(PlayWSMessage("complete", messageId))).orDie
                        }
                        .forkDaemon
                        .flatMap(fiber => subscriptions.update(_.updated(messageId, fiber)))
                    case other                                                =>
                      sendMessage(sendTo, messageId, other, result.errors) *>
                        IO.fromFuture(_ => sendTo.offer(PlayWSMessage("complete", messageId)))
                  }
      } yield ()

    val (queue, source) = Source.queue[PlayWSMessage](0, OverflowStrategy.fail).preMaterialize()
    val subscriptions   = runtime.unsafeRun(Ref.make(Map.empty[Option[String], Fiber[Throwable, Unit]]))

    val sink = Sink.foreach[PlayWSMessage] { msg =>
      val io = for {
        _ <- RIO.whenCase(msg.messageType) {
               case "connection_init"      =>
                 Task.fromFuture(_ => queue.offer(PlayWSMessage("connection_ack"))) *>
                   Task.whenCase(keepAliveTime) { case Some(time) =>
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
               case "start"                =>
                 RIO.whenCase(msg.request) { case Some(req) =>
                   startSubscription(msg.id, req, queue, subscriptions)
                     .catchAll(error =>
                       IO.fromFuture(_ =>
                         queue.offer(PlayWSMessage("error", msg.id, Json.obj("message" -> error.toString)))
                       )
                     )
                 }
               case "stop"                 =>
                 subscriptions
                   .modify(map => (map.get(msg.id), map - msg.id))
                   .flatMap(fiber =>
                     IO.whenCase(fiber) { case Some(fiber) =>
                       fiber.interrupt
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

  def makeWakeSocketOrResult[E](
    interpreter: GraphQLInterpreter[R, E],
    handleRequestHeader: RequestHeader => Future[Either[Result, Unit]],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit ec: ExecutionContext, runtime: Runtime[R], materializer: Materializer): WebSocket =
    WebSocket
      .acceptOrResult(requestHeader =>
        handleRequestHeader(requestHeader)
          .map(
            _.map(_ => webSocketFlow(interpreter, skipValidation, enableIntrospection, keepAliveTime, queryExecution))
          )
      )

  private def makeParser(
    runtime: Runtime[Blocking with Random]
  ): BodyParser[Either[GraphQLUploadRequest, GraphQLRequest]] =
    parse.using { req =>
      implicit val ec: ExecutionContext = runtime.platform.executor.asEC
      req.contentType.map(_.toLowerCase(Locale.ENGLISH)) match {
        case Some(`application/graphql`)              => parse.text.map(text => GraphQLRequest(query = Some(text))).map(Right(_))
        case Some("text/json") | Some(MimeTypes.JSON) =>
          parse.json[GraphQLRequest].map(Right(_))
        case Some("multipart/form-data")              =>
          uploadFormParser(runtime).map(Left(_))
        case _                                        =>
          parse.error(Future.successful(Results.BadRequest("Invalid content type")))
      }
    }
}

object PlayAdapter {
  def apply[R <: Has[_] with Blocking with Random](
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

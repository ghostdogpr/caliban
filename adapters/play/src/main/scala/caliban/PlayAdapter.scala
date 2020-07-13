package caliban

import java.util.Locale

import akka.stream.{ Materializer, OverflowStrategy, QueueOfferResult }
import akka.stream.scaladsl.{ Flow, Sink, Source, SourceQueueWithComplete }

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.interop.play.json.parsingException
import play.api.http.Writeable
import play.api.libs.json.{ JsValue, Json, Writes }
import play.api.mvc.{
  Action,
  ActionBuilder,
  AnyContent,
  BodyParser,
  PlayBodyParsers,
  Request,
  RequestHeader,
  Result,
  Results,
  WebSocket
}
import play.api.mvc.Results.Ok
import zio.{ random, CancelableFuture, Fiber, Has, IO, RIO, Ref, Runtime, Schedule, Task, ZIO, ZLayer }
import zio.clock.Clock
import zio.duration.Duration
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

import caliban.Uploads.{ FileMeta, Uploads }
import caliban.Value.NullValue
import zio.blocking.Blocking
import zio.console.Console
import zio.random.Random

trait PlayAdapter {

  def actionBuilder: ActionBuilder[Request, AnyContent]
  def parse: PlayBodyParsers

  implicit def writableGraphQLResponse[E](implicit wr: Writes[GraphQLResponse[E]]): Writeable[GraphQLResponse[E]] =
    Writeable.writeableOf_JsValue.map(wr.writes)

  type ExampleEnv = Clock with Console with Blocking with Random

  def makePostAction[R <: Blocking with Random, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true
  )(implicit runtime: Runtime[R]): Action[Either[GraphQLUploadRequest, GraphQLRequest]] =
    actionBuilder.async(makeParser(runtime)) { req =>
      req.body match {
        case Left(value) =>
          executeRequest[R, E](
            interpreter,
            value.remap,
            skipValidation,
            enableIntrospection,
            value.fileHandle.toLayerMany
          )

        case Right(value) =>
          executeRequest[R, E](
            interpreter,
            value,
            skipValidation,
            enableIntrospection
          )
      }
    }

  private def executeRequest[R <: Has[_], E](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest,
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    fileHandle: ZLayer[Any, Nothing, Uploads] = Uploads.Service.empty
  )(implicit runtime: Runtime[R]): CancelableFuture[Result] =
    runtime.unsafeRunToFuture(
      interpreter
        .executeRequest(request, skipValidation = skipValidation, enableIntrospection = enableIntrospection)
        .catchAllCause(cause => ZIO.succeed(GraphQLResponse[Throwable](NullValue, cause.defects)))
        .map(Ok(_))
        .provideSomeLayer[R](fileHandle)
    )

  private def makeParser(
    runtime: Runtime[Blocking with Random]
  ): BodyParser[Either[GraphQLUploadRequest, GraphQLRequest]] =
    parse.using { req =>
      implicit val ec: ExecutionContext = runtime.platform.executor.asEC
      req.contentType.map(_.toLowerCase(Locale.ENGLISH)) match {
        case Some("text/json") | Some("application/json") =>
          parse.json[GraphQLRequest].map(Right(_))
        case Some("multipart/form-data") =>
          uploadFormParser(runtime).map(Left(_))
        case _ =>
          parse.error(Future.successful(Results.BadRequest("Invalid content type")))
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
      val tryMap = parseJson(form.dataParts("map").head)
        .map(_.as[Map[String, Seq[String]]])

      runtime.unsafeRunToFuture(
        (for {
          operations <- ZIO
                         .fromTry(tryOperations)
                         .orElseFail(Results.BadRequest("Missing multipart field 'operations'"))
          map <- ZIO
                  .fromTry(tryMap)
                  .orElseFail(Results.BadRequest("Missing multipart field 'map'"))
          filePaths = map.map { case (key, value) => (key, value.map(parsePath).toList) }.toList
            .flatMap(kv => kv._2.map(kv._1 -> _))
          fileRef <- Ref.make(form.files.map(f => f.key -> f).toMap)
          rand    <- ZIO.environment[Random]
        } yield GraphQLUploadRequest(
          operations,
          filePaths,
          Uploads.Service.handler(handle =>
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

  private def parseJson(s: String): Try[JsValue] =
    Try(Json.parse(s))

  private def parsePath(path: String): List[Either[String, Int]] =
    path.split('.').map(c => Try(c.toInt).toEither.left.map(_ => c)).toList

  def makeGetAction[R <: Has[_], E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true
  )(
    query: Option[String],
    variables: Option[String],
    operation: Option[String],
    extensions: Option[String]
  )(implicit runtime: Runtime[R]): Action[AnyContent] =
    actionBuilder.async(
      getGraphQLRequest(
        query,
        operation,
        variables,
        extensions
      ).fold(
        Future.failed,
        executeRequest(interpreter, _, skipValidation, enableIntrospection)
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

  def makeWebSocket[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None
  )(implicit ec: ExecutionContext, runtime: Runtime[R], materializer: Materializer): WebSocket =
    WebSocket.accept(_ => webSocketFlow(interpreter, skipValidation, enableIntrospection, keepAliveTime))

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

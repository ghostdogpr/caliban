package caliban

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import caliban.execution.QueryExecution
import caliban.interop.cats.CatsInterop
import caliban.uploads._
import cats.data.{ Kleisli, OptionT }
import cats.effect.kernel.Async
import cats.effect.std.{ Dispatcher, Queue => CatsQueue }
import cats.syntax.either._
import cats.syntax.traverse._
import cats.~>
import fs2.io.file.Files
import fs2.text.utf8
import fs2.{ Pipe, Stream }
import io.circe.Decoder.Result
import io.circe.parser._
import io.circe.syntax._
import io.circe.{ DecodingFailure, Json }
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Disposition`
import org.http4s.implicits._
import org.http4s.multipart.{ Multipart, Part }
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import org.typelevel.ci.CIString
import zio.Exit.Failure
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.duration.Duration
import zio.interop.catz.concurrentInstance
import zio.random.Random

import java.io.File
import java.nio.file.Path
import scala.util.Try

object Http4sAdapter {

  val `application/graphql`: MediaType = mediaType"application/graphql"

  private def parsePath(path: String): List[Either[String, Int]] =
    path.split('.').map(c => Try(c.toInt).toEither.left.map(_ => c)).toList

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

  private def executeToJsonWithUpload[R <: Has[_], E](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest,
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    queryExecution: QueryExecution,
    fileHandle: ZLayer[Any, Nothing, Uploads]
  ): URIO[R, Json] =
    interpreter
      .executeRequest(
        request,
        skipValidation = skipValidation,
        enableIntrospection = enableIntrospection,
        queryExecution
      )
      .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
      .provideSomeLayer[R](fileHandle)

  @deprecated("Use makeHttpService instead", "0.4.0")
  def makeRestService[R, E](interpreter: GraphQLInterpreter[R, E]): HttpRoutes[RIO[R, *]] =
    makeHttpService(interpreter)

  private def getGraphQLRequest(
    query: String,
    op: Option[String] = None,
    vars: Option[String] = None,
    exts: Option[String] = None
  ): Result[GraphQLRequest] = {
    val variablesJs  = vars.flatMap(parse(_).toOption)
    val extensionsJs = exts.flatMap(parse(_).toOption)
    val fields       = List("query" -> Json.fromString(query)) ++
      op.map(o => "operationName" -> Json.fromString(o)) ++
      variablesJs.map(js => "variables" -> js) ++
      extensionsJs.map(js => "extensions" -> js)
    Json
      .fromFields(fields)
      .as[GraphQLRequest]
  }

  private def getGraphQLRequest(params: Map[String, String]): Result[GraphQLRequest] =
    getGraphQLRequest(
      params.getOrElse("query", ""),
      params.get("operationName"),
      params.get("variables"),
      params.get("extensions")
    )

  private def parseGraphQLRequest(body: String): Result[GraphQLRequest] =
    parse(body).flatMap(_.as[GraphQLRequest]).leftMap(e => DecodingFailure(e.getMessage, Nil))

  private def parsePaths(map: Map[String, Seq[String]]): List[(String, List[Either[String, Int]])] =
    map.map { case (k, v) => k -> v.map(parsePath).toList }.toList.flatMap(kv => kv._2.map(kv._1 -> _))

  def makeHttpUploadService[R <: Has[_] with Random with Clock with Blocking, E](
    interpreter: GraphQLInterpreter[R, E],
    rootUploadPath: Path,
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): HttpRoutes[RIO[R, *]] = {
    object dsl extends Http4sDsl[RIO[R, *]]
    import dsl._

    HttpRoutes.of[RIO[R, *]] {
      case req @ POST -> Root if req.contentType.exists(_.mediaType.isMultipart) =>
        import zio.interop.catz.asyncInstance
        def getFileRefs(
          parts: Vector[Part[RIO[R, *]]]
        )(random: Random.Service): RIO[R, Map[String, (File, Part[RIO[R, *]])]] =
          parts
            .filter(_.headers.headers.exists(_.value.contains("filename")))
            .traverse { p =>
              p.name.traverse { n =>
                random.nextUUID.flatMap { uuid =>
                  val path = rootUploadPath.resolve(uuid.toString)
                  p.body
                    .through(Files[RIO[R, *]].writeAll(fs2.io.file.Path.fromNioPath(path)))
                    .compile
                    .drain
                    .as((n, path.toFile -> p))
                }
              }
            }
            .map(_.flatten.toMap)

        def getUploadQuery(
          operations: GraphQLRequest,
          map: Map[String, Seq[String]],
          parts: Vector[Part[RIO[R, *]]]
        )(random: Random.Service): RIO[R, GraphQLUploadRequest] = {
          val fileRefs  = getFileRefs(parts)(random)
          val filePaths = parsePaths(map)

          fileRefs.map { fileRef =>
            def handler(handle: String): UIO[Option[FileMeta]] =
              fileRef
                .get(handle)
                .traverse { case (file, fp) =>
                  random.nextUUID.asSomeError
                    .map(uuid =>
                      FileMeta(
                        uuid.toString,
                        file.getAbsoluteFile.toPath,
                        fp.headers.get[`Content-Disposition`].map(_.dispositionType),
                        fp.contentType.map { ct =>
                          val mt = ct.mediaType
                          s"${mt.mainType}/${mt.subType}"
                        },
                        fp.filename.getOrElse(file.getName),
                        file.length
                      )
                    )
                    .optional
                }
                .map(_.flatten)

            GraphQLUploadRequest(
              operations,
              filePaths,
              Uploads.handler(handler)
            )
          }
        }

        req.decode[Multipart[RIO[R, *]]] { m =>
          // First bit is always a standard graphql payload, it comes from the `operations` field
          val optOperations =
            m.parts.find(_.name.contains("operations")).traverse {
              _.body
                .through(utf8.decode)
                .compile
                .foldMonoid
                .flatMap(body => Task.fromEither(parseGraphQLRequest(body)))
            }

          // Second bit is the mapping field
          val optMap =
            m.parts.find(_.name.contains("map")).traverse {
              _.body
                .through(utf8.decode)
                .compile
                .foldMonoid
                .flatMap { body =>
                  Task.fromEither(
                    parse(body)
                      .flatMap(_.as[Map[String, Seq[String]]])
                      .leftMap(msg => msg.fillInStackTrace())
                  )
                }
            }

          for {
            ooperations <- optOperations
            omap        <- optMap
            random      <- ZIO.service[Random.Service]
            result      <- (ooperations, omap) match {
                             case (Some(operations), Some(map)) =>
                               for {
                                 query           <- getUploadQuery(operations, map, m.parts)(random)
                                 queryWithTracing =
                                   req.headers.headers
                                     .find(r =>
                                       r.name == CIString(
                                         GraphQLRequest.`apollo-federation-include-trace`
                                       ) && r.value == GraphQLRequest.ftv1
                                     )
                                     .foldLeft(query.remap)((q, _) => q.withFederatedTracing)

                                 result   <- executeToJsonWithUpload(
                                               interpreter,
                                               queryWithTracing,
                                               skipValidation = skipValidation,
                                               enableIntrospection = enableIntrospection,
                                               queryExecution,
                                               query.fileHandle.toLayerMany
                                             )
                                 response <- Ok(result)
                               } yield response

                             case (None, _) => BadRequest("Missing multipart field 'operations'")
                             case (_, None) => BadRequest("Missing multipart field 'map'")
                           }
          } yield result
        }
    }
  }

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): HttpRoutes[RIO[R, *]] = {
    object dsl extends Http4sDsl[RIO[R, *]]
    import dsl._

    HttpRoutes.of[RIO[R, *]] {
      case req @ POST -> Root =>
        for {
          query           <- if (req.params.contains("query"))
                               Task.fromEither(getGraphQLRequest(req.params))
                             else if (req.contentType.exists(_.mediaType == `application/graphql`))
                               for {
                                 body   <- req.attemptAs[String](EntityDecoder.text).value.absolve
                                 parsed <- Task.fromEither(getGraphQLRequest(body))
                               } yield parsed
                             else
                               req.attemptAs[GraphQLRequest].value.absolve
          queryWithTracing =
            req.headers.headers
              .find(r =>
                r.name == CIString(GraphQLRequest.`apollo-federation-include-trace`) && r.value == GraphQLRequest.ftv1
              )
              .foldLeft(query)((q, _) => q.withFederatedTracing)
          result          <- executeToJson(
                               interpreter,
                               queryWithTracing,
                               skipValidation = skipValidation,
                               enableIntrospection = enableIntrospection,
                               queryExecution
                             )
          response        <- Ok(result)
        } yield response
      case req @ GET -> Root  =>
        for {
          query    <- Task.fromEither(getGraphQLRequest(req.params))
          result   <- executeToJson(
                        interpreter,
                        query,
                        skipValidation = skipValidation,
                        enableIntrospection = enableIntrospection,
                        queryExecution
                      )
          response <- Ok(result)
        } yield response
    }
  }

  def executeRequest[R0, R, E](
    interpreter: GraphQLInterpreter[R, E],
    provideEnv: R0 => R,
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): HttpApp[RIO[R0, *]] =
    Kleisli { req =>
      object dsl extends Http4sDsl[RIO[R0, *]]
      import dsl._
      for {
        query    <- req.attemptAs[GraphQLRequest].value.absolve
        result   <- executeToJson(
                      interpreter,
                      query,
                      skipValidation = skipValidation,
                      enableIntrospection = enableIntrospection,
                      queryExecution
                    ).provideSome[R0](provideEnv)
        response <- Ok(result)
      } yield response
    }

  def makeWebSocketService[R, E](
    builder: WebSocketBuilder2[RIO[R, *]],
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): HttpRoutes[RIO[R, *]] = {

    object dsl extends Http4sDsl[RIO[R, *]]
    import dsl._

    def sendMessage(
      sendQueue: CatsQueue[Task, WebSocketFrame],
      messageType: String,
      id: String,
      payload: Json
    ): RIO[R, Unit] =
      sendQueue.offer(
        WebSocketFrame.Text(
          Json
            .obj(
              "id"      -> Json.fromString(id),
              "type"    -> Json.fromString(messageType),
              "payload" -> payload
            )
            .noSpaces
        )
      )

    def processMessage(
      receivingQueue: CatsQueue[Task, WebSocketFrame],
      sendQueue: CatsQueue[Task, WebSocketFrame],
      subscriptions: Ref[Map[String, Fiber[Throwable, Unit]]]
    ): RIO[R, Unit] =
      Stream
        .repeatEval(receivingQueue.take)
        .collect { case Text(text, _) => text }
        .flatMap { text =>
          Stream.eval {
            for {
              msg    <- RIO.fromEither(decode[Json](text))
              msgType = msg.hcursor.downField("type").success.flatMap(_.value.asString).getOrElse("")
              _      <- RIO.whenCase[R, String](msgType) {
                          case "connection_init"      =>
                            sendQueue.offer(WebSocketFrame.Text("""{"type":"connection_ack"}""")) *>
                              RIO.whenCase(keepAliveTime) { case Some(time) =>
                                // Save the keep-alive fiber with a key of None so that it's interrupted later
                                sendQueue
                                  .offer(WebSocketFrame.Text("""{"type":"ka"}"""))
                                  .repeat(Schedule.spaced(time))
                                  .provideLayer(Clock.live)
                                  .unit
                                  .fork
                              }
                          case "connection_terminate" => RIO.fromEither(WebSocketFrame.Close(1000)) >>= sendQueue.offer
                          case "start"                =>
                            val payload = msg.hcursor.downField("payload")
                            val id      = msg.hcursor.downField("id").success.flatMap(_.value.asString).getOrElse("")
                            RIO.whenCase(payload.as[GraphQLRequest]) { case Right(req) =>
                              (for {
                                result <- interpreter.executeRequest(
                                            req,
                                            skipValidation = skipValidation,
                                            enableIntrospection = enableIntrospection,
                                            queryExecution
                                          )
                                _      <- result.data match {
                                            case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                                              stream.foreach { item =>
                                                sendMessage(
                                                  sendQueue,
                                                  "data",
                                                  id,
                                                  GraphQLResponse(ObjectValue(List(fieldName -> item)), result.errors).asJson
                                                )
                                              }.onExit {
                                                case Failure(cause) if !cause.interrupted =>
                                                  sendMessage(
                                                    sendQueue,
                                                    "error",
                                                    id,
                                                    Json.obj("message" -> Json.fromString(cause.squash.toString))
                                                  ).orDie
                                                case _                                    =>
                                                  sendQueue
                                                    .offer(WebSocketFrame.Text(s"""{"type":"complete","id":"$id"}"""))
                                                    .orDie
                                              }.fork
                                                .flatMap(fiber => subscriptions.update(_.updated(id, fiber)))
                                            case other                                                =>
                                              sendMessage(sendQueue, "data", id, GraphQLResponse(other, result.errors).asJson) *>
                                                sendQueue.offer(WebSocketFrame.Text(s"""{"type":"complete","id":"$id"}"""))
                                          }
                              } yield ()).catchAll(error =>
                                sendMessage(sendQueue, "error", id, Json.obj("message" -> Json.fromString(error.toString)))
                              )
                            }
                          case "stop"                 =>
                            val id = msg.hcursor.downField("id").success.flatMap(_.value.asString).getOrElse("")
                            subscriptions
                              .modify(map => (map.get(id), map - id))
                              .flatMap(fiber =>
                                IO.whenCase(fiber) { case Some(fiber) =>
                                  fiber.interrupt
                                }
                              )
                        }
            } yield ()
          }
        }
        .compile
        .drain

    def passThroughPipe(
      receivingQueue: CatsQueue[Task, WebSocketFrame]
    ): Pipe[RIO[R, *], WebSocketFrame, Unit] = _.evalMap(receivingQueue.offer)

    HttpRoutes.of[RIO[R, *]] { case GET -> Root =>
      for {
        receivingQueue      <- CatsQueue.unbounded[Task, WebSocketFrame]
        sendQueue           <- CatsQueue.unbounded[Task, WebSocketFrame]
        subscriptions       <- Ref.make(Map.empty[String, Fiber[Throwable, Unit]])
        // We provide fiber to process messages, which inherits the context of WebSocket connection request,
        // so that we can pass information available at connection request, such as authentication information,
        // to execution of subscription.
        processMessageFiber <- processMessage(receivingQueue, sendQueue, subscriptions).forkDaemon
        builder             <- builder
                                 .withHeaders(Headers(Header.Raw(CIString("Sec-WebSocket-Protocol"), "graphql-ws")))
                                 .withOnClose(processMessageFiber.interrupt.unit)
                                 .build(Stream.repeatEval(sendQueue.take), passThroughPipe(receivingQueue))
      } yield builder
    }
  }

  /**
   * Utility function to create an http4s middleware that can extracts something from each request
   * and provide a layer to eliminate the ZIO environment
   * @param route an http4s route
   * @param f a function from a request to a ZLayer
   * @tparam R the environment type to eliminate
   * @return a new route without the R requirement
   */
  def provideLayerFromRequest[R <: Has[_]](route: HttpRoutes[RIO[R, *]], f: Request[Task] => TaskLayer[R])(implicit
    tagged: Tag[R]
  ): HttpRoutes[Task] =
    Kleisli { (req: Request[Task[*]]) =>
      val to: Task ~> RIO[R, *] = new (Task ~> RIO[R, *]) {
        def apply[A](fa: Task[A]): RIO[R, A] = fa
      }

      val from: RIO[R, *] ~> Task = new (RIO[R, *] ~> Task) {
        def apply[A](fa: RIO[R, A]): Task[A] = fa.provideLayer(f(req))
      }

      route(req.mapK(to)).mapK(from).map(_.mapK(from))
    }

  /**
   * Utility function to create an http4s middleware that can extracts something from each request
   * and provide a layer to eliminate some part of the ZIO environment
   * @param route an http4s route
   * @param f a function from a request to a ZLayer
   * @tparam R the remaining environment
   * @tparam R1 the environment to eliminate
   * @return a new route that requires only R
   */
  def provideSomeLayerFromRequest[R <: Has[_], R1 <: Has[_]](
    route: HttpRoutes[RIO[R with R1, *]],
    f: Request[RIO[R, *]] => RLayer[R, R1]
  )(implicit tagged: Tag[R1]): HttpRoutes[RIO[R, *]] =
    Kleisli { (req: Request[RIO[R, *]]) =>
      val to: RIO[R, *] ~> RIO[R with R1, *] = new (RIO[R, *] ~> RIO[R with R1, *]) {
        def apply[A](fa: RIO[R, A]): RIO[R with R1, A] = fa
      }

      val from: RIO[R with R1, *] ~> RIO[R, *] = new (RIO[R with R1, *] ~> RIO[R, *]) {
        def apply[A](fa: RIO[R with R1, A]): RIO[R, A] = fa.provideSomeLayer[R](f(req))
      }

      route(req.mapK(to)).mapK(from).map(_.mapK(from))
    }

  private def wrapRoute[F[_]: Async, R](
    route: HttpRoutes[RIO[R, *]]
  )(implicit dispatcher: Dispatcher[F], runtime: Runtime[R]): HttpRoutes[F] = {
    val toF: RIO[R, *] ~> F   = CatsInterop.toEffectK
    val toRIO: F ~> RIO[R, *] = CatsInterop.fromEffectK

    val to: OptionT[RIO[R, *], *] ~> OptionT[F, *] = new (OptionT[RIO[R, *], *] ~> OptionT[F, *]) {
      def apply[A](fa: OptionT[RIO[R, *], A]): OptionT[F, A] = fa.mapK(toF)
    }

    route
      .mapK(to)
      .dimap((req: Request[F]) => req.mapK(toRIO))((res: Response[RIO[R, *]]) => res.mapK(toF))
  }

  private def wrapApp[F[_]: Async, R](
    app: HttpApp[RIO[R, *]]
  )(implicit dispatcher: Dispatcher[F], runtime: Runtime[R]): HttpApp[F] = {
    val toF: RIO[R, *] ~> F   = CatsInterop.toEffectK
    val toRIO: F ~> RIO[R, *] = CatsInterop.fromEffectK

    app
      .mapK(toF)
      .dimap((req: Request[F]) => req.mapK(toRIO))((res: Response[RIO[R, *]]) => res.mapK(toF))
  }

  def makeWebSocketServiceF[F[_], R, E](
    builder: WebSocketBuilder2[F],
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit F: Async[F], dispatcher: Dispatcher[F], runtime: Runtime[R]): HttpRoutes[F] =
    wrapRoute(
      makeWebSocketService[R, E](
        builder.imapK[RIO[R, *]](CatsInterop.fromEffectK)(CatsInterop.toEffectK),
        interpreter,
        skipValidation = skipValidation,
        enableIntrospection = enableIntrospection,
        keepAliveTime,
        queryExecution
      )
    )

  @deprecated("Use makeHttpServiceF instead", "0.4.0")
  def makeRestServiceF[F[_], E](
    interpreter: GraphQLInterpreter[Any, E]
  )(implicit F: Async[F], dispatcher: Dispatcher[F], runtime: Runtime[Any]): HttpRoutes[F] =
    makeHttpServiceF(interpreter)

  def makeHttpServiceF[F[_], R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit F: Async[F], dispatcher: Dispatcher[F], runtime: Runtime[R]): HttpRoutes[F] =
    wrapRoute(
      makeHttpService[R, E](
        interpreter,
        skipValidation = skipValidation,
        enableIntrospection = enableIntrospection,
        queryExecution
      )
    )

  def executeRequestF[F[_], R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit F: Async[F], dispatcher: Dispatcher[F], runtime: Runtime[R]): HttpApp[F] =
    wrapApp(
      executeRequest[R, R, E](
        interpreter,
        identity,
        skipValidation = skipValidation,
        enableIntrospection = enableIntrospection,
        queryExecution
      )
    )
}

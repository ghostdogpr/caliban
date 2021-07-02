package caliban

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import caliban.execution.QueryExecution
import caliban.uploads._
import cats.arrow.FunctionK
import cats.data.{ Kleisli, OptionT }
import cats.syntax.either._
import cats.syntax.traverse._
import cats.syntax.semigroupk._
import cats.effect.{ Blocker, Effect }
import cats.effect.syntax.all._
import cats.~>
import fs2.{ Pipe, Stream }
import fs2.text.utf8Decode
import io.circe.Decoder.Result
import io.circe.Json
import io.circe.parser._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Disposition`
import org.http4s.implicits._
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import org.http4s.multipart.{ Multipart, Part }
import zio.Exit.Failure
import zio._
import zio.clock.Clock
import zio.duration.Duration
import zio.interop.catz._

import java.io.File
import java.nio.file.Paths
import java.util.UUID
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

  def makeHttpServiceWithUpload[R <: Has[_], E](
    interpreter: GraphQLInterpreter[R, E],
    rootUploadPath: String,
    blocker: Blocker,
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): HttpRoutes[RIO[R, *]] = {
    object dsl extends Http4sDsl[RIO[R, *]]
    import dsl._

    HttpRoutes.of[RIO[R, *]] {
      case req @ POST -> Root if req.contentType.exists(_.mediaType.isMultipart) =>
        def filterFileTypes(part: Part[RIO[R, *]]): Boolean = part.headers.exists(_.value.contains("filename"))

        req.decode[Multipart[RIO[R, *]]] { m =>
          // First bit is always a standard graphql payload, it comes from the `operations` field
          val optOperations =
            m.parts.find(_.name.contains("operations")).traverse {
              _.body
                .through(utf8Decode)
                .compile
                .foldMonoid
                .flatMap(body => Task.fromEither(getGraphQLRequest(body)))
            }

          // Second bit is the mapping field
          val optMap =
            m.parts.find(_.name.contains("map")).traverse {
              _.body
                .through(utf8Decode)
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
            result      <- (ooperations, omap) match {
                             case (Some(operations), Some(map)) =>
                               val filePaths =
                                 map.map { case (key, value) => (key, value.map(parsePath).toList) }.toList.flatMap(kv =>
                                   kv._2.map(kv._1 -> _)
                                 )

                               val fileRefs =
                                 m.parts
                                   .filter(filterFileTypes)
                                   .traverse { p =>
                                     p.name.traverse { n =>
                                       val path = s"$rootUploadPath/${UUID.randomUUID()}"
                                       p.body
                                         .through(
                                           fs2.io.file.writeAll(
                                             Paths.get(path),
                                             blocker
                                           )
                                         )
                                         .compile
                                         .foldMonoid
                                         .as((n, new File(path) -> p))
                                     }
                                   }
                                   .map(_.flatten.toMap)

                               val uploadQuery = for {
                                 fileRef <- fileRefs
                               } yield {
                                 def handler(handle: String): UIO[Option[FileMeta]] =
                                   fileRef
                                     .get(handle)
                                     .traverse { case (file, fp) =>
                                       ZIO.succeed(
                                         FileMeta(
                                           UUID.randomUUID().toString,
                                           file.getAbsoluteFile.toPath,
                                           fp.headers.get(`Content-Disposition`).map(_.dispositionType),
                                           fp.contentType.map { ct =>
                                             val mt = ct.mediaType
                                             s"${mt.mainType}/${mt.subType}"
                                           },
                                           fp.filename.getOrElse(file.getName),
                                           file.length
                                         )
                                       )
                                     }

                                 GraphQLUploadRequest(
                                   operations,
                                   filePaths,
                                   Uploads.handler(handler)
                                 )
                               }

                               for {
                                 query           <- uploadQuery
                                 queryWithTracing =
                                   req.headers
                                     .find(r =>
                                       r.name == GraphQLRequest.`apollo-federation-include-trace` && r.value == GraphQLRequest.ftv1
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
    } <+> makeHttpService(interpreter, skipValidation, enableIntrospection, queryExecution)
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
            req.headers
              .find(r => r.name == GraphQLRequest.`apollo-federation-include-trace` && r.value == GraphQLRequest.ftv1)
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
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): HttpRoutes[RIO[R, *]] = {

    object dsl extends Http4sDsl[RIO[R, *]]
    import dsl._

    def sendMessage(
      sendQueue: fs2.concurrent.Queue[Task, WebSocketFrame],
      messageType: String,
      id: String,
      payload: Json
    ): RIO[R, Unit] =
      sendQueue.enqueue1(
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
      receivingQueue: fs2.concurrent.Queue[Task, WebSocketFrame],
      sendQueue: fs2.concurrent.Queue[Task, WebSocketFrame],
      subscriptions: Ref[Map[String, Fiber[Throwable, Unit]]]
    ) =
      receivingQueue.dequeue.collect { case Text(text, _) => text }.flatMap { text =>
        Stream.eval {
          for {
            msg    <- Task.fromEither(decode[Json](text))
            msgType = msg.hcursor.downField("type").success.flatMap(_.value.asString).getOrElse("")
            _      <- RIO.whenCase(msgType) {
                        case "connection_init"      =>
                          sendQueue.enqueue1(WebSocketFrame.Text("""{"type":"connection_ack"}""")) *>
                            Task.whenCase(keepAliveTime) { case Some(time) =>
                              // Save the keep-alive fiber with a key of None so that it's interrupted later
                              sendQueue
                                .enqueue1(WebSocketFrame.Text("""{"type":"ka"}"""))
                                .repeat(Schedule.spaced(time))
                                .provideLayer(Clock.live)
                                .unit
                                .fork
                            }
                        case "connection_terminate" => Task.fromEither(WebSocketFrame.Close(1000)) >>= sendQueue.enqueue1
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
                                                  .enqueue1(WebSocketFrame.Text(s"""{"type":"complete","id":"$id"}"""))
                                                  .orDie
                                            }.fork
                                              .flatMap(fiber => subscriptions.update(_.updated(id, fiber)))
                                          case other                                                =>
                                            sendMessage(sendQueue, "data", id, GraphQLResponse(other, result.errors).asJson) *>
                                              sendQueue.enqueue1(WebSocketFrame.Text(s"""{"type":"complete","id":"$id"}"""))
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
      }.compile.drain

    def passThroughPipe(
      receivingQueue: fs2.concurrent.Queue[Task, WebSocketFrame]
    ): Pipe[RIO[R, *], WebSocketFrame, Unit] = _.evalMap(receivingQueue.enqueue1)

    HttpRoutes.of[RIO[R, *]] { case GET -> Root =>
      for {
        receivingQueue      <- fs2.concurrent.Queue.unbounded[Task, WebSocketFrame]
        sendQueue           <- fs2.concurrent.Queue.unbounded[Task, WebSocketFrame]
        subscriptions       <- Ref.make(Map.empty[String, Fiber[Throwable, Unit]])
        // We provide fiber to process messages, which inherits the context of WebSocket connection request,
        // so that we can pass information available at connection request, such as authentication information,
        // to execution of subscription.
        processMessageFiber <- processMessage(receivingQueue, sendQueue, subscriptions).forkDaemon
        builder             <- WebSocketBuilder[RIO[R, *]].build(
                                 sendQueue.dequeue,
                                 passThroughPipe(receivingQueue),
                                 headers = Headers.of(Header("Sec-WebSocket-Protocol", "graphql-ws")),
                                 onClose = processMessageFiber.interrupt.unit
                               )
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
    Kleisli { req: Request[Task[*]] =>
      val to: Task ~> RIO[R, *]   = FunctionK.lift[Task, RIO[R, *]](identity)
      val from: RIO[R, *] ~> Task = λ[FunctionK[RIO[R, *], Task]](_.provideLayer(f(req)))
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
    Kleisli { req: Request[RIO[R, *]] =>
      val to: RIO[R, *] ~> RIO[R with R1, *]   = FunctionK.lift[RIO[R, *], RIO[R with R1, *]](identity)
      val from: RIO[R with R1, *] ~> RIO[R, *] =
        λ[FunctionK[RIO[R with R1, *], RIO[R, *]]](_.provideSomeLayer[R](f(req)))
      route(req.mapK(to)).mapK(from).map(_.mapK(from))
    }

  private def wrapRoute[F[_]: Effect](route: HttpRoutes[Task])(implicit runtime: Runtime[Any]): HttpRoutes[F] = {
    val toF: Task ~> F    = λ[Task ~> F](_.toIO.to[F])
    val toTask: F ~> Task = λ[F ~> Task](_.toIO.to[Task])

    route
      .mapK(λ[OptionT[Task, *] ~> OptionT[F, *]](_.mapK(toF)))
      .dimap((req: Request[F]) => req.mapK(toTask))((res: Response[Task]) => res.mapK(toF))
  }

  private def wrapApp[F[_]: Effect](app: HttpApp[Task])(implicit runtime: Runtime[Any]): HttpApp[F] = {
    val toF: Task ~> F    = λ[Task ~> F](_.toIO.to[F])
    val toTask: F ~> Task = λ[F ~> Task](_.toIO.to[Task])

    app
      .mapK(toF)
      .dimap((req: Request[F]) => req.mapK(toTask))((res: Response[Task]) => res.mapK(toF))
  }

  def makeWebSocketServiceF[F[_], E](
    interpreter: GraphQLInterpreter[Any, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpRoutes[F] =
    wrapRoute(
      makeWebSocketService[Any, E](
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
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpRoutes[F] =
    makeHttpServiceF(interpreter)

  def makeHttpServiceF[F[_], E](
    interpreter: GraphQLInterpreter[Any, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpRoutes[F] =
    wrapRoute(
      makeHttpService[Any, E](
        interpreter,
        skipValidation = skipValidation,
        enableIntrospection = enableIntrospection,
        queryExecution
      )
    )

  def executeRequestF[F[_], E](
    interpreter: GraphQLInterpreter[Any, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit F: Effect[F], runtime: Runtime[Any]): HttpApp[F] =
    wrapApp(
      executeRequest[Any, Any, E](
        interpreter,
        identity,
        skipValidation = skipValidation,
        enableIntrospection = enableIntrospection,
        queryExecution
      )
    )
}

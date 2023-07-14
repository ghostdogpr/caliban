package caliban.interop.tapir

import caliban.InputValue.ObjectValue
import caliban.Value.StringValue
import caliban._
import sttp.capabilities.zio.ZioStreams
import sttp.capabilities.{ Effect, WebSockets }
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import sttp.client3.{ BasicRequestBody, DeserializationException, HttpError, ResponseException, SttpBackend }
import sttp.model._
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.client.sttp.SttpClientInterpreter
import sttp.tapir.client.sttp.ws.zio._
import sttp.tapir.model.{ ConnectionInfo, ServerRequest }
import sttp.tapir.{ AttributeKey, DecodeResult }
import zio.json._
import zio.stream.{ ZPipeline, ZSink, ZStream }
import zio.test.TestAspect.before
import zio.test._
import zio.{ test => _, _ }

import scala.language.postfixOps

object TapirAdapterSpec {
  case class FakeServerRequest(method: Method, uri: Uri, headers: List[Header] = Nil) extends ServerRequest {
    override def protocol: String = "http"

    override def connectionInfo: ConnectionInfo = ConnectionInfo.NoInfo

    override def underlying: Any = ()

    override def pathSegments: List[String] =
      uri.pathSegments.segments.map(_.v).toList

    override def queryParameters: QueryParams = uri.params

    override def attribute[T](k: AttributeKey[T]): Option[T]           = None
    override def attribute[T](k: AttributeKey[T], v: T): ServerRequest = this

    override def withUnderlying(underlying: Any): ServerRequest = this
  }

  private def customError: CustomAssertion[ResponseException[String, String], String] = CustomAssertion.make {
    case HttpError(body, _)                 => Right(body)
    case DeserializationException(_, error) => Right(error)
  }

  def makeSuite(
    label: String,
    httpUri: Uri,
    uploadUri: Option[Uri] = None,
    wsUri: Option[Uri] = None
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[CalibanError]],
    wsInputCodec: JsonCodec[GraphQLWSInput],
    wsOutputCodec: JsonCodec[GraphQLWSOutput]
  ): Spec[TestService, Throwable] = suite(label) {
    val httpClient   = new TapirClient(httpUri)
    val uploadClient = uploadUri.map(new TapirClient(_))
    val run          = (request: GraphQLRequest) => httpClient.runPost(request)
    val runUpload    = uploadClient.map(client => (request: List[Part[BasicRequestBody]]) => client.runUpload(request))
    val runWS        = wsUri.map(wsUri =>
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(WebSocketInterpreter.makeWebSocketEndpoint, Some(wsUri))
    )

    val tests: List[Option[Spec[SttpBackend[Task, ZioStreams with WebSockets], Throwable]]] = List(
      Some(
        suite("http")(
          test("test POST http endpoint") {
            for {
              res      <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](
                            run(GraphQLRequest(Some("{ characters { name }  }"))).send(_)
                          )
              response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
            } yield assertTrue(
              res.contentType.contains(MediaType.ApplicationJson.toString()),
              response.is(_.left).data.toString ==
                """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
            )
          },
          test("test interceptor failure") {
            for {
              res      <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](
                            run(GraphQLRequest(Some("{ characters { name }  }"))).header("X-Invalid", "1").send(_)
                          )
              response <- ZIO.fromEither(res.body).flip.orElseFail(new Throwable("Failed to parse result"))
            } yield assertTrue(
              res.code == StatusCode.Unauthorized,
              response.is(_.custom(customError)) == "You are unauthorized!"
            )
          },
          test("lower-case content-type header") {
            val q = "{ characters { name }  }"
            val r = run(GraphQLRequest())
              .header(Header("content-type", "application/graphql; charset=utf-8"), replaceExisting = true)
              .body(q)
              // if we don't set content-length here it gets incorrectly set to 70 rather than 24.
              .contentLength(q.length)

            for {
              res  <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](r.send(_))
              body <- ZIO.fromEither(res.body).orElseFail(new Throwable(s"Failed to parse result: $res"))
            } yield assertTrue(
              body.is(_.left).data.toString ==
                """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
            )
          },
          test("don't defer pure values") {
            val q = """{ characters { ... on Character @defer(label: "character") { name nicknames } } }"""
            val r = run(GraphQLRequest())
              .header(Header("content-type", "application/graphql; charset=utf-8"), replaceExisting = true)
              .body(q)
              // if we don't set content-length here it gets incorrectly set to 70 rather than 24.
              .contentLength(q.length)

            for {
              res  <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](r.send(_))
              body <- ZIO.fromEither(res.body).orElseFail(new Throwable(s"Failed to parse result: $res"))
            } yield assertTrue(body.isLeft)
          },
          test("allow deferred effects") {
            val q =
              """{ characters { ... on Character @defer(label: "character") { name  ... @defer(label: "nicknames") { labels }  } } }"""
            val r = run(GraphQLRequest())
              .header(Header("content-type", "application/graphql; charset=utf-8"), replaceExisting = true)
              .body(q)
              .contentLength(q.length)

            for {
              res  <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](r.send(_))
              body <-
                ZIO.fromEither(res.body).mapError(e => new Throwable(s"Failed to parse result: $res, ${e.getMessage}"))
            } yield assertTrue(body.isRight, body.toOption.exists(_.size == 9))
          }
        )
      ),
      runUpload.map(runUpload =>
        suite("uploads")(
          test("test http upload endpoint") {
            val query =
              """{ "query": "mutation ($files: [Upload!]!) { uploadFiles(files: $files) { hash, filename, mimetype } }", "variables": { "files": [null, null] }}"""

            val parts: List[Part[BasicRequestBody]] = List(
              sttp.client3.multipart("operations", query.getBytes).contentType(MediaType.ApplicationJson),
              sttp.client3.multipart("map", """{ "0": ["variables.files.0"], "1":  ["variables.files.1"]}""".getBytes),
              sttp.client3.multipart("0", """image""".getBytes).contentType(MediaType.ImagePng).fileName("a.png"),
              sttp.client3.multipart("1", """text""".getBytes).contentType(MediaType.TextPlain).fileName("a.txt")
            )

            for {
              res  <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](runUpload(parts).send(_))
              body <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
            } yield assertTrue(
              body.is(_.left).data.toString ==
                """{"uploadFiles":[{"hash":"6105d6cc76af400325e94d588ce511be5bfdbb73b437dc51eca43917d7a43e3d","filename":"a.png","mimetype":"image/png"},{"hash":"982d9e3eb996f559e633f4d194def3761d909f5a3b647d1a851fead67c32c9d1","filename":"a.txt","mimetype":"text/plain"}]}"""
            )
          },
          test("test http upload endpoint for extra fields") {
            val query =
              """{ "query": "mutation ($uploadedDocuments: [UploadedDocumentInput!]!) { uploadFilesWithExtraFields(uploadedDocuments: $uploadedDocuments) { someField1, someField2} }", "variables": { "uploadedDocuments": [{"file": null, "someField1": 1, "someField2": 2}, {"file": null, "someField1": 3}] }}"""

            val parts = List(
              sttp.client3.multipart("operations", query.getBytes).contentType(MediaType.ApplicationJson),
              sttp.client3.multipart(
                "map",
                """{ "0": ["variables.uploadedDocuments.0.file"], "1":  ["variables.uploadedDocuments.1.file"]}""".getBytes
              ),
              sttp.client3.multipart("0", """image""".getBytes).contentType(MediaType.ImagePng).fileName("a.png"),
              sttp.client3.multipart("1", """text""".getBytes).contentType(MediaType.TextPlain).fileName("a.txt")
            )

            for {
              res  <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](runUpload(parts).send(_))
              body <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
            } yield assertTrue(
              body.is(_.left).data.toString ==
                """{"uploadFilesWithExtraFields":[{"someField1":1,"someField2":2},{"someField1":3,"someField2":null}]}"""
            )
          }
        )
      ),
      runWS.map(runWS =>
        suite("test ws endpoint")(
          test("legacy ws") {
            import caliban.interop.tapir.ws.Protocol.Legacy.Ops
            val io =
              for {
                res         <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](
                                 runWS(
                                   FakeServerRequest(
                                     Method.GET,
                                     Uri.unsafeParse("http://localhost:80/ws/graphql"),
                                     Header("Sec-WebSocket-Protocol", "graphql-ws") :: Nil
                                   ) -> "graphql-ws"
                                 ).send(_)
                               )
                res         <- ZIO.fromEither(res.body).mapError(cause => new Throwable(s"Failed to parse result $cause"))
                (_, pipe)    = res
                inputQueue  <- Queue.unbounded[GraphQLWSInput]
                inputStream  = ZStream.fromQueueWithShutdown(inputQueue)
                outputStream = pipe(inputStream)
                _           <- inputQueue.offer(GraphQLWSInput(Ops.ConnectionInit, None, None))
                _           <- inputQueue.offer(
                                 GraphQLWSInput(
                                   Ops.Start,
                                   Some("id"),
                                   Some(ObjectValue(Map("query" -> StringValue("subscription { characterDeleted }"))))
                                 )
                               )
                sendDelete   = Live.live {
                                 ZIO
                                   .serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](
                                     run(
                                       GraphQLRequest(Some("""mutation{ deleteCharacter(name: "Amos Burton") }"""))
                                     ).send(_)
                                   )
                                   .delay(3 seconds)
                               }
                stop         = inputQueue.offer(GraphQLWSInput(Ops.Stop, Some("id"), None))
                messages    <- outputStream
                                 .tap(out =>
                                   ZIO.whenCase(out) {
                                     case Right(out) if out.`type` == Ops.ConnectionAck => sendDelete
                                     case Right(out) if out.`type` == Ops.Data          => stop
                                   }
                                 )
                                 .take(3)
                                 .runCollect
              } yield messages

            io.map(_.collect { case Right(value) => value }).map { messages =>
              assertTrue(
                messages.head.`type` == Ops.ConnectionAck,
                messages(1).payload.get.toString == """{"data":{"characterDeleted":"Amos Burton"}}""",
                messages(2).`type` == Ops.Complete
              )
            }
          } @@ TestAspect.timeout(60.seconds),
          test("graphql-ws") {
            import caliban.interop.tapir.ws.Protocol.GraphQLWS.Ops
            val io =
              for {
                res         <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](
                                 runWS(
                                   FakeServerRequest(
                                     Method.GET,
                                     Uri.unsafeParse("http://localhost:80/ws/graphql"),
                                     Header("Sec-WebSocket-Protocol", "graphql-transport-ws") :: Nil
                                   ) -> "graphql-transport-ws"
                                 ).send(_)
                               )
                res         <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
                (_, pipe)    = res
                inputQueue  <- Queue.unbounded[GraphQLWSInput]
                inputStream  = ZStream.fromQueueWithShutdown(inputQueue)
                outputStream = pipe(inputStream)
                _           <- inputQueue.offer(GraphQLWSInput(Ops.ConnectionInit, None, None))
                _           <- inputQueue.offer(
                                 GraphQLWSInput(
                                   Ops.Subscribe,
                                   Some("id"),
                                   Some(ObjectValue(Map("query" -> StringValue("subscription { characterDeleted }"))))
                                 )
                               )
                sendDelete   = Live.live {
                                 ZIO
                                   .serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](
                                     run(
                                       GraphQLRequest(Some("""mutation{ deleteCharacter(name: "Amos Burton") }"""))
                                     ).send(_)
                                   )
                                   .delay(3 seconds)
                               }
                stop         = inputQueue.offer(GraphQLWSInput(Ops.Complete, Some("id"), None))
                ping         = inputQueue.offer(GraphQLWSInput(Ops.Ping, Some("id"), None))
                messages    <- outputStream
                                 .tap(out =>
                                   ZIO.whenCase(out) {
                                     case Right(out) if out.`type` == Ops.ConnectionAck => sendDelete
                                     case Right(out) if out.`type` == Ops.Next          => ping
                                     case Right(out) if out.`type` == Ops.Pong          => stop
                                   }
                                 )
                                 .take(4)
                                 .runCollect
              } yield messages

            io.map { messages =>
              assertTrue(
                messages.head.map(_.`type`) == Right(Ops.ConnectionAck),
                messages(1).map(_.payload.get.toString) == Right("""{"data":{"characterDeleted":"Amos Burton"}}"""),
                messages(2).map(_.`type`) == Right(Ops.Pong),
                messages(3).map(_.`type`) == Right(Ops.Complete)
              )
            }
          } @@ TestAspect.timeout(60.seconds)
        )
      )
    )

    ZIO.succeed(tests.flatten)
  }.provideLayerShared(AsyncHttpClientZioBackend.layer()) @@
    before(TestService.reset) @@
    TestAspect.sequential

  private class TapirClient(httpUri: Uri)(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[CalibanError]]
  ) {
    import sttp.client3._

    implicit def jsonBodySerializer[B](implicit encoder: JsonCodec[B]): BodySerializer[B] =
      b => StringBody(encoder.encode(b), "UTF-8", MediaType.ApplicationJson)

    implicit val stringShows: ShowError[String] = (error: String) => error

    def runPost(request: GraphQLRequest): Request[Either[ResponseException[String, String], Either[GraphQLResponse[
      CalibanError
    ], Chunk[ResponseValue]]], Effect[Task] with ZioStreams] =
      basicRequest
        .post(httpUri)
        .body(request)
        .response(asStreamOrSingle)

    def runGet(request: GraphQLRequest): Request[Either[ResponseException[String, String], Either[GraphQLResponse[
      CalibanError
    ], Chunk[ResponseValue]]], Effect[Task] with ZioStreams] =
      basicRequest
        .get(
          httpUri.addParams(
            QueryParams.fromSeq(
              List(
                request.query.map("query" -> _),
                request.operationName.map("operationName" -> _),
                request.variables.map("variables" -> _.toJson),
                request.extensions.map("extensions" -> _.toJson)
              ).flatten
            )
          )
        )
        .response(asStreamOrSingle)

    def runUpload(
      request: List[Part[BasicRequestBody]]
    ): Request[Either[ResponseException[String, String], Either[GraphQLResponse[CalibanError], Chunk[
      ResponseValue
    ]]], Effect[Task] with ZioStreams] =
      basicRequest
        .post(httpUri)
        .response(asStreamOrSingle)
        .multipartBody(request)

    private def readMultipartResponse(
      stream: ZStream[Any, Throwable, Byte]
    ): ZIO[Any, Throwable, Chunk[ResponseValue]] =
      (stream >>>
        ZPipeline.utfDecode >>>
        ZPipeline.splitLines >>>
        ZPipeline.map[String, String](_.trim) >>>
        ZPipeline.collect[String, Either[Throwable, ResponseValue]] {
          case line if line.startsWith("{") =>
            line.fromJson[ResponseValue].left.map(new Throwable(_))
        }).absolve >>> ZSink.collectAll[ResponseValue]

    private def asStreamOrSingle(implicit
      codec: JsonCodec[GraphQLResponse[CalibanError]]
    ): ResponseAs[Either[ResponseException[String, String], Either[GraphQLResponse[CalibanError], Chunk[
      ResponseValue
    ]]], Effect[Task] with ZioStreams] =
      fromMetadata(
        asStringAlways.map(error => Left(HttpError(error, StatusCode.UnprocessableEntity))),
        ConditionalResponseAs(
          _.contentType.exists(
            MediaType.unsafeParse(_).matches(ContentTypeRange("multipart", "mixed", ContentTypeRange.Wildcard))
          ),
          asStream(ZioStreams)(readMultipartResponse)
            .mapRight(Right(_))
            .mapLeft(s => HttpError(s, StatusCode.UnprocessableEntity))
        ),
        ConditionalResponseAs(
          _.contentType.exists(MediaType.unsafeParse(_) == MediaType.ApplicationJson),
          asJsonBody[GraphQLResponse[CalibanError]](codec).mapRight(Left(_))
        )
      )

    private def asJsonBody[B: JsonCodec]
      : ResponseAs[Either[ResponseException[String, String], B], Effect[Task] with ZioStreams] =
      asString.mapWithMetadata(
        ResponseAs.deserializeRightWithError(
          implicitly[JsonCodec[B]].decode(_) match {
            case _: DecodeResult.Failure => Left("Failed to decode")
            case DecodeResult.Value(v)   => Right(v)
          }
        )
      )
  }
}

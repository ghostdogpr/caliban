package caliban.interop.tapir

import caliban.InputValue.ObjectValue
import caliban.Value.StringValue
import caliban._
import sttp.capabilities.zio.ZioStreams
import sttp.capabilities.{ Effect, WebSockets }
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import sttp.client3.httpclient.zio.SttpClient
import sttp.client3.impl.zio.ZioServerSentEvents
import sttp.client3.{ BasicRequestBody, DeserializationException, HttpError, ResponseException, SttpBackend }
import sttp.model._
import sttp.model.sse.ServerSentEvent
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
  trait Capabilities extends ZioStreams with WebSockets

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
    wsUri: Option[Uri] = None,
    sseSupport: Boolean = true
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[CalibanError]],
    wsInputCodec: JsonCodec[GraphQLWSInput],
    wsOutputCodec: JsonCodec[GraphQLWSOutput]
  ): Spec[TestService, Throwable] = suite(label) {
    val httpClient   = new TapirClient(httpUri)
    val uploadClient = uploadUri.map(new TapirClient(_))
    val run          = (request: GraphQLRequest) => httpClient.runPost(request)
    val runSSE       = (request: GraphQLRequest) => httpClient.runPostStreaming(request)
    val runGet       = (request: GraphQLRequest) => httpClient.runGet(request)
    val runUpload    = uploadClient.map(client => (request: List[Part[BasicRequestBody]]) => client.runUpload(request))
    val runWS        = wsUri.map(wsUri =>
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(WebSocketInterpreter.makeWebSocketEndpoint, Some(wsUri))
    )

    val acceptApplicationJson = Header.accept("application/json")
    val acceptTextEventStream = Header.accept(MediaType.TextEventStream)

    def runHttpRequest(
      method: String,
      acceptHeader: Header = acceptApplicationJson,
      query: String = "{ characters { name }  }"
    ) =
      for {
        _run <- method match {
                  case "POST" => ZIO.succeed(run)
                  case "GET"  => ZIO.succeed(runGet)
                  case _      => ZIO.fail(new RuntimeException(s"Unsupported test method $method"))
                }
        res  <- ZIO.serviceWithZIO[SttpBackend[Task, Capabilities]] {
                  _run(GraphQLRequest(Some(query)))
                    .header(acceptHeader, replaceExisting = true)
                    .send(_)
                }
      } yield res

    def runSSERequest(
      method: String,
      acceptHeader: Header = acceptTextEventStream,
      query: String = "{ characters { name }  }"
    ) =
      for {
        res    <- ZIO
                    .serviceWithZIO[SttpBackend[Task, Capabilities]] {
                      runSSE(GraphQLRequest(Some(query)))
                        .header(acceptHeader, replaceExisting = true)
                        .send(_)
                    }
        stream <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
      } yield stream

    def testHttpEndpoint(
      method: String,
      acceptHeader: Header = acceptApplicationJson,
      expectedContentType: String = MediaType.ApplicationJson.toString()
    ) =
      for {
        res      <- runHttpRequest(method, acceptHeader)
        response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
      } yield assertTrue(
        res.contentType.contains(expectedContentType),
        response.is(_.left).data.toString ==
          """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
      )

    val tests: List[Option[Spec[SttpBackend[Task, Capabilities], Throwable]]] = List(
      Some(
        suite("http")(
          test("test POST http endpoint")(testHttpEndpoint("POST")),
          test("test GET http endpoint")(testHttpEndpoint("GET")),
          test("test interceptor failure") {
            for {
              res      <- ZIO.serviceWithZIO[SttpBackend[Task, Capabilities]](
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
              res  <- ZIO.serviceWithZIO[SttpBackend[Task, Capabilities]](r.send(_))
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
              res  <- ZIO.serviceWithZIO[SttpBackend[Task, Capabilities]](r.send(_))
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
              res  <- ZIO.serviceWithZIO[SttpBackend[Task, Capabilities]](r.send(_))
              body <-
                ZIO.fromEither(res.body).mapError(e => new Throwable(s"Failed to parse result: $res, ${e.getMessage}"))
            } yield assertTrue(body.isRight, body.toOption.exists(_.size == 9))
          },
          test("test caching directives") {
            val q =
              """{ characters { name } }"""

            val r = run(GraphQLRequest())
              .header(Header("content-type", "application/graphql; charset=utf-8"), replaceExisting = true)
              .body(q)
              .contentLength(q.length)

            for {
              backend    <- ZIO.service[SttpBackend[Task, Capabilities]]
              res        <- backend.send(r)
              cacheHeader = res.headers.collectFirst { case h if h.is("Cache-Control") => h }
            } yield assertTrue(cacheHeader.get == Header("cache-control", "max-age=10, public"))
          },
          suite("Accept application/graphql-response+json") {
            val contentT     = "application/graphql-response+json"
            val acceptHeader = Header.accept(contentT)

            List(
              test("Succeeds for GET and POST requests") {
                testHttpEndpoint("GET", acceptHeader, contentT) &&
                testHttpEndpoint("POST", acceptHeader, contentT)
              },
              test("Content-type matches the request Accept header") {
                def test_(method: String) =
                  for {
                    resp <- runHttpRequest(method, acceptHeader)
                    body <- ZIO.fromEither(resp.body)
                  } yield assertTrue(
                    resp.is200,
                    resp.contentType.contains(contentT),
                    body.isLeft
                  )
                test_("GET") && test_("POST")
              },
              test("Returns 400 status code on parsing errors") {
                def test_(method: String) =
                  for {
                    resp  <- runHttpRequest(method, acceptHeader, query = "{characters { name }")
                    error <-
                      resp.body.fold(ZIO.succeed(_), v => ZIO.fail(new Throwable(s"expected request to fail $v")))
                  } yield assertTrue(
                    resp.code == StatusCode.BadRequest,
                    resp.contentType.contains(contentT),
                    !error.toString.contains("\"data\"")
                  )
                test_("GET") && test_("POST")
              },
              test("Returns 400 status code on validation errors") {
                def test_(method: String) =
                  for {
                    resp  <- runHttpRequest(method, acceptHeader, query = "{characterss { name }}")
                    error <-
                      resp.body.fold(ZIO.succeed(_), v => ZIO.fail(new Throwable(s"expected request to fail: $v")))
                  } yield assertTrue(
                    resp.code == StatusCode.BadRequest,
                    resp.contentType.contains(contentT),
                    !error.toString.contains("\"data\"")
                  )
                test_("GET") && test_("POST")
              },
              test("Returns 400 status code on variable coersion errors") {
                def test_(method: String) =
                  for {
                    resp  <- runHttpRequest(method, acceptHeader, query = "{character(name: 42) { name }}")
                    error <-
                      resp.body.fold(ZIO.succeed(_), v => ZIO.fail(new Throwable(s"expected request to fail: $v")))
                  } yield assertTrue(
                    resp.code == StatusCode.BadRequest,
                    resp.contentType.contains(contentT),
                    !error.toString.contains("\"data\"")
                  )
                test_("GET") && test_("POST")
              }
            )
          },
          test("returns 400 status code on invalid GET requests") {
            ZIO
              .serviceWithZIO[SttpBackend[Task, Capabilities]](runGet(GraphQLRequest(None)).send(_))
              .map(r => assertTrue(r.code.code == 400))
          },
          test("returns 400 status code on invalid POST requests") {
            ZIO
              .serviceWithZIO[SttpBackend[Task, Capabilities]](run(GraphQLRequest(None)).send(_))
              .map(r => assertTrue(r.code.code == 400))
          }
        )
      ),
      Some(
        suite("server-sent events")(
          test("TextEventStream") {
            for {
              res   <- runSSERequest(
                         Method.POST.method,
                         acceptTextEventStream,
                         "subscription { characterDeleted }"
                       )
              _     <- runHttpRequest(
                         method = Method.POST.method,
                         query = """mutation{ deleteCharacter(name: "Amos Burton") }"""
                       )
              event <- res.runHead
            } yield assertTrue(
              event.isDefined && event.get == ServerSentEvent(
                Some("""{"data":{"characterDeleted":"Amos Burton"}}"""),
                Some("next")
              )
            )
          } @@ TestAspect.timeout(10.seconds)
        ).when(sseSupport)
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
              res  <- ZIO.serviceWithZIO[SttpBackend[Task, Capabilities]](runUpload(parts).send(_))
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
              res  <- ZIO.serviceWithZIO[SttpBackend[Task, Capabilities]](runUpload(parts).send(_))
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
            import caliban.ws.Protocol.Legacy.Ops
            val io =
              for {
                res         <- ZIO.serviceWithZIO[SttpBackend[Task, Capabilities]](
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
                                   .serviceWithZIO[SttpBackend[Task, Capabilities]](
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
            import caliban.ws.Protocol.GraphQLWS.Ops
            val io =
              for {
                res         <- ZIO.serviceWithZIO[SttpBackend[Task, Capabilities]](
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
                                   .serviceWithZIO[SttpBackend[Task, Capabilities]](
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
  }.provideLayerShared(
    ZLayer.scoped(
      AsyncHttpClientZioBackend.scoped().asInstanceOf[ZIO[Scope, Throwable, SttpBackend[Task, Capabilities]]]
    )
  ) @@
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

    def runPostStreaming(
      request: GraphQLRequest
    ): RequestT[Identity, Either[String, ZStream[Any, Throwable, ServerSentEvent]], ZioStreams] = {
      val req = basicRequest
        .post(httpUri)
        .body(request)
        .contentType(MediaType.TextEventStream)
        .header(Header.accept(MediaType.TextEventStream))

      req
        .response(asStreamUnsafe(ZioStreams))
        .mapResponseRight(ZioServerSentEvents.parse(_))
    }

    def runGet(request: GraphQLRequest): Request[Either[ResponseException[String, String], Either[GraphQLResponse[
      CalibanError
    ], Chunk[ResponseValue]]], Effect[Task] with ZioStreams] =
      basicRequest
        .get(
          httpUri.addParams(
            QueryParams.fromSeq(
              List(
                request.query.map("query"                 -> _),
                request.operationName.map("operationName" -> _),
                request.variables.map("variables"         -> _.toJson),
                request.extensions.map("extensions"       -> _.toJson)
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
          _.contentType.exists(_.startsWith("multipart/mixed")),
          asStream(ZioStreams)(readMultipartResponse)
            .mapRight(Right(_))
            .mapLeft(s => HttpError(s, StatusCode.UnprocessableEntity))
        ),
        ConditionalResponseAs(
          _.contentType.exists { ct =>
            val mt = MediaType.unsafeParse(ct)
            mt == MediaType.ApplicationJson || mt == MediaType("application", "graphql-response+json")
          },
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

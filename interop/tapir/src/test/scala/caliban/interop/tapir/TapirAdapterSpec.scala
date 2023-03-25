package caliban.interop.tapir

import caliban.InputValue.ObjectValue
import caliban.Value.StringValue
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, GraphQLWSInput, GraphQLWSOutput }
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.client3.SttpBackend
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import sttp.model.{ Header, MediaType, Method, Part, QueryParams, StatusCode, Uri }
import sttp.tapir.AttributeKey
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.client.sttp.SttpClientInterpreter
import sttp.tapir.client.sttp.ws.zio._
import sttp.tapir.model.{ ConnectionInfo, ServerRequest }
import zio.stream.ZStream
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

  def makeSuite(
    label: String,
    httpUri: Uri,
    uploadUri: Option[Uri] = None,
    wsUri: Option[Uri] = None
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]],
    responseCodec: JsonCodec[GraphQLResponse[CalibanError]],
    wsInputCodec: JsonCodec[GraphQLWSInput],
    wsOutputCodec: JsonCodec[GraphQLWSOutput]
  ): Spec[TestService, Throwable] = suite(label) {
    val run       =
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeHttpEndpoints[CalibanError].head, Some(httpUri))
    val runUpload = uploadUri.map(uploadUri =>
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeHttpUploadEndpoint[CalibanError], Some(uploadUri))
    )
    val runWS     = wsUri.map(wsUri =>
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeWebSocketEndpoint, Some(wsUri))
    )

    val tests: List[Option[Spec[SttpBackend[Task, ZioStreams with WebSockets], Throwable]]] = List(
      Some(
        suite("http")(
          test("test http endpoint") {
            for {
              res      <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](
                            run((GraphQLRequest(Some("{ characters { name }  }")), null)).send(_)
                          )
              response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
            } yield assertTrue(
              response.data.toString ==
                """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
            )

          },
          test("test interceptor failure") {
            for {
              res      <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](
                            run((GraphQLRequest(Some("{ characters { name }  }")), null)).header("X-Invalid", "1").send(_)
                          )
              response <- ZIO.fromEither(res.body).flip.orElseFail(new Throwable("Failed to parse result"))
            } yield assertTrue(response.code == StatusCode.Unauthorized) &&
              assertTrue(response.body == "You are unauthorized!")
          },
          test("lower-case content-type header") {
            val q = "{ characters { name }  }"
            val r = run((GraphQLRequest(), null))
              .header(Header("content-type", "application/graphql; charset=utf-8"), replaceExisting = true)
              .body(q)
              // if we don't set content-length here it gets incorrectly set to 70 rather than 24.
              .contentLength(q.length)

            for {
              res      <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](r.send(_))
              response <- ZIO.fromEither(res.body).orElseFail(new Throwable(s"Failed to parse result: $res"))
            } yield assertTrue(
              response.data.toString ==
                """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
            )
          }
        )
      ),
      runUpload.map(runUpload =>
        test("test http upload endpoint") {
          val query =
            """{ "query": "mutation ($files: [Upload!]!) { uploadFiles(files: $files) { hash, filename, mimetype } }", "variables": { "files": [null, null] }}"""

          val parts =
            List(
              Part("operations", query.getBytes, contentType = Some(MediaType.ApplicationJson)),
              Part("map", """{ "0": ["variables.files.0"], "1":  ["variables.files.1"]}""".getBytes),
              Part("0", """image""".getBytes, contentType = Some(MediaType.ImagePng)).fileName("a.png"),
              Part("1", """text""".getBytes, contentType = Some(MediaType.TextPlain)).fileName("a.txt")
            )

          for {
            res      <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](runUpload((parts, null)).send(_))
            response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
          } yield assertTrue(
            response.data.toString ==
              """{"uploadFiles":[{"hash":"6105d6cc76af400325e94d588ce511be5bfdbb73b437dc51eca43917d7a43e3d","filename":"a.png","mimetype":"image/png"},{"hash":"982d9e3eb996f559e633f4d194def3761d909f5a3b647d1a851fead67c32c9d1","filename":"a.txt","mimetype":"text/plain"}]}"""
          )
        }
      ),
      runUpload.map(runUpload =>
        test("test http upload endpoint for extra fields") {
          val query =
            """{ "query": "mutation ($uploadedDocuments: [UploadedDocumentInput!]!) { uploadFilesWithExtraFields(uploadedDocuments: $uploadedDocuments) { someField1, someField2} }", "variables": { "uploadedDocuments": [{"file": null, "someField1": 1, "someField2": 2}, {"file": null, "someField1": 3}] }}"""

          val parts =
            List(
              Part("operations", query.getBytes, contentType = Some(MediaType.ApplicationJson)),
              Part(
                "map",
                """{ "0": ["variables.uploadedDocuments.0.file"], "1":  ["variables.uploadedDocuments.1.file"]}""".getBytes
              ),
              Part("0", """image""".getBytes, contentType = Some(MediaType.ImagePng)).fileName("a.png"),
              Part("1", """text""".getBytes, contentType = Some(MediaType.TextPlain)).fileName("a.txt")
            )

          for {
            res      <- ZIO.serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](runUpload((parts, null)).send(_))
            response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
          } yield assertTrue(
            response.data.toString ==
              """{"uploadFilesWithExtraFields":[{"someField1":1,"someField2":2},{"someField1":3,"someField2":null}]}"""
          )
        }
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
                res         <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
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
                                       (
                                         GraphQLRequest(Some("""mutation{ deleteCharacter(name: "Amos Burton") }""")),
                                         null
                                       )
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
              assertTrue(messages.head.`type` == Ops.ConnectionAck) &&
              assertTrue(messages(1).payload.get.toString == """{"data":{"characterDeleted":"Amos Burton"}}""") &&
              assertTrue(messages(2).`type` == Ops.Complete)
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
                                       (
                                         GraphQLRequest(Some("""mutation{ deleteCharacter(name: "Amos Burton") }""")),
                                         null
                                       )
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
              assertTrue(messages.head.map(_.`type`) == Right(Ops.ConnectionAck)) &&
              assertTrue(
                messages(1).map(_.payload.get.toString) == Right("""{"data":{"characterDeleted":"Amos Burton"}}""")
              ) &&
              assertTrue(messages(2).map(_.`type`) == Right(Ops.Pong)) &&
              assertTrue(messages(3).map(_.`type`) == Right(Ops.Complete))
            }
          } @@ TestAspect.timeout(60.seconds)
        )
      )
    )

    ZIO.succeed(tests.flatten)
  }.provideLayerShared(AsyncHttpClientZioBackend.layer()) @@
    before(TestService.reset) @@
    TestAspect.sequential
}

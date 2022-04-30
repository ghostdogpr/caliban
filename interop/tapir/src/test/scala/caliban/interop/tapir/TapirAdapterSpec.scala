package caliban.interop.tapir

import caliban.InputValue.ObjectValue
import caliban.Value.StringValue
import caliban.interop.tapir.TestService.TestService
import caliban.{ CalibanError, GraphQLRequest, GraphQLWSInput }
import sttp.client3.asynchttpclient.zio._
import sttp.model.{ Header, MediaType, Method, Part, QueryParams, StatusCode, Uri }
import sttp.tapir.client.sttp.SttpClientInterpreter
import sttp.tapir.client.sttp.ws.zio1._
import sttp.tapir.json.circe._
import sttp.tapir.model.{ ConnectionInfo, ServerRequest }
import zio.clock.Clock
import zio.duration._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect.before
import zio.test._
import zio.{ Queue, ZIO }

import scala.language.postfixOps

object TapirAdapterSpec {
  case class FakeServerRequest(method: Method, uri: Uri, headers: List[Header] = Nil) extends ServerRequest {
    override def protocol: String = "http"

    override def connectionInfo: ConnectionInfo = ConnectionInfo.NoInfo

    override def underlying: Any = ()

    override def pathSegments: List[String] =
      uri.pathSegments.segments.map(_.v).toList

    override def queryParameters: QueryParams = uri.params
  }

  def makeSuite(
    label: String,
    httpUri: Uri,
    uploadUri: Option[Uri] = None,
    wsUri: Option[Uri] = None
  ): ZSpec[TestService, Throwable] = suiteM(label) {
    val run       =
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeHttpEndpoints[Any, CalibanError].head, Some(httpUri))
    val runUpload = uploadUri.map(uploadUri =>
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeHttpUploadEndpoint[Any, CalibanError], Some(uploadUri))
    )
    val runWS     = wsUri.map(wsUri =>
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeWebSocketEndpoint, Some(wsUri))
    )

    val tests: List[Option[ZSpec[SttpClient, Throwable]]] = List(
      Some(
        suite("http")(
          testM("test http endpoint") {
            val io =
              for {
                res      <- send(run((GraphQLRequest(Some("{ characters { name }  }")), null)))
                response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
              } yield response.data.toString

            assertM(io)(
              equalTo(
                """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
              )
            )
          },
          testM("test interceptor failure") {
            for {
              res      <- send(
                            run(
                              (
                                GraphQLRequest(Some("{ characters { name }  }")),
                                null
                              )
                            ).header("X-Invalid", "1")
                          )
              response <- ZIO.fromEither(res.body).flip.orElseFail(new Throwable("Failed to parse result"))
            } yield assert(response.code)(equalTo(StatusCode.Unauthorized)) && assert(response.body)(
              equalTo("You are unauthorized!")
            )
          },
          testM("lower-case content-type header") {
            val q = "{ characters { name }  }"
            val r = run((GraphQLRequest(), null))
              .header(Header("content-type", "application/graphql; charset=utf-8"), true)
              .body(q)
              // if we don't set content-length here it gets incorrectly set to 70 rather than 24.
              .contentLength(q.length)

            val io =
              for {
                res      <- send(r)
                response <- ZIO.fromEither(res.body).orElseFail(new Throwable(s"Failed to parse result: ${res}"))
              } yield response.data.toString

            assertM(io)(
              equalTo(
                """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
              )
            )
          }
        )
      ),
      runUpload.map(runUpload =>
        testM("test http upload endpoint") {
          val query =
            """{ "query": "mutation ($files: [Upload!]!) { uploadFiles(files: $files) { hash, filename, mimetype } }", "variables": { "files": [null, null] }}"""

          val parts =
            List(
              Part("operations", query.getBytes, contentType = Some(MediaType.ApplicationJson)),
              Part("map", """{ "0": ["variables.files.0"], "1":  ["variables.files.1"]}""".getBytes),
              Part("0", """image""".getBytes, contentType = Some(MediaType.ImagePng)).fileName("a.png"),
              Part("1", """text""".getBytes, contentType = Some(MediaType.TextPlain)).fileName("a.txt")
            )

          val io =
            for {
              res      <- send(runUpload((parts, null)))
              response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
            } yield response.data.toString

          assertM(io)(
            equalTo(
              """{"uploadFiles":[{"hash":"6105d6cc76af400325e94d588ce511be5bfdbb73b437dc51eca43917d7a43e3d","filename":"a.png","mimetype":"image/png"},{"hash":"982d9e3eb996f559e633f4d194def3761d909f5a3b647d1a851fead67c32c9d1","filename":"a.txt","mimetype":"text/plain"}]}"""
            )
          )
        }
      ),
      runUpload.map(runUpload =>
        testM("test http upload endpoint for extra fields") {
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

          val io =
            for {
              res      <- send(runUpload((parts, null)))
              response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
            } yield response.data.toString

          assertM(io)(
            equalTo(
              """{"uploadFilesWithExtraFields":[{"someField1":1,"someField2":2},{"someField1":3,"someField2":null}]}"""
            )
          )
        }
      ),
      runWS.map(runWS =>
        suite("test ws endpoint")(
          testM("legacy ws") {
            import caliban.interop.tapir.ws.Protocol.Legacy.Ops
            val io =
              for {
                res         <- send(
                                 runWS(
                                   FakeServerRequest(
                                     Method.GET,
                                     Uri.unsafeParse("http://localhost:80/ws/graphql"),
                                     Header("Sec-WebSocket-Protocol", "graphql-ws") :: Nil
                                   ) -> "graphql-ws"
                                 )
                               )
                pipe        <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
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
                sendDelete   = send(
                                 run((GraphQLRequest(Some("""mutation{ deleteCharacter(name: "Amos Burton") }""")), null))
                               ).delay(3 seconds)
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
                                 .timeoutFail(new Throwable("timeout ws"))(60.seconds)
                                 .provideSomeLayer[SttpClient](Clock.live)
              } yield messages

            io.map(_.collect { case Right(value) => value }).map { messages =>
              assertTrue(messages.head.`type` == Ops.ConnectionAck) &&
              assertTrue(messages(1).payload.get.toString == """{"data":{"characterDeleted":"Amos Burton"}}""") &&
              assertTrue(messages(2).`type` == Ops.Complete)
            }
          },
          testM("graphql-ws") {
            import caliban.interop.tapir.ws.Protocol.GraphQLWS.Ops
            val io =
              for {
                res         <- send(
                                 runWS(
                                   FakeServerRequest(
                                     Method.GET,
                                     Uri.unsafeParse("http://localhost:80/ws/graphql"),
                                     Header("Sec-WebSocket-Protocol", "graphql-transport-ws") :: Nil
                                   ) -> "graphql-transport-ws"
                                 )
                               )
                pipe        <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
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
                sendDelete   = send(
                                 run((GraphQLRequest(Some("""mutation{ deleteCharacter(name: "Amos Burton") }""")), null))
                               ).delay(3 seconds)
                stop         = inputQueue.offer(GraphQLWSInput(Ops.Complete, Some("id"), None))
                messages    <- outputStream
                                 .tap(out =>
                                   ZIO.whenCase(out) {
                                     case Right(out) if out.`type` == Ops.ConnectionAck => sendDelete
                                     case Right(out) if out.`type` == Ops.Next          => stop
                                   }
                                 )
                                 .take(3)
                                 .runCollect
                                 .timeoutFail(new Throwable("timeout ws"))(60.seconds)
                                 .provideSomeLayer[SttpClient](Clock.live)
              } yield messages

            io.map { messages =>
              assertTrue(messages.head.map(_.`type`) == Right(Ops.ConnectionAck)) &&
              assertTrue(
                messages(1).map(_.payload.get.toString) == Right("""{"data":{"characterDeleted":"Amos Burton"}}""")
              ) &&
              assertTrue(messages(2).map(_.`type`) == Right(Ops.Complete))
            }
          }
        ) @@ TestAspect.sequential
      )
    )

    ZIO.succeed(tests.flatten)
  }.provideLayer(AsyncHttpClientZioBackend.layer().mapError(TestFailure.fail)) @@ before(
    TestService.reset
  ) @@ TestAspect.sequential
}

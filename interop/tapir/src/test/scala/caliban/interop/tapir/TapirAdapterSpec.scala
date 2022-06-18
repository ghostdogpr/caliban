package caliban.interop.tapir

import caliban.InputValue.ObjectValue
import caliban.Value.StringValue
import caliban.{ CalibanError, GraphQLRequest, GraphQLWSInput }
import sttp.client3.asynchttpclient.zio._
import sttp.model.{ Header, MediaType, Method, Part, QueryParams, StatusCode, Uri }
import sttp.tapir.client.sttp.SttpClientInterpreter
import sttp.tapir.client.sttp.ws.zio._
import sttp.tapir.json.circe._
import sttp.tapir.model.{ ConnectionInfo, ServerRequest }
import zio.stream.ZStream
import zio.test._
import zio.{ test => _, _ }
import scala.language.postfixOps

import sttp.tapir.AttributeKey

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
  ): Spec[Live, Throwable] = {
    val run       =
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeHttpEndpoints[Any, CalibanError].head, Some(httpUri))
    val runUpload = uploadUri.map(uploadUri =>
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeHttpUploadEndpoint[Any, CalibanError], Some(uploadUri))
    )
    val runWS     = wsUri.map(wsUri =>
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeWebSocketEndpoint[Any, CalibanError], Some(wsUri))
    )

    val tests: List[Option[Spec[Live with SttpClient, Throwable]]] = List(
      Some(
        suite("http")(
          test("test http endpoint") {
            for {
              res      <- send(run((GraphQLRequest(Some("{ characters { name }  }")), null)))
              response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
            } yield assertTrue(
              response.data.toString ==
                """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
            )

          },
          test("test interceptor failure") {
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
              res      <- send(r)
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
            res      <- send(runUpload((parts, null)))
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
            res      <- send(runUpload((parts, null)))
            response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
          } yield assertTrue(
            response.data.toString ==
              """{"uploadFilesWithExtraFields":[{"someField1":1,"someField2":2},{"someField1":3,"someField2":null}]}"""
          )
        }
      ),
      runWS.map(runWS =>
        test("test ws endpoint") {
          val io =
            for {
              res         <- send(runWS(null))
              pipe        <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
              inputQueue  <- Queue.unbounded[GraphQLWSInput]
              inputStream  = ZStream.fromQueue(inputQueue)
              outputStream = pipe(inputStream)
              _           <- inputQueue.offer(GraphQLWSInput("connection_init", None, None))
              _           <- inputQueue.offer(
                               GraphQLWSInput(
                                 "start",
                                 Some("id"),
                                 Some(ObjectValue(Map("query" -> StringValue("subscription { characterDeleted }"))))
                               )
                             )
              sendDelete   = send(
                               run((GraphQLRequest(Some("""mutation{ deleteCharacter(name: "Amos Burton") }""")), null))
                             ).delay(3 seconds)
              stop         = inputQueue.offer(GraphQLWSInput("stop", Some("id"), None))
              messages    <- Live.live {
                               outputStream
                                 .tap(out => ZIO.when(out.`type` == "connection_ack")(sendDelete))
                                 .tap(out => ZIO.when(out.`type` == "data")(stop))
                                 .take(3)
                                 .runCollect
                                 .timeoutFail(new Throwable("timeout ws"))(30.seconds)
                             }
            } yield messages

          io.map { messages =>
            assertTrue(messages.head.`type` == "connection_ack") &&
            assertTrue(messages(1).payload.get.toString == """{"data":{"characterDeleted":"Amos Burton"}}""") &&
            assertTrue(messages(2).`type` == "complete")
          }
        }
      )
    )

    suite(label)(tests.flatten: _*).provideSomeLayer[Live](AsyncHttpClientZioBackend.layer()) @@ TestAspect.sequential
  }
}

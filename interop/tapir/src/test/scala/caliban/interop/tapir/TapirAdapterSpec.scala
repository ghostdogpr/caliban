package caliban.interop.tapir

import caliban.InputValue.ObjectValue
import caliban.Value.StringValue
import caliban.{ CalibanError, GraphQLRequest, GraphQLWSInput }
import sttp.client3.asynchttpclient.zio._
import sttp.model.{ MediaType, Part, Uri }
import sttp.tapir.client.sttp.SttpClientInterpreter
import sttp.tapir.client.sttp.ws.zio._
import sttp.tapir.json.circe._
import zio.clock.Clock
import zio.duration._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio.{ Queue, ZIO }

import scala.language.postfixOps

object TapirAdapterSpec {
  def makeSuite(
    label: String,
    httpUri: Uri,
    uploadUri: Option[Uri] = None,
    wsUri: Option[Uri] = None
  ): ZSpec[Any, Throwable] = {
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

    val tests: List[Option[ZSpec[SttpClient, Throwable]]] = List(
      Some(
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
        }
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
              _         = println(response.data.toString)
            } yield response.data.toString

          assertM(io)(
            equalTo(
              """{"uploadFilesWithExtraFields":[{"someField1": 1,"someField2": 2},[{"someField1": 2}]}"""
            )
          )
        }
      ),
      runWS.map(runWS =>
        testM("test ws endpoint") {
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
              sendDelete   =
                send(run((GraphQLRequest(Some("""mutation{ deleteCharacter(name: "Amos Burton") }""")), null)))
                  .delay(3 seconds)
              messages    <- outputStream
                               .tap(out => ZIO.when(out.`type` == "connection_ack")(sendDelete))
                               .take(2)
                               .runCollect
                               .timeoutFail(new Throwable("timeout ws"))(30.seconds)
                               .provideSomeLayer[SttpClient](Clock.live)
            } yield messages

          io.map { messages =>
            assert(messages.head.`type`)(equalTo("connection_ack")) &&
            assert(messages(1).payload.get.toString)(equalTo("""{"data":{"characterDeleted":"Amos Burton"}}"""))
          }
        }
      )
    )

    suite(label)(tests.flatten: _*)
      .provideLayer(AsyncHttpClientZioBackend.layer().mapError(TestFailure.fail)) @@ TestAspect.sequential
  }
}

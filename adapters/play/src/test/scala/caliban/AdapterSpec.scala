package caliban

import java.io.File
import java.math.BigInteger
import java.net.URL
import java.security.MessageDigest

import caliban.GraphQL.graphQL
import caliban.schema.GenericSchema
import caliban.uploads._
import io.circe.generic.auto._
import io.circe.parser._
import play.api.Mode
import play.api.mvc.DefaultControllerComponents
import play.core.server.{ AkkaHttpServer, Server, ServerConfig }
import play.mvc.Http.MimeTypes
import sttp.client3._
import sttp.client3.asynchttpclient.zio.{ AsyncHttpClientZioBackend, _ }
import zio.{ Has, Runtime, UIO, ZIO, ZLayer }
import zio.blocking._
import zio.clock.Clock
import zio.console.Console
import zio.internal.Platform
import zio.random.Random
import zio.test._
import zio.test.Assertion._
import zio.test.environment.TestEnvironment

case class Response[A](data: A)
case class UploadFile(uploadFile: TestAPI.File)
case class UploadFiles(uploadFiles: List[TestAPI.File])

object Service {
  def uploadFile(file: Upload): ZIO[Uploads with Blocking, Throwable, TestAPI.File] =
    for {
      bytes <- file.allBytes
      meta  <- file.meta
    } yield TestAPI.File(
      Service.hex(Service.sha256(bytes.toArray)),
      meta.map(_.path.toAbsolutePath.toString).getOrElse(""),
      meta.map(_.fileName).getOrElse(""),
      meta.flatMap(_.contentType).getOrElse("")
    )

  def uploadFiles(files: List[Upload]): ZIO[Uploads with Blocking, Throwable, List[TestAPI.File]] =
    ZIO.collectAllPar(
      for {
        file <- files
      } yield for {
        bytes <- file.allBytes
        meta  <- file.meta
      } yield TestAPI.File(
        Service.hex(Service.sha256(bytes.toArray)),
        meta.map(_.path.toAbsolutePath.toString).getOrElse(""),
        meta.map(_.fileName).getOrElse(""),
        meta.flatMap(_.contentType).getOrElse("")
      )
    )

  def sha256(b: Array[Byte]) =
    MessageDigest.getInstance("SHA-256").digest(b)

  def hex(b: Array[Byte]): String =
    String.format("%032x", new BigInteger(1, b))
}

case class UploadFileArgs(file: Upload)
case class UploadFilesArgs(files: List[Upload])

object TestAPI extends GenericSchema[Blocking with Uploads with Console with Clock] {
  val api: GraphQL[Blocking with Uploads with Console with Clock] =
    graphQL(
      RootResolver(
        Queries(args => UIO("stub")),
        Mutations(args => Service.uploadFile(args.file), args => Service.uploadFiles(args.files))
      )
    )

  implicit val uploadFileArgsSchema = gen[UploadFileArgs]
  implicit val mutationsSchema      = gen[Mutations]
  implicit val queriesSchema        = gen[Queries]

  case class File(hash: String, path: String, filename: String, mimetype: String)

  case class Queries(stub: Unit => UIO[String])

  case class Mutations(
    uploadFile: UploadFileArgs => ZIO[Blocking with Uploads, Throwable, File],
    uploadFiles: UploadFilesArgs => ZIO[Blocking with Uploads, Throwable, List[File]]
  )
}

object AdapterSpec extends DefaultRunnableSpec {
  val runtime: Runtime[Console with Clock with Blocking with Random with Uploads] =
    Runtime.unsafeFromLayer(
      Console.live ++ Clock.live ++ Blocking.live ++ Random.live ++ Uploads.empty,
      Platform.default
    )

  val apiLayer = ZLayer.fromAcquireRelease(
    for {
      interpreter <- TestAPI.api.interpreter
    } yield AkkaHttpServer.fromRouterWithComponents(
      ServerConfig(
        mode = Mode.Dev,
        port = Some(8088),
        address = "127.0.0.1"
      )
    ) { components =>
      PlayRouter(
        interpreter,
        DefaultControllerComponents(
          components.defaultActionBuilder,
          components.playBodyParsers,
          components.messagesApi,
          components.langs,
          components.fileMimeTypes,
          components.executionContext
        )
      )(runtime, components.materializer).routes
    }
  )(server => UIO(server.stop()))

  val specLayer: ZLayer[zio.ZEnv, CalibanError.ValidationError, Has[Server]] =
    Uploads.empty >>> apiLayer

  val uri = uri"http://localhost:8088/api/graphql"

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Requests")(
      testM("multipart request with one file") {
        val fileHash         = "64498927ff9cd735daefebe7175ed1567650399e58648a6b8340f636243962c0"
        val fileName: String = s"$fileHash.png"
        val fileURL: URL     = getClass.getResource(s"/$fileName")

        val query: String =
          """{ "query": "mutation ($file: Upload!) { uploadFile(file: $file) { hash, path, filename, mimetype } }",   "variables": {  "file": null }}"""

        val request = basicRequest
          .post(uri)
          .multipartBody(
            multipart("operations", query).contentType(MimeTypes.JSON),
            multipart("map", """{ "0": ["variables.file"] }"""),
            multipartFile("0", new File(fileURL.getPath)).contentType("image/png")
          )
          .contentType("multipart/form-data")

        val body = for {
          response <- send(
                        request.mapResponse { strRespOrError =>
                          for {
                            resp           <- strRespOrError
                            json           <- parse(resp)
                            fileUploadResp <- json.as[Response[UploadFile]]
                          } yield fileUploadResp
                        }
                      )
        } yield response.body

        assertM(body.map(_.toOption.get.data.uploadFile))(
          hasField("hash", (f: TestAPI.File) => f.hash, equalTo(fileHash)) &&
            hasField("filename", (f: TestAPI.File) => f.filename, equalTo(fileName)) &&
            hasField("mimetype", (f: TestAPI.File) => f.mimetype, equalTo("image/png"))
        )
      },
      testM("multipart request with several files") {
        val file1Hash         = "64498927ff9cd735daefebe7175ed1567650399e58648a6b8340f636243962c0"
        val file1Name: String = s"$file1Hash.png"
        val file1URL: URL     = getClass.getResource(s"/$file1Name")

        val file2Hash         = "d6359a52607b6953b2cb96be00b228c6030f11806e380c29c3f5d8db608c399b"
        val file2Name: String = s"$file2Hash.txt"
        val file2URL: URL     = getClass.getResource(s"/$file2Name")

        val query: String =
          """{ "query": "mutation ($files: [Upload!]!) { uploadFiles(files: $files) { hash, path, filename, mimetype } }",   "variables": {  "files": [null, null] }}"""

        val request = basicRequest
          .post(uri)
          .contentType("multipart/form-data")
          .multipartBody(
            multipart("operations", query).contentType(MimeTypes.JSON),
            multipart("map", """{ "0": ["variables.files.0"], "1":  ["variables.files.1"]}"""),
            multipartFile("0", new File(file1URL.getPath)).contentType("image/png"),
            multipartFile("1", new File(file2URL.getPath)).contentType(MimeTypes.TEXT)
          )

        val body = for {
          response <- send(
                        request.mapResponse { strRespOrError =>
                          for {
                            resp           <- strRespOrError
                            json           <- parse(resp)
                            fileUploadResp <- json.as[Response[UploadFiles]]
                          } yield fileUploadResp
                        }
                      )
        } yield response.body

        assertM(body.map(_.toOption.get.data.uploadFiles))(
          hasField("hash", (fl: List[TestAPI.File]) => fl(0).hash, equalTo(file1Hash)) &&
            hasField("hash", (fl: List[TestAPI.File]) => fl(1).hash, equalTo(file2Hash)) &&
            hasField("filename", (fl: List[TestAPI.File]) => fl(0).filename, equalTo(file1Name)) &&
            hasField("filename", (fl: List[TestAPI.File]) => fl(1).filename, equalTo(file2Name)) &&
            hasField("mimetype", (fl: List[TestAPI.File]) => fl(0).mimetype, equalTo("image/png")) &&
            hasField("mimetype", (fl: List[TestAPI.File]) => fl(1).mimetype, equalTo(MimeTypes.TEXT))
        )
      }
    ).provideCustomLayerShared(AsyncHttpClientZioBackend.layer() ++ specLayer)
      .mapError(TestFailure.fail)

}

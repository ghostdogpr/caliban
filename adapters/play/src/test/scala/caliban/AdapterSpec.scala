package caliban

import java.net.URL

import caliban.HttpClient.HttpClient
import zio.random.Random
//import caliban.Service.HService
import caliban.schema.GenericSchema
import cats.effect.Blocker
import org.http4s.MediaType
import org.http4s.headers.`Content-Type`
import org.http4s.multipart.{ Multipart, Part }
import zio.{ /*Has, */ Runtime, Task, UIO /*, URIO*/, ZIO, ZLayer }
import zio.internal.Platform
import play.api.mvc.DefaultControllerComponents
import play.api.Mode
import play.core.server.{ AkkaHttpServer, ServerConfig }
import zio.clock.Clock
import zio.console.Console
import zio.test._
import caliban.GraphQL.graphQL
import zio.test.environment.TestEnvironment
import zio.test.Assertion._
import zio.blocking._
import zio.interop.catz._
import java.security.MessageDigest
import java.math.BigInteger
import io.circe.generic.auto._
import caliban.Uploads.Uploads

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
    ZIO.collectAllPar(for {
      file <- files
    } yield {
      for {
        bytes <- file.allBytes
        meta  <- file.meta
      } yield TestAPI.File(
        Service.hex(Service.sha256(bytes.toArray)),
        meta.map(_.path.toAbsolutePath.toString).getOrElse(""),
        meta.map(_.fileName).getOrElse(""),
        meta.flatMap(_.contentType).getOrElse("")
      )
    })

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
  val httpCliLayer: ZLayer[Any, Throwable, HttpClient] =
    HttpClient.makeHttpClient.toLayer >>> HttpClient.make("127.0.0.1", 8088)
  val interpLayer: ZLayer[Any, Any, Console with Clock] = Console.live ++ Clock.live
  val runtime: Runtime[Console with Clock with Blocking with Random with Uploads] =
    Runtime.unsafeFromLayer(
      Console.live ++ Clock.live ++ Blocking.live ++ Random.live ++ Uploads.Service.empty,
      Platform.default
    )

  // THIS BLOCKS INDEFINTELY DUE TO httpCliLayer
  //  val runtime: Runtime[HService with HttpClient with Console with Clock] =
  //    Runtime.unsafeFromLayer(
  //      (Service.make ++ httpCliLayer ++ Console.live ++ Clock.live),
  //      Platform.default
  //    )

  val interpreter: GraphQLInterpreter[Blocking with Uploads with Console with Clock, CalibanError] =
    runtime.unsafeRun(TestAPI.api.interpreter)
  val server = AkkaHttpServer.fromRouterWithComponents(
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

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Requests")(
      testM("multipart request with one file") {
        val fileHash         = "64498927ff9cd735daefebe7175ed1567650399e58648a6b8340f636243962c0"
        val fileName: String = s"$fileHash.png"
        val fileURL: URL     = getClass.getResource(s"/$fileName")

        val query: String =
          """{ "query": "mutation ($file: Upload!) { uploadFile(file: $file) { hash, path, filename, mimetype } }",   "variables": {  "file": "File" }}"""

        val multipart =
          for {
            exec <- blockingExecutor
          } yield Multipart[Task](
            Vector(
              Part.formData("operations", query, `Content-Type`(MediaType.application.json)),
              Part.formData("map", """{ "0": ["variables.file"] }"""),
              Part.fileData("0", fileURL, Blocker.liftExecutionContext(exec.asEC), `Content-Type`(MediaType.image.png))
            )
          )

        val resp = for {
          mp   <- multipart
          resp <- HttpClient.post[Response[UploadFile]]("/api/graphql", "", Map(), mp)
        } yield (
          resp.data.uploadFile
        )

        assertM(resp)(
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
          """{ "query": "mutation ($files: [Upload!]!) { uploadFiles(files: $files) { hash, path, filename, mimetype } }",   "variables": {  "files": ["File", "File"] }}"""

        val multipart =
          for {
            exec <- blockingExecutor
          } yield Multipart[Task](
            Vector(
              Part.formData("operations", query, `Content-Type`(MediaType.application.json)),
              Part.formData("map", """{ "0": ["variables.files.0"], "1":  ["variables.files.1"]}"""),
              Part.fileData(
                "0",
                file1URL,
                Blocker.liftExecutionContext(exec.asEC),
                `Content-Type`(MediaType.image.png)
              ),
              Part.fileData(
                "1",
                file2URL,
                Blocker.liftExecutionContext(exec.asEC),
                `Content-Type`(MediaType.text.plain)
              )
            )
          )

        val resp = for {
          mp   <- multipart
          resp <- HttpClient.post[Response[UploadFiles]]("/api/graphql", "", Map(), mp)
        } yield (
          resp.data.uploadFiles
        )

        assertM(resp)(
          hasField("hash", (fl: List[TestAPI.File]) => fl(0).hash, equalTo(file1Hash)) &&
            hasField("hash", (fl: List[TestAPI.File]) => fl(1).hash, equalTo(file2Hash)) &&
            hasField("filename", (fl: List[TestAPI.File]) => fl(0).filename, equalTo(file1Name)) &&
            hasField("filename", (fl: List[TestAPI.File]) => fl(1).filename, equalTo(file2Name)) &&
            hasField("mimetype", (fl: List[TestAPI.File]) => fl(0).mimetype, equalTo("image/png")) &&
            hasField("mimetype", (fl: List[TestAPI.File]) => fl(1).mimetype, equalTo("text/plain"))
        )
      }
    ).provideCustomLayer(httpCliLayer ++ interpLayer)
      .mapError(TestFailure.fail _)

}

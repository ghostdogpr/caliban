package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{ TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.client4.{ asStringAlways, basicRequest, multipart, UriContext }
import zio._
import zio.http._
import zio.test.{ assertTrue, suite, test, Live, ZIOSpecDefault }

import scala.language.postfixOps

object QuickAdapterSpec extends ZIOSpecDefault {
  import caliban.quick._

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private val auth = Middleware.intercept { case (req, resp) =>
    if (req.headers.get("X-Invalid").nonEmpty)
      Response(Status.Unauthorized, body = Body.fromString("You are unauthorized!"))
    else resp
  }

  private val apiLayer = envLayer >>> ZLayer.fromZIO {
    for {
      routes  <- TestApi.api.interpreter.map { interpreter =>
                   val default       = QuickAdapter(interpreter).configureSse(SseConfig(Some(1.second)))
                   val existing      = default.withMaxRequestBodyBytes(Int.MaxValue - 2)
                   val smallResponse = default.withMaxResponseBodyBytes(64)

                   (existing.routes(
                     "/api/graphql",
                     uploadPath = Some("/upload/graphql"),
                     webSocketPath = Some("/ws/graphql")
                   ) ++
                     default.routes(
                       "/api/graphql-default",
                       uploadPath = Some("/upload/graphql-default")
                     ) ++
                     smallResponse.routes("/api/graphql-small-response")) @@ auth
                 }
      _       <- Server.serve(routes).forkScoped
      _       <- Live.live(Clock.sleep(3 seconds))
      service <- ZIO.service[TestService]
    } yield service
  }

  override def spec = suite("ZIO Http Quick") {
    val adapterSuite = TapirAdapterSpec.makeSuite(
      "QuickAdapterSpec",
      uri"http://localhost:8090/api/graphql",
      wsUri = Some(uri"ws://localhost:8090/ws/graphql"),
      uploadUri = Some(uri"http://localhost:8090/upload/graphql"),
      mutationOverGetStatus = 405
    )
    suite("Quick regressions")(adapterSuite, regressionSuite).provideShared(
      apiLayer,
      Scope.default,
      Server.defaultWith(_.port(8090).enableRequestStreaming.responseCompression())
    )
  }

  private val regressionSuite = suite("HTTP compatibility and limits")(
    test("treats Accept */* like an absent Accept header") {
      val endpoint = uri"http://localhost:8090/api/graphql"
      val body     = """{"query":"{ characters { name } }"}"""

      def send(accept: Option[String]) = {
        val request = basicRequest
          .post(endpoint)
          .contentType("application/json")
          .body(body)
          .response(asStringAlways)
        execute(accept.fold(request)(request.header("Accept", _)))
      }

      for {
        absent       <- send(None)
        wildcard     <- send(Some("*/*"))
        explicitJson <- send(Some("application/json, text/plain, */*"))
        eventStream  <- send(Some("text/event-stream, */*"))
      } yield assertTrue(
        absent.code == wildcard.code,
        absent.contentType == wildcard.contentType,
        absent.contentType.contains("application/json"),
        absent.body.contains("\"data\""),
        wildcard.body.contains("\"data\""),
        explicitJson.contentType.contains("application/json"),
        eventStream.contentType.contains("text/event-stream")
      )
    },
    test("accepts the legacy application/graphql+json POST media type") {
      val body = """{"query":"{ characters { name } }"}"""

      for {
        response <- execute(
                      basicRequest
                        .post(uri"http://localhost:8090/api/graphql")
                        .contentType("application/graphql+json")
                        .body(body)
                        .response(asStringAlways)
                    )
      } yield assertTrue(response.is200, response.body.contains("\"data\""))
    },
    test("accepts a known-length JSON body within the configured limit") {
      val body = """{"query":"{ characters { name } }"}"""

      for {
        response <- execute(
                      basicRequest
                        .post(uri"http://localhost:8090/api/graphql-default")
                        .contentType("application/json")
                        .body(body)
                        .contentLength(body.getBytes.length.toLong)
                        .response(asStringAlways)
                    )
      } yield assertTrue(response.is200, response.body.contains("\"data\""))
    },
    test("returns 406 for an upload route with an unacceptable response type") {
      for {
        response <- execute(
                      basicRequest
                        .post(uri"http://localhost:8090/upload/graphql")
                        .header("Accept", "text/plain")
                        .multipartBody(uploadParts("content".getBytes))
                        .response(asStringAlways)
                    )
      } yield assertTrue(response.code.code == 406)
    },
    test("does not apply the one-megabyte JSON default to uploads") {
      val largeFile = Array.fill[Byte](1024 * 1024 + 1)('x'.toByte)

      for {
        response <- execute(
                      basicRequest
                        .post(uri"http://localhost:8090/upload/graphql-default")
                        .multipartBody(uploadParts(largeFile))
                        .response(asStringAlways)
                    )
      } yield assertTrue(response.is200)
    },
    test("uses query parameters for POST when query is present") {
      val query    = "{ characters { name } }"
      val endpoint = uri"http://localhost:8090/api/graphql?query=$query"

      for {
        response <- execute(basicRequest.post(endpoint).response(asStringAlways))
      } yield assertTrue(response.is200, response.body.contains("\"data\""))
    },
    test("returns an observable error when response encoding exceeds its limit") {
      val body = """{"query":"{ characters { name } }"}"""

      for {
        response <- execute(
                      basicRequest
                        .post(uri"http://localhost:8090/api/graphql-small-response")
                        .contentType("application/json")
                        .body(body)
                        .response(asStringAlways)
                    )
      } yield assertTrue(
        response.code.code == 500,
        response.body.nonEmpty,
        response.body.contains("exceeds the configured limit")
      )
    },
    test("keeps GraphQL request errors on an SSE response at status 200") {
      for {
        response <- execute(
                      basicRequest
                        .get(uri"http://localhost:8090/api/graphql?query=%7B")
                        .header("Accept", "text/event-stream")
                        .response(asStringAlways)
                    )
      } yield assertTrue(
        response.code.code == 200,
        response.body.contains("event: next"),
        response.body.contains("errors")
      )
    },
    test("emits an SSE error event when response encoding exceeds its limit") {
      val body = """{"query":"{ characters { name } }"}"""

      for {
        response <- execute(
                      basicRequest
                        .post(uri"http://localhost:8090/api/graphql-small-response")
                        .contentType("application/json")
                        .header("Accept", "text/event-stream")
                        .body(body)
                        .response(asStringAlways)
                    )
      } yield assertTrue(
        response.code.code == 200,
        response.body.contains("event: next"),
        response.body.contains("exceeds the configured limit")
      )
    }
  )

  private def execute[T](request: sttp.client4.Request[T]): Task[sttp.client4.Response[T]] =
    ZIO.scoped[Any](HttpClientZioBackend.scoped().flatMap(request.send(_)))

  private def uploadParts(file: Array[Byte]) = {
    val operations =
      """{"query":"mutation ($files: [Upload!]!) { uploadFiles(files: $files) { filename } }","variables":{"files":[null]}}"""
    List(
      multipart("operations", operations.getBytes).contentType(sttp.model.MediaType.ApplicationJson),
      multipart("map", """{"0":["variables.files.0"]}""".getBytes),
      multipart("0", file).contentType(sttp.model.MediaType.TextPlain).fileName("large.txt")
    )
  }
}

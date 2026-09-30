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
                   val coded         = interpreter.mapError {
                     case e: CalibanError.ValidationError =>
                       e.copy(extensions =
                         Some(ResponseValue.ObjectValue(List("code" -> Value.StringValue("BAD_REQUEST"))))
                       )
                     case e                               => e
                   }
                   val default       = QuickAdapter(coded).configureSse(SseConfig(Some(1.second)))
                   val existing      = default.configureHttp(HttpConfig.default.withMaxRequestBodyBytes(Int.MaxValue - 2))
                   val smallResponse = default.configureHttp(HttpConfig.default.withMaxResponseBodyBytes(64))
                   val rebuilt       = QuickAdapter(
                     coded.wrapExecutionWith[TestService with Uploads, CalibanError](
                       _.map(r => GraphQLResponse(r.data, r.errors, r.extensions))
                     )
                   )

                   (existing.routes(
                     "/api/graphql",
                     uploadPath = Some("/upload/graphql"),
                     webSocketPath = Some("/ws/graphql")
                   ) ++
                     default.routes(
                       "/api/graphql-default",
                       uploadPath = Some("/upload/graphql-default")
                     ) ++
                     smallResponse.routes("/api/graphql-small-response") ++
                     rebuilt.routes("/api/graphql-rebuilt")) @@ auth
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
    test("negotiates the response media type from Accept like an absent Accept header") {
      val json     = "application/json"
      val expected = List(
        "*/*"                                                  -> json,
        "*/*;charset=utf-8"                                    -> json,
        "text/plain, */*"                                      -> json,
        "application/json, text/plain, */*"                    -> json,
        "text/event-stream, */*"                               -> "text/event-stream",
        "multipart/mixed;deferSpec=20220824, application/json" -> json,
        "application/json; charset=utf-8"                      -> json,
        "application/json; charset=UTF8"                       -> json,
        "application/json; charset=iso-8859-1"                 -> json,
        "application/json; profile=custom"                     -> json
      )
      for {
        absent     <- execute(postQuery(apiUri))
        negotiated <- ZIO.foreach(expected) { case (accept, _) =>
                        execute(postQuery(apiUri).header("Accept", accept)).map(r => (r.code.code, r.contentType))
                      }
      } yield assertTrue(
        absent.is200,
        absent.contentType.contains(json),
        absent.body.contains("\"data\""),
        absent.header("Cache-Control").nonEmpty,
        !absent.body.contains("cacheControl"),
        negotiated == expected.map { case (_, mediaType) => (200, Some(mediaType)) }
      )
    },
    test("rejects text/plain and decodes other request media types, including legacy graphql+json, as JSON") {
      val expected = List(
        "application/graphql+json"          -> 200,
        "text/plain;charset=UTF-8"          -> 415,
        "application/x-www-form-urlencoded" -> 200
      )
      ZIO.foreach(expected) { case (contentType, _) => execute(postQuery(apiUri, contentType)) }.map { responses =>
        assertTrue(
          responses.map(_.code.code) == expected.map(_._2),
          responses.filter(_.is200).forall(_.body.contains("\"data\""))
        )
      }
    },
    test("accepts a known-length JSON body within the configured limit") {
      for {
        response <- execute(
                      postQuery(defaultUri)
                        .contentLength(CharactersQuery.getBytes.length.toLong)
                    )
      } yield assertTrue(response.is200, response.body.contains("\"data\""))
    },
    test("responds with JSON for an upload route with an unacceptable response type") {
      for {
        response <- execute(
                      basicRequest
                        .post(uri"http://localhost:8090/upload/graphql")
                        .header("Accept", "text/plain")
                        .multipartBody(uploadParts("content".getBytes))
                        .response(asStringAlways)
                    )
      } yield assertTrue(response.is200, response.contentType.contains("application/json"))
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
    test("keeps 405 for a mutation over GET when mapError rewrites the error") {
      val query = """mutation{ deleteCharacter(name: "Amos Burton") }"""
      for {
        response <- execute(basicRequest.get(uri"$defaultUri?query=$query").response(asStringAlways))
      } yield assertTrue(response.code.code == 405, response.header("Allow").contains("POST"))
    },
    test("returns an observable error, or an SSE error event, when response encoding exceeds its limit") {
      for {
        json <- execute(postQuery(smallResponseUri))
        sse  <- execute(postQuery(smallResponseUri).header("Accept", "text/event-stream"))
      } yield assertTrue(
        json.code.code == 500,
        json.contentType.contains("application/json"),
        json.body.contains("exceeds the configured limit"),
        sse.code.code == 200,
        sse.body.contains("event: next"),
        sse.body.contains("exceeds the configured limit")
      )
    },
    test("ends an @defer response after a payload exceeds the response limit") {
      val query =
        """{ characters { ... on Character @defer(label: "character") { name ... @defer(label: "nicknames") { labels } } } }"""
      for {
        response <-
          execute(
            basicRequest.post(smallResponseUri).contentType("application/graphql").body(query).response(asStringAlways)
          )
      } yield assertTrue(
        response.body.split("exceeds the configured limit", -1).length == 2,
        !response.body.contains("incremental")
      )
    },
    test("rejects a subscription over JSON with a GraphQL error response") {
      def reject(response: UIO[GraphQLResponse[CalibanError]]) =
        for {
          response <- respond(response, "application/json")
          body     <- response.body.asString
        } yield assertTrue(
          response.status == Status.BadRequest,
          response.headers.get(Header.ContentType).exists(_.mediaType == MediaType.application.json),
          body == """{"data":null,"errors":[{"message":"Subscriptions require text/event-stream or WebSocket."}]}"""
        )
      val stream                                               = ResponseValue.StreamValue(zio.stream.ZStream.empty)
      (reject(GraphQLResponseContext.markSubscribed.as(GraphQLResponse(stream, Nil))) <*>
        reject(ZIO.succeed(GraphQLResponse(ResponseValue.ObjectValue(List("characterDeleted" -> stream)), Nil)))).map {
        case (a, b) => a && b
      }
    },
    test("negotiates each result kind separately for urql's default Accept header") {
      val accept                          =
        "application/graphql-response+json, application/graphql+json, application/json, text/event-stream, multipart/mixed"
      def negotiated(data: ResponseValue) =
        respond(ZIO.succeed(GraphQLResponse(data, Nil)), accept).map { response =>
          (response.status, response.headers.get(Header.ContentType).map(_.mediaType.fullType))
        }
      val subscription                    = ResponseValue.ObjectValue(
        List("characterDeleted" -> ResponseValue.StreamValue(zio.stream.ZStream.empty))
      )
      for {
        single <- negotiated(ResponseValue.ObjectValue(List("name" -> Value.StringValue("Amos"))))
        stream <- negotiated(subscription)
      } yield assertTrue(
        single == ((Status.Ok, Some("application/graphql-response+json"))),
        stream == ((Status.Ok, Some("text/event-stream")))
      )
    },
    test("keeps @defer on multipart when a wrapper rebuilds the response without hasNext") {
      val query =
        """{ characters { ... on Character @defer(label: "character") { name ... @defer(label: "nicknames") { labels } } } }"""
      for {
        response <- execute(
                      basicRequest
                        .post(uri"http://localhost:8090/api/graphql-rebuilt")
                        .contentType("application/graphql")
                        .header("Accept", "text/event-stream, multipart/mixed")
                        .body(query)
                        .response(asStringAlways)
                    )
      } yield assertTrue(
        response.is200,
        response.contentType.exists(_.startsWith("multipart/mixed")),
        response.body.contains("incremental")
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
    }
  )

  private val CharactersQuery  = """{"query":"{ characters { name } }"}"""
  private val apiUri           = uri"http://localhost:8090/api/graphql"
  private val smallResponseUri = uri"http://localhost:8090/api/graphql-small-response"
  private val defaultUri       = uri"http://localhost:8090/api/graphql-default"

  private def postQuery(endpoint: sttp.model.Uri, contentType: String = "application/json") =
    basicRequest.post(endpoint).contentType(contentType).body(CharactersQuery).response(asStringAlways)

  private def respond(response: UIO[GraphQLResponse[CalibanError]], accept: String) = {
    val interpreter = new GraphQLInterpreter[Any, CalibanError] {
      def check(query: String)(implicit trace: Trace)                    = ZIO.unit
      def executeRequest(request: GraphQLRequest)(implicit trace: Trace) = response
    }
    QuickAdapter(interpreter).handlers.api.runZIO(
      Request
        .post(URL.empty, Body.fromString(CharactersQuery))
        .addHeader(Header.ContentType(MediaType.application.json))
        .addHeader(Header.Custom("Accept", accept))
    )
  }

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

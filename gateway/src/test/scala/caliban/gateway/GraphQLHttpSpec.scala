package caliban.gateway

import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.GraphQLSource._
import caliban.gateway.internal.RemoteGraphQLSource
import caliban.{ GraphQLRequest, ResponseValue }
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.model.Uri
import zio._
import zio.http.{ Body, Handler, Header, Headers, Method, Request, Response, Routes, Server, Status }
import zio.stream.ZStream
import zio.test._

import java.nio.charset.StandardCharsets

object GraphQLHttpSpec extends ZIOSpecDefault {

  private val schema  = "type Query { value(input: String): String }"
  private val request = GraphQLRequest(query = Some("query Value { value }"), operationName = Some("Value"))

  private def endpoint(handler: Request => UIO[Response]): ZIO[Server with Ref[Int], Nothing, Uri] =
    for {
      id     <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
      path    = s"graphql-http-$id"
      server <- ZIO.service[Server]
      _      <- server.install(Routes(Method.POST / path -> Handler.fromFunctionZIO(handler)))
      port   <- server.port
    } yield Uri.unsafeParse(s"http://127.0.0.1:$port/$path")

  private def fixed(status: Status, mediaType: Option[String], body: String): ZIO[Server with Ref[Int], Nothing, Uri] =
    endpoint { _ =>
      val headers = mediaType.fold(Headers.empty)(value => Headers(Header.Custom("Content-Type", value)))
      ZIO.succeed(Response(status, headers, Body.fromString(body)))
    }

  private def streaming(
    stream: ZStream[Any, Throwable, Byte],
    status: Status = Status.Ok,
    mediaType: String = "application/graphql-response+json"
  ): ZIO[Server with Ref[Int], Nothing, Uri] =
    endpoint(_ =>
      ZIO.succeed(
        Response(
          status,
          Headers(Header.Custom("Content-Type", mediaType)),
          Body.fromStreamChunked(stream)
        )
      )
    )

  private def finite(body: String, releases: Ref[Int], released: Promise[Nothing, Unit]) =
    ZStream
      .fromChunk(Chunk.fromArray(body.getBytes(StandardCharsets.UTF_8)))
      .ensuring(releases.update(_ + 1) *> released.succeed(()).unit)

  def spec = suite("GraphQLHttpSpec")(
    test("classifies status, media type, malformed envelopes, and redirects") {
      val valid = """{"data":{"value":"ok"}}"""

      for {
        backend             <- HttpClientZioBackend.scoped()
        graphQlError        <- fixed(
                                 Status.ServiceUnavailable,
                                 Some("application/graphql-response+json; charset=utf-8"),
                                 """{"errors":[{"message":"unavailable"}]}"""
                               )
        legacySuccess       <- fixed(Status.Ok, Some("application/json"), valid)
        legacyFailure       <- fixed(Status.ServiceUnavailable, Some("application/json"), valid)
        textFailure         <- fixed(Status.ServiceUnavailable, Some("text/plain"), "overloaded")
        untypedFailure      <- fixed(Status.ServiceUnavailable, None, "overloaded")
        malformed           <- fixed(Status.Ok, Some("application/graphql-response+json"), "{")
        empty               <- fixed(Status.Ok, Some("application/graphql-response+json"), "")
        unsupported         <- fixed(Status.Ok, Some("text/plain"), valid)
        redirectCalls       <- Ref.make(0)
        redirectTarget      <- endpoint(_ => redirectCalls.update(_ + 1).as(Response.json(valid)))
        redirect            <- endpoint(_ =>
                                 ZIO.succeed(
                                   Response(
                                     Status.TemporaryRedirect,
                                     Headers(Header.Custom("Location", redirectTarget.toString)),
                                     Body.empty
                                   )
                                 )
                               )
        graphQlResult       <- new RemoteGraphQLSource(graphQlError, backend).execute(request).either
        legacyResult        <- new RemoteGraphQLSource(legacySuccess, backend).execute(request).either
        statusResult        <- new RemoteGraphQLSource(legacyFailure, backend).execute(request).either
        textStatusResult    <- new RemoteGraphQLSource(textFailure, backend).execute(request).either
        untypedStatusResult <- new RemoteGraphQLSource(untypedFailure, backend).execute(request).either
        malformedResult     <- new RemoteGraphQLSource(malformed, backend).execute(request).either
        emptyResult         <- new RemoteGraphQLSource(empty, backend).execute(request).either
        unsupportedResult   <- new RemoteGraphQLSource(unsupported, backend).execute(request).either
        redirectResult      <- new RemoteGraphQLSource(redirect, backend).execute(request).either
        followed            <- redirectCalls.get
      } yield assertTrue(
        graphQlResult.exists(_.errors.map(_.msg) == List("unavailable")),
        legacyResult.exists(_.data == ObjectValue(List("value" -> StringValue("ok")))),
        statusResult == Left(HttpFailure(503)),
        textStatusResult == Left(HttpFailure(503)),
        untypedStatusResult == Left(HttpFailure(503)),
        malformedResult == Left(InvalidResponse),
        emptyResult == Left(InvalidResponse),
        unsupportedResult == Left(UnsupportedMediaType),
        redirectResult == Left(RedirectResponse),
        followed == 0
      )
    },
    test("enforces request, response byte, nesting, and structure limits") {
      val limits = RemoteGraphQLSource.Limits(
        timeout = 5.seconds,
        maxRequestBytes = 96,
        maxResponseBytes = 512,
        maxResponseDepth = 5,
        maxResponseTokens = 12
      )

      for {
        backend         <- HttpClientZioBackend.scoped()
        requestCalls    <- Ref.make(0)
        requestEndpoint <- endpoint(_ =>
                             requestCalls
                               .update(_ + 1)
                               .as(Response.json("""{"data":{"value":"ok"}}"""))
                           )
        oversizedBody   <- fixed(
                             Status.Ok,
                             Some("application/graphql-response+json"),
                             s"""{"data":{"value":"${"x" * 600}"}}"""
                           )
        nestedBody      <- fixed(
                             Status.Ok,
                             Some("application/graphql-response+json"),
                             """{"data":{"value":[[[[["x"]]]]]}}"""
                           )
        structuredBody  <- fixed(
                             Status.Ok,
                             Some("application/graphql-response+json"),
                             """{"data":{"value":[0,1,2,3,4,5,6,7,8,9,10,11]}}"""
                           )
        largeRequest     = request.copy(variables = Some(Map("secret" -> StringValue("x" * 200))))
        requestResult   <- new RemoteGraphQLSource(requestEndpoint, backend, limits).execute(largeRequest).either
        responseResult  <- new RemoteGraphQLSource(oversizedBody, backend, limits).execute(request).either
        nestingResult   <- new RemoteGraphQLSource(nestedBody, backend, limits).execute(request).either
        structureResult <- new RemoteGraphQLSource(structuredBody, backend, limits).execute(request).either
        calls           <- requestCalls.get
      } yield assertTrue(
        requestResult == Left(RequestTooLarge),
        responseResult == Left(ResponseTooLarge),
        nestingResult == Left(ResponseNestingTooDeep),
        structureResult == Left(ResponseStructureTooLarge),
        calls == 0
      )
    },
    test("drops request extensions and masks protocol details through GatewayRuntime") {
      for {
        captured <- Promise.make[Nothing, (GraphQLRequest, Headers)]
        remote   <- endpoint { incoming =>
                      for {
                        bytes  <- incoming.body.asArray.orDie
                        decoded = readFromArray[GraphQLRequest](bytes)
                        _      <- captured.succeed(decoded -> incoming.headers)
                      } yield Response(
                        Status.Ok,
                        Headers(
                          Header.Custom("Content-Type", "text/plain"),
                          Header.Custom("X-Internal", "source-secret")
                        ),
                        Body.fromString("source-secret-body")
                      )
                    }
        gateway  <- Gateway.compose(Subgraph.graphql("remote", remote, schema)).build
        outbound  = GraphQLRequest(
                      query = Some("query Value($input: String) { value(input: $input) }"),
                      operationName = Some("Value"),
                      variables = Some(Map("input" -> StringValue("한국어-secret"))),
                      extensions = Some(Map("private" -> StringValue("extension-secret")))
                    )
        response <- gateway.executeRequest(outbound)
        sent     <- captured.await
        rendered  = response.errors.map(_.msg).mkString(" ")
      } yield assertTrue(
        sent._1 == outbound.copy(extensions = None),
        sent._2.get("Content-Type").exists(_.startsWith("application/json; charset=utf-8")),
        sent._2.get("Accept").exists(_.contains("application/graphql-response+json")),
        response.errors.map(_.msg) == List("Remote GraphQL request failed."),
        !rendered.contains("source-secret"),
        !rendered.contains("한국어-secret"),
        !rendered.contains("extension-secret")
      )
    },
    test("releases response streams once on success, failure, size failure, timeout, and interruption") {
      val small = RemoteGraphQLSource.Limits.default.copy(maxResponseBytes = 32)
      val short = RemoteGraphQLSource.Limits.default.copy(timeout = 1.second)

      for {
        backend             <- HttpClientZioBackend.scoped()
        successReleases     <- Ref.make(0)
        successReleased     <- Promise.make[Nothing, Unit]
        successEndpoint     <- streaming(
                                 finite("""{"data":{"value":"ok"}}""", successReleases, successReleased)
                               )
        success             <- new RemoteGraphQLSource(successEndpoint, backend).execute(request).either
        _                   <- successReleased.await
        failureReleases     <- Ref.make(0)
        failureReleased     <- Promise.make[Nothing, Unit]
        failureEndpoint     <- streaming(finite("not-json", failureReleases, failureReleased))
        failure             <- new RemoteGraphQLSource(failureEndpoint, backend).execute(request).either
        _                   <- failureReleased.await
        sizeReleases        <- Ref.make(0)
        sizeReleased        <- Promise.make[Nothing, Unit]
        sizeEndpoint        <- streaming(finite("x" * 128, sizeReleases, sizeReleased))
        sizeFailure         <- new RemoteGraphQLSource(sizeEndpoint, backend, small).execute(request).either
        _                   <- sizeReleased.await
        timeoutStarted      <- Promise.make[Nothing, Unit]
        timeoutReleases     <- Ref.make(0)
        timeoutReleased     <- Promise.make[Nothing, Unit]
        timeoutEndpoint     <- streaming(
                                 (ZStream.fromZIO(timeoutStarted.succeed(()).unit).drain ++ ZStream.never)
                                   .ensuring(timeoutReleases.update(_ + 1) *> timeoutReleased.succeed(()).unit)
                               )
        timeoutFiber        <- new RemoteGraphQLSource(timeoutEndpoint, backend, short).execute(request).either.fork
        _                   <- timeoutStarted.await
        _                   <- TestClock.adjust(2.seconds)
        timeoutFailure      <- timeoutFiber.join
        _                   <- timeoutReleased.await
        interruptStarted    <- Promise.make[Nothing, Unit]
        interruptReleases   <- Ref.make(0)
        interruptReleased   <- Promise.make[Nothing, Unit]
        interruptEndpoint   <- streaming(
                                 (ZStream.fromZIO(interruptStarted.succeed(()).unit).drain ++ ZStream.never)
                                   .ensuring(interruptReleases.update(_ + 1) *> interruptReleased.succeed(()).unit)
                               )
        interruptFiber      <- new RemoteGraphQLSource(interruptEndpoint, backend).execute(request).fork
        _                   <- interruptStarted.await
        interrupted         <- interruptFiber.interrupt
        _                   <- interruptReleased.await
        successReleaseCount <- successReleases.get
        failureReleaseCount <- failureReleases.get
        sizeReleaseCount    <- sizeReleases.get
        timeoutReleaseCount <- timeoutReleases.get
        interruptCount      <- interruptReleases.get
      } yield assertTrue(
        success.isRight,
        failure == Left(InvalidResponse),
        sizeFailure == Left(ResponseTooLarge),
        timeoutFailure == Left(TimeoutFailure),
        interrupted.isInterrupted,
        successReleaseCount == 1,
        failureReleaseCount == 1,
        sizeReleaseCount == 1,
        timeoutReleaseCount == 1,
        interruptCount == 1
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

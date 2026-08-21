package caliban.gateway

import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.GraphQLSource._
import caliban.gateway.internal.RemoteGraphQLSource
import caliban.parsing.adt.OperationType
import caliban.{ GraphQLRequest, ResponseValue }
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.model.{ Header => SttpHeader, Uri }
import zio._
import zio.http.{ Body, Handler, Header, Headers, Method, Request, Response, Routes, Server, Status }
import zio.stream.ZStream
import zio.test._

import java.nio.charset.StandardCharsets

object GraphQLHttpSpec extends ZIOSpecDefault {

  private trait RuntimeHeaders {
    def values: UIO[List[SttpHeader]]
  }

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
      val valid     = """{"data":{"value":"ok"}}"""
      val disclosed = RemoteGraphQLConfig.default.withErrorDisclosure(_.withMessages(true))

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
        malformedMetadata   <- fixed(
                                 Status.Ok,
                                 Some("application/graphql-response+json"),
                                 """{"data":{"value":null},"errors":[{"message":"failed","path":"value"}]}"""
                               )
        incremental         <- fixed(
                                 Status.Ok,
                                 Some("application/graphql-response+json"),
                                 """{"data":{"value":"ok"},"hasNext":true}"""
                               )
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
        graphQlResult       <- RemoteGraphQLSource(graphQlError, backend, disclosed)
                                 .execute(request, OperationType.Query)
                                 .either
        legacyResult        <- RemoteGraphQLSource(legacySuccess, backend).execute(request, OperationType.Query).either
        statusResult        <- RemoteGraphQLSource(legacyFailure, backend).execute(request, OperationType.Query).either
        textStatusResult    <- RemoteGraphQLSource(textFailure, backend).execute(request, OperationType.Query).either
        untypedStatusResult <- RemoteGraphQLSource(untypedFailure, backend).execute(request, OperationType.Query).either
        malformedResult     <- RemoteGraphQLSource(malformed, backend).execute(request, OperationType.Query).either
        metadataResult      <- RemoteGraphQLSource(malformedMetadata, backend)
                                 .execute(request, OperationType.Query)
                                 .either
        incrementalResult   <- RemoteGraphQLSource(incremental, backend).execute(request, OperationType.Query).either
        emptyResult         <- RemoteGraphQLSource(empty, backend).execute(request, OperationType.Query).either
        unsupportedResult   <- RemoteGraphQLSource(unsupported, backend).execute(request, OperationType.Query).either
        redirectResult      <- RemoteGraphQLSource(redirect, backend).execute(request, OperationType.Query).either
        followed            <- redirectCalls.get
      } yield assertTrue(
        graphQlResult.exists(_.errors.map(_.msg) == List("unavailable")),
        legacyResult.exists(_.data == ObjectValue(List("value" -> StringValue("ok")))),
        statusResult == Left(HttpFailure(503)),
        textStatusResult == Left(HttpFailure(503)),
        untypedStatusResult == Left(HttpFailure(503)),
        malformedResult == Left(InvalidResponse),
        metadataResult == Left(InvalidResponse),
        incrementalResult == Left(InvalidResponse),
        emptyResult == Left(InvalidResponse),
        unsupportedResult == Left(UnsupportedMediaType),
        redirectResult == Left(RedirectResponse),
        followed == 0
      )
    },
    test("enforces request, response byte, nesting, and structure limits") {
      val config     = RemoteGraphQLConfig.default.withExecution(
        _.withTimeout(5.seconds)
          .withMaxRequestBytes(96)
          .withMaxResponseBytes(512)
      )
      val structural = RemoteGraphQLSource.StructuralLimits(
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
        requestResult   <- RemoteGraphQLSource(requestEndpoint, backend, config, structural)
                             .execute(largeRequest, OperationType.Query)
                             .either
        responseResult  <-
          RemoteGraphQLSource(oversizedBody, backend, config, structural).execute(request, OperationType.Query).either
        nestingResult   <-
          RemoteGraphQLSource(nestedBody, backend, config, structural).execute(request, OperationType.Query).either
        structureResult <-
          RemoteGraphQLSource(structuredBody, backend, config, structural).execute(request, OperationType.Query).either
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
      val small = RemoteGraphQLConfig.default.withExecution(_.withMaxResponseBytes(32))
      val short = RemoteGraphQLConfig.default.withExecution(_.withTimeout(200.millis))

      Live.live {
        for {
          backend             <- HttpClientZioBackend.scoped()
          successReleases     <- Ref.make(0)
          successReleased     <- Promise.make[Nothing, Unit]
          successEndpoint     <- streaming(
                                   finite("""{"data":{"value":"ok"}}""", successReleases, successReleased)
                                 )
          success             <- RemoteGraphQLSource(successEndpoint, backend).execute(request, OperationType.Query).either
          _                   <- successReleased.await
          failureReleases     <- Ref.make(0)
          failureReleased     <- Promise.make[Nothing, Unit]
          failureEndpoint     <- streaming(finite("not-json", failureReleases, failureReleased))
          failure             <- RemoteGraphQLSource(failureEndpoint, backend).execute(request, OperationType.Query).either
          _                   <- failureReleased.await
          sizeReleases        <- Ref.make(0)
          sizeReleased        <- Promise.make[Nothing, Unit]
          sizeEndpoint        <- streaming(finite("x" * 128, sizeReleases, sizeReleased))
          sizeFailure         <- RemoteGraphQLSource(sizeEndpoint, backend, small).execute(request, OperationType.Query).either
          _                   <- sizeReleased.await
          timeoutStarted      <- Promise.make[Nothing, Unit]
          timeoutReleases     <- Ref.make(0)
          timeoutReleased     <- Promise.make[Nothing, Unit]
          timeoutEndpoint     <- streaming(
                                   (ZStream.fromZIO(timeoutStarted.succeed(()).unit).drain ++ ZStream.never)
                                     .ensuring(timeoutReleases.update(_ + 1) *> timeoutReleased.succeed(()).unit)
                                 )
          timeoutFailure      <-
            RemoteGraphQLSource(timeoutEndpoint, backend, short).execute(request, OperationType.Query).either
          _                   <- timeoutStarted.await
          _                   <- timeoutReleased.await
          interruptStarted    <- Promise.make[Nothing, Unit]
          interruptComplete   <- Promise.make[Nothing, Unit]
          interruptReleases   <- Ref.make(0)
          interruptReleased   <- Promise.make[Nothing, Unit]
          interruptEndpoint   <- streaming(
                                   (ZStream.fromZIO(interruptStarted.succeed(()).unit).drain ++
                                     ZStream.fromZIO(interruptComplete.await).drain)
                                     .ensuring(interruptReleases.update(_ + 1) *> interruptReleased.succeed(()).unit)
                                 )
          interruptFiber      <- RemoteGraphQLSource(interruptEndpoint, backend).execute(request, OperationType.Query).fork
          _                   <- interruptStarted.await
          _                   <- interruptFiber.interruptFork
          _                   <- interruptComplete.succeed(())
          interrupted         <- interruptFiber.await
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
    },
    test("validates finite source policy with accumulated source diagnostics") {
      val firstPolicy  = RemoteGraphQLConfig.default
        .withAcquisition(_.withMaxResponseBytes(0))
        .withExecution(
          _.withTimeout(Duration.Zero)
            .withMaxRequestBytes(0)
            .withRetries(-1, Duration.Infinity)
        )
      val secondPolicy = RemoteGraphQLConfig.default
        .withExecution(_.withMaxResponseBytes(0))
        .withExecution(
          _.withMaxConcurrentCalls(0)
            .withHeaders(SttpHeader("Content-Type", "text/plain"))
            .forwardIncomingHeaders("Accept")
        )

      for {
        remote <- stub("""{"data":{"value":"ok"}}""")
        exit   <- Gateway
                    .compose(
                      Subgraph.graphql("first", remote.endpoint, firstPolicy),
                      Subgraph.graphql("second", remote.endpoint, schema, secondPolicy)
                    )
                    .build
                    .exit
        calls  <- remote.requests.get
        errors  = buildDiagnostics(exit)
      } yield assertTrue(
        calls.isEmpty,
        errors.count(_.startsWith("[first]")) == 5,
        errors.count(_.startsWith("[second]")) == 4,
        errors.exists(_.contains("timeout must be finite and positive")),
        errors.exists(_.contains("retry backoff must be finite and non-negative")),
        errors.exists(_.contains("header 'Content-Type' is owned"))
      )
    },
    test("combines selected incoming, static, and effectful headers with safe precedence") {
      val policy      = RemoteGraphQLConfig.default
        .withExecution(
          _.forwardIncomingHeaders("X-Forwarded", "X-Precedence")
            .withHeaders(
              SttpHeader("X-Static", "static"),
              SttpHeader("X-Precedence", "static")
            )
        )
        .withExecutionHeadersZIO(
          ZIO.serviceWithZIO[RuntimeHeaders](_.values)
        )
      val environment = new RuntimeHeaders {
        def values: UIO[List[SttpHeader]] =
          ZIO.succeed(
            List(
              SttpHeader("X-Effect", "effect"),
              SttpHeader("X-Precedence", "effect"),
              SttpHeader("X-Multi", "first"),
              SttpHeader("X-Multi", "second"),
              SttpHeader("Accept", "text/plain")
            )
          )
      }

      for {
        remote   <- stub("""{"data":{"value":"ok"}}""")
        gateway  <- Gateway.compose(Subgraph.graphql("remote", remote.endpoint, schema, policy)).build
        response <- gateway
                      .executeRequest(
                        request,
                        List(
                          SttpHeader("X-Forwarded", "incoming"),
                          SttpHeader("X-Precedence", "incoming"),
                          SttpHeader("X-Ignored", "ignored"),
                          SttpHeader("Content-Type", "text/plain")
                        )
                      )
                      .provideEnvironment(ZEnvironment(environment))
        sent     <- remote.headers.get
        headers   = sent.headOption
        multi     = headers.fold(List.empty[String])(
                      _.iterator
                        .filter(_.headerName.equalsIgnoreCase("X-Multi"))
                        .map(_.renderedValue)
                        .toList
                    )
      } yield assertTrue(
        response.errors.isEmpty,
        headers.flatMap(_.get("X-Forwarded")).contains("incoming"),
        headers.flatMap(_.get("X-Static")).contains("static"),
        headers.flatMap(_.get("X-Effect")).contains("effect"),
        headers.flatMap(_.get("X-Precedence")).contains("effect"),
        multi == List("first", "second"),
        headers.flatMap(_.get("X-Ignored")).isEmpty,
        headers.flatMap(_.get("Content-Type")).exists(_.startsWith("application/json")),
        headers.flatMap(_.get("Accept")).exists(_.contains("application/graphql-response+json"))
      )
    },
    test("forwards all incoming headers only when explicitly enabled") {
      val config = RemoteGraphQLConfig.default
        .withExecution(_.forwardAllIncomingHeaders)
        .withExecutionHeadersZIO(
          ZIO.succeed(
            List(
              SttpHeader("Connection", "X-Effect-Hop"),
              SttpHeader("X-Effect-Hop", "hop-by-hop")
            )
          )
        )

      for {
        remote  <- stub("""{"data":{"value":"ok"}}""")
        gateway <- Gateway.compose(Subgraph.graphql("remote", remote.endpoint, schema, config)).build
        _       <- gateway.executeRequest(
                     request,
                     List(
                       SttpHeader("Authorization", "Bearer incoming"),
                       SttpHeader("X-Incoming", "forwarded"),
                       SttpHeader("Connection", "keep-alive, X-Internal"),
                       SttpHeader("X-Internal", "hop-by-hop"),
                       SttpHeader("Accept", "text/plain")
                     )
                   )
        sent    <- remote.headers.get
        headers  = sent.headOption
      } yield assertTrue(
        headers.flatMap(_.get("Authorization")).contains("Bearer incoming"),
        headers.flatMap(_.get("X-Incoming")).contains("forwarded"),
        headers.flatMap(_.get("X-Internal")).isEmpty,
        headers.flatMap(_.get("X-Effect-Hop")).isEmpty,
        headers.flatMap(_.get("Accept")).exists(_.contains("application/graphql-response+json"))
      )
    },
    test("masks effectful header failures without dispatching the source call") {
      val secret = "header-provider-secret"
      val config = RemoteGraphQLConfig.default.withExecutionHeadersZIO(
        ZIO.fail(new RuntimeException(secret))
      )

      for {
        remote   <- stub("""{"data":{"value":"ok"}}""")
        gateway  <- Gateway.compose(Subgraph.graphql("remote", remote.endpoint, schema, config)).build
        response <- gateway.executeRequest(request)
        calls    <- remote.requests.get
        rendered  = response.errors.map(_.msg).mkString(" ")
      } yield assertTrue(
        calls.isEmpty,
        response.errors.map(_.msg) == List("Remote GraphQL request failed."),
        !rendered.contains(secret)
      )
    },
    test("retries one logical replay-safe call and never retries mutations or GraphQL envelopes") {
      val policy = RemoteGraphQLConfig.default.withExecution(
        _.withRetries(2, Duration.Zero)
      )

      def unavailable: Response =
        Response(
          Status.ServiceUnavailable,
          Headers(Header.Custom("Content-Type", "text/plain")),
          Body.fromString("unavailable")
        )

      for {
        backend          <- HttpClientZioBackend.scoped()
        queryCalls       <- Ref.make(0)
        headerCalls      <- Ref.make(0)
        queryEndpoint    <- endpoint { _ =>
                              queryCalls.updateAndGet(_ + 1).map { attempt =>
                                if (attempt < 3) unavailable else Response.json("""{"data":{"value":"ok"}}""")
                              }
                            }
        countedPolicy     = policy.withExecutionHeadersZIO(
                              headerCalls.updateAndGet(_ + 1).as(List(SttpHeader("Authorization", "secret")))
                            )
        queryResult      <-
          RemoteGraphQLSource(queryEndpoint, backend, countedPolicy).execute(request, OperationType.Query).either
        queryAttempts    <- queryCalls.get
        policyRuns       <- headerCalls.get
        mutationCalls    <- Ref.make(0)
        mutationEndpoint <- endpoint(_ => mutationCalls.update(_ + 1).as(unavailable))
        mutationRequest   = GraphQLRequest(query = Some("mutation Update { value }"), operationName = Some("Update"))
        mutationResult   <-
          RemoteGraphQLSource(mutationEndpoint, backend, policy).execute(mutationRequest, OperationType.Mutation).either
        mutationAttempts <- mutationCalls.get
        rejectedCalls    <- Ref.make(0)
        rejectedEndpoint <- endpoint(_ =>
                              rejectedCalls
                                .update(_ + 1)
                                .as(
                                  Response(
                                    Status.BadRequest,
                                    Headers(Header.Custom("Content-Type", "text/plain")),
                                    Body.fromString("bad request")
                                  )
                                )
                            )
        rejectedResult   <-
          RemoteGraphQLSource(rejectedEndpoint, backend, policy).execute(request, OperationType.Query).either
        rejectedAttempts <- rejectedCalls.get
        envelopeCalls    <- Ref.make(0)
        envelopeEndpoint <- endpoint(_ =>
                              envelopeCalls
                                .update(_ + 1)
                                .as(
                                  Response(
                                    Status.ServiceUnavailable,
                                    Headers(Header.Custom("Content-Type", "application/graphql-response+json")),
                                    Body.fromString("""{"errors":[{"message":"try later"}]}""")
                                  )
                                )
                            )
        envelopeResult   <-
          RemoteGraphQLSource(envelopeEndpoint, backend, policy.withErrorDisclosure(_.withMessages(true)))
            .execute(request, OperationType.Query)
            .either
        envelopeAttempts <- envelopeCalls.get
      } yield assertTrue(
        queryResult.isRight,
        queryAttempts == 3,
        policyRuns == 1,
        mutationResult == Left(HttpFailure(503)),
        mutationAttempts == 1,
        rejectedResult == Left(HttpFailure(400)),
        rejectedAttempts == 1,
        envelopeResult.exists(_.errors.map(_.msg) == List("try later")),
        envelopeAttempts == 1
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

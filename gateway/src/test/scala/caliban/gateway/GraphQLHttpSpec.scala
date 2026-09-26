package caliban.gateway

import caliban.{ GraphQLRequest, IncomingRequestHeaders }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.execution.RemoteSubgraphExecutor
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.execution.SubgraphExecutor._
import caliban.parsing.adt.OperationType
import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import zio._
import zio.http._
import zio.stream.ZStream
import zio.test._

object GraphQLHttpSpec extends ZIOSpecDefault {

  private trait RuntimeHeaders {
    def values: UIO[List[Header]]
  }

  private val request     = GraphQLRequest(query = Some("query Value { value }"), operationName = Some("Value"))
  private val unavailable = Response(
    Status.ServiceUnavailable,
    Headers(Header.Custom("Content-Type", "text/plain")),
    Body.fromString("unavailable")
  )

  private def unmanagedRemoteSubgraphExecutor[R](
    endpoint: URL,
    http: GatewayHttpClient,
    config: RemoteGraphQLConfig[R] = RemoteGraphQLConfig.default,
    maxResponseDepth: Int = RemoteSubgraphExecutor.DefaultMaxResponseDepth,
    remoteErrorMessages: Boolean = false
  ): RemoteSubgraphExecutor[R] =
    new RemoteSubgraphExecutor(
      "remote",
      endpoint,
      http,
      config,
      maxResponseDepth,
      None,
      PhaseHooks.empty,
      remoteErrorMessages
    )

  private def call[R](
    endpoint: URL,
    http: GatewayHttpClient,
    config: RemoteGraphQLConfig[R] = RemoteGraphQLConfig.default,
    maxResponseDepth: Int = RemoteSubgraphExecutor.DefaultMaxResponseDepth,
    value: GraphQLRequest = request,
    operation: OperationType = OperationType.Query,
    remoteErrorMessages: Boolean = false
  ) =
    unmanagedRemoteSubgraphExecutor(endpoint, http, config, maxResponseDepth, remoteErrorMessages)
      .execute(value, operation)
      .either

  private def fixed(status: Status, mediaType: Option[String], body: String): ZIO[Server with Ref[Int], Nothing, URL] =
    postEndpoint("graphql-http") { _ =>
      val headers = mediaType.fold(Headers.empty)(value => Headers(Header.Custom("Content-Type", value)))
      ZIO.succeed(Response(status, headers, Body.fromString(body)))
    }

  private final case class BlockedEndpoint(
    uri: URL,
    calls: Ref[Int],
    started: Promise[Nothing, Unit],
    release: Promise[Nothing, Unit]
  )

  private def blockedEndpoint(expectedCalls: Int): ZIO[Server with Ref[Int], Nothing, BlockedEndpoint] =
    for {
      calls   <- Ref.make(0)
      started <- Promise.make[Nothing, Unit]
      release <- Promise.make[Nothing, Unit]
      uri     <- postEndpoint("graphql-http") { _ =>
                   calls.updateAndGet(_ + 1).flatMap { count =>
                     ZIO.when(count == expectedCalls)(started.succeed(()).unit) *>
                       release.await.as(Response.json(okResponse))
                   }
                 }
    } yield BlockedEndpoint(uri, calls, started, release)

  def spec = suite("GraphQLHttpSpec")(
    test("classifies status, media type, malformed envelopes, and redirects") {
      for {
        http                <- GatewayHttpClient.make
        graphQlError        <- fixed(
                                 Status.ServiceUnavailable,
                                 Some("application/graphql-response+json; charset=utf-8"),
                                 """{"errors":[{"message":"unavailable"}]}"""
                               )
        legacySuccess       <- fixed(Status.Ok, Some("application/json"), okResponse)
        legacyFailure       <- fixed(Status.ServiceUnavailable, Some("application/json"), okResponse)
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
        malformedEntries    <- fixed(
                                 Status.Ok,
                                 Some("application/graphql-response+json"),
                                 """{"data":{"value":"ok"},"errors":[{"message":"warning","path":["value",1.5]},{"path":["value"]}]}"""
                               )
        emptyErrors         <- fixed(Status.Ok, Some("application/graphql-response+json"), """{"errors":[]}""")
        emptyErrorsWithData <-
          fixed(Status.Ok, Some("application/graphql-response+json"), """{"data":{"value":"ok"},"errors":[]}""")
        unexpected          <- fixed(Status.Ok, Some("application/graphql-response+json"), invalidResponse)
        empty               <- fixed(Status.Ok, Some("application/graphql-response+json"), "")
        unsupported         <- fixed(Status.Ok, Some("text/plain"), okResponse)
        redirectCalls       <- Ref.make(0)
        redirectTarget      <- postEndpoint("graphql-http")(_ => redirectCalls.update(_ + 1).as(Response.json(okResponse)))
        redirect            <- postEndpoint("graphql-http")(_ =>
                                 ZIO.succeed(
                                   Response(
                                     Status.TemporaryRedirect,
                                     Headers(Header.Custom("Location", redirectTarget.toString)),
                                     Body.empty
                                   )
                                 )
                               )
        graphQlResult       <- call(graphQlError, http, RemoteGraphQLConfig.default, remoteErrorMessages = true)
        legacyResult        <- call(legacySuccess, http)
        statusResult        <- call(legacyFailure, http)
        textStatusResult    <- call(textFailure, http)
        untypedStatusResult <- call(untypedFailure, http)
        malformedResult     <- call(malformed, http)
        metadataResult      <- call(malformedMetadata, http)
        entriesResult       <- call(malformedEntries, http, remoteErrorMessages = true)
        emptyErrorsResult   <- call(emptyErrors, http)
        withDataResult      <- call(emptyErrorsWithData, http)
        unexpectedResult    <- call(unexpected, http)
        incrementalResult   <- call(incremental, http)
        emptyResult         <- call(empty, http)
        unsupportedResult   <- call(unsupported, http)
        redirectResult      <- call(redirect, http)
        followed            <- redirectCalls.get
        metadataPreserved    = metadataResult.exists(response =>
                                 response.data == ObjectValue(List("value" -> caliban.Value.NullValue)) &&
                                   response.errors.map(_.msg) == List("Remote GraphQL request failed.")
                               )
      } yield assertTrue(
        graphQlResult.exists(_.errors.map(_.msg) == List("unavailable")),
        legacyResult.exists(_.data == ObjectValue(List("value" -> StringValue("ok")))),
        statusResult == Left(HttpFailure(503)),
        textStatusResult == Left(HttpFailure(503)),
        untypedStatusResult == Left(HttpFailure(503)),
        malformedResult == Left(InvalidResponse),
        metadataPreserved,
        entriesResult.exists(response =>
          response.data == ObjectValue(List("value" -> StringValue("ok"))) && response.errors.size == 2
        ),
        emptyErrorsResult == Left(InvalidResponse),
        withDataResult.exists(response =>
          response.data == ObjectValue(List("value" -> StringValue("ok"))) && response.errors.isEmpty
        ),
        unexpectedResult == Left(InvalidResponse),
        incrementalResult == Left(InvalidResponse),
        emptyResult == Left(InvalidResponse),
        unsupportedResult == Left(UnsupportedMediaType),
        redirectResult == Left(RedirectResponse),
        followed == 0
      )
    },
    test("enforces request, response byte, and nesting limits") {
      val config   = RemoteGraphQLConfig.default.withExecution(
        _.withTimeout(5.seconds)
          .withMaxRequestBytes(96)
          .withMaxResponseBytes(512)
      )
      val maxDepth = 5

      for {
        http            <- GatewayHttpClient.make
        requestCalls    <- Ref.make(0)
        requestEndpoint <- postEndpoint("graphql-http")(_ =>
                             requestCalls
                               .update(_ + 1)
                               .as(Response.json(okResponse))
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
        largeRequest     = request.copy(variables = Some(Map("secret" -> StringValue("x" * 200))))
        requestResult   <- call(requestEndpoint, http, config, maxDepth, largeRequest)
        responseResult  <- call(oversizedBody, http, config, maxDepth)
        nestingResult   <- call(nestedBody, http, config, maxDepth)
        calls           <- requestCalls.get
      } yield assertTrue(
        requestResult == Left(RequestTooLarge),
        responseResult == Left(ResponseTooLarge),
        nestingResult == Left(ResponseNestingTooDeep),
        calls == 0
      )
    },
    test("drops request extensions and masks protocol details through GatewayInterpreter") {
      for {
        captured <- Promise.make[Nothing, (GraphQLRequest, Headers)]
        remote   <- postEndpoint("graphql-http") { incoming =>
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
        runtime  <- remoteGateway(remote).interpreter
        outbound  = GraphQLRequest(
                      query = Some("query Value($input: String) { value(input: $input) }"),
                      operationName = Some("Value"),
                      variables = Some(Map("input" -> StringValue("한국어-secret"))),
                      extensions = Some(Map("private" -> StringValue("extension-secret")))
                    )
        response <- runtime.executeRequest(outbound)
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
          http                                             <- GatewayHttpClient.make
          successTracked                                   <- tracked(okResponse)
          (successStream, successReleases, successReleased) = successTracked
          successEndpoint                                  <- streamingEndpoint(successStream)
          success                                          <- call(successEndpoint, http)
          _                                                <- successReleased.await
          failureTracked                                   <- tracked("not-json")
          (failureStream, failureReleases, failureReleased) = failureTracked
          failureEndpoint                                  <- streamingEndpoint(failureStream)
          failure                                          <- call(failureEndpoint, http)
          _                                                <- failureReleased.await
          sizeTracked                                      <- tracked("x" * 128)
          (sizeStream, sizeReleases, sizeReleased)          = sizeTracked
          sizeEndpoint                                     <- streamingEndpoint(sizeStream)
          sizeFailure                                      <-
            call(sizeEndpoint, http, small)
          _                                                <- sizeReleased.await
          timeoutStarted                                   <- Promise.make[Nothing, Unit]
          timeoutReleases                                  <- Ref.make(0)
          timeoutReleased                                  <- Promise.make[Nothing, Unit]
          timeoutEndpoint                                  <-
            streamingEndpoint(
              (ZStream.fromZIO(timeoutStarted.succeed(()).unit).drain ++ ZStream.never)
                .ensuring(timeoutReleases.update(_ + 1) *> timeoutReleased.succeed(()).unit)
            )
          timeoutFailure                                   <-
            call(timeoutEndpoint, http, short)
          _                                                <- timeoutStarted.await
          _                                                <- timeoutReleased.await
          interruptStarted                                 <- Promise.make[Nothing, Unit]
          interruptComplete                                <- Promise.make[Nothing, Unit]
          interruptReleases                                <- Ref.make(0)
          interruptReleased                                <- Promise.make[Nothing, Unit]
          interruptEndpoint                                <-
            streamingEndpoint(
              (ZStream.fromZIO(interruptStarted.succeed(()).unit).drain ++
                ZStream.fromZIO(interruptComplete.await).drain)
                .ensuring(interruptReleases.update(_ + 1) *> interruptReleased.succeed(()).unit)
            )
          interruptFiber                                   <-
            unmanagedRemoteSubgraphExecutor(interruptEndpoint, http).execute(request, OperationType.Query).fork
          _                                                <- interruptStarted.await
          _                                                <- interruptFiber.interruptFork
          _                                                <- interruptComplete.succeed(())
          interrupted                                      <- interruptFiber.await
          _                                                <- interruptReleased.await
          successReleaseCount                              <- successReleases.get
          failureReleaseCount                              <- failureReleases.get
          sizeReleaseCount                                 <- sizeReleases.get
          timeoutReleaseCount                              <- timeoutReleases.get
          interruptCount                                   <- interruptReleases.get
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
          _.withHeaders(Header.Custom("Content-Type", "text/plain"))
            .forwardIncomingHeaders("Accept")
        )

      for {
        remote <- stub(okResponse)
        exit   <- Gateway
                    .compose(
                      Subgraph.graphql("first", remote.endpoint, firstPolicy),
                      Subgraph.graphql("second", remote.endpoint, valueInputSchema, secondPolicy)
                    )
                    .interpreter
                    .exit
        sent   <- remote.requests.get
        errors  = buildDiagnostics(exit)
      } yield assertTrue(
        sent.isEmpty,
        errors.count(_.startsWith("[first]")) == 5,
        errors.count(_.startsWith("[second]")) == 3,
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
              Header.Custom("X-Static", "static"),
              Header.Custom("X-Precedence", "static")
            )
        )
        .withExecutionHeadersZIO(
          ZIO.serviceWithZIO[RuntimeHeaders](_.values)
        )
      val environment = new RuntimeHeaders {
        def values: UIO[List[Header]] =
          ZIO.succeed(
            List(
              Header.Custom("X-Effect", "effect"),
              Header.Custom("X-Precedence", "effect"),
              Header.Custom("X-Multi", "first"),
              Header.Custom("x-multi", "second"),
              Header.Custom("Cookie", "a=1"),
              Header.Custom("Cookie", "b=2"),
              Header.Custom("Accept", "text/plain")
            )
          )
      }

      for {
        remote   <- stub(okResponse)
        runtime  <- remoteGateway(remote.endpoint, config = policy).interpreter
        response <- runtime
                      .executeRequest(
                        request,
                        List(
                          Header.Custom("X-Forwarded", "incoming"),
                          Header.Custom("X-Precedence", "incoming"),
                          Header.Custom("X-Ignored", "ignored"),
                          Header.Custom("Content-Type", "text/plain")
                        )
                      )
                      .provideEnvironment(ZEnvironment(environment))
        sent     <- remote.headers.get
        headers   = sent.headOption
        multi     = headers.fold(List.empty[String])(renderedHeaderValues(_, "X-Multi"))
      } yield assertTrue(
        response.errors.isEmpty,
        headers.flatMap(_.get("X-Forwarded")).contains("incoming"),
        headers.flatMap(_.get("X-Static")).contains("static"),
        headers.flatMap(_.get("X-Effect")).contains("effect"),
        headers.flatMap(_.get("X-Precedence")).contains("effect"),
        multi == List("first, second"),
        headers.fold(List.empty[String])(renderedHeaderValues(_, "Cookie")) == List("a=1; b=2"),
        headers.flatMap(_.get("X-Ignored")).isEmpty,
        headers.flatMap(_.get("Content-Type")).exists(_.startsWith("application/json")),
        headers.flatMap(_.get("Accept")).exists(_.contains("application/graphql-response+json"))
      )
    },
    test("does not let an incoming Connection header strip trusted headers") {
      val config = RemoteGraphQLConfig.default
        .withExecution(
          _.forwardAllIncomingHeaders
            .withHeaders(Header.Custom("Authorization", "Bearer configured"))
        )
        .withExecutionHeadersZIO(ZIO.succeed(List(Header.Custom("X-Trusted", "effectful"))))

      for {
        remote  <- stub(okResponse)
        runtime <- remoteGateway(remote.endpoint, config = config).interpreter
        _       <- runtime.executeRequest(
                     request,
                     List(
                       Header.Custom("Connection", "Authorization, X-Trusted"),
                       Header.Custom("Authorization", "Bearer incoming"),
                       Header.Custom("X-Trusted", "incoming")
                     )
                   )
        sent    <- remote.headers.get
        headers  = sent.headOption
      } yield assertTrue(
        headers.flatMap(_.get("Authorization")).contains("Bearer configured"),
        headers.flatMap(_.get("X-Trusted")).contains("effectful"),
        headers
          .flatMap(_.get("Connection"))
          .forall(value => !value.contains("Authorization") && !value.contains("X-Trusted"))
      )
    },
    test("forwards all incoming headers only when explicitly enabled") {
      val config = RemoteGraphQLConfig.default
        .withExecution(_.forwardAllIncomingHeaders)
        .withExecutionHeadersZIO(
          ZIO.succeed(
            List(
              Header.Custom("Connection", "X-Effect-Hop"),
              Header.Custom("X-Effect-Hop", "hop-by-hop")
            )
          )
        )

      for {
        remote  <- stub(okResponse)
        runtime <- remoteGateway(remote.endpoint, config = config).interpreter
        _       <- runtime.executeRequest(
                     request,
                     List(
                       Header.Custom("Authorization", "Bearer incoming"),
                       Header.Custom("X-Incoming", "forwarded"),
                       Header.Custom("Connection", "keep-alive, X-Internal"),
                       Header.Custom("X-Internal", "hop-by-hop"),
                       Header.Custom("Accept-Encoding", "br, gzip"),
                       Header.Custom("Accept", "text/plain")
                     )
                   )
        sent    <- remote.headers.get
        headers  = sent.headOption
      } yield assertTrue(
        headers.flatMap(_.get("Authorization")).contains("Bearer incoming"),
        headers.flatMap(_.get("X-Incoming")).contains("forwarded"),
        headers.flatMap(_.get("X-Internal")).isEmpty,
        headers.flatMap(_.get("X-Effect-Hop")).isEmpty,
        headers.flatMap(_.get("Accept-Encoding")).forall(!_.contains("br")),
        headers.flatMap(_.get("Accept")).exists(_.contains("application/graphql-response+json"))
      )
    },
    test("masks effectful header failures without dispatching the source call") {
      val secret = "header-provider-secret"
      val config = RemoteGraphQLConfig.default.withExecutionHeadersZIO(
        ZIO.fail(new RuntimeException(secret))
      )

      for {
        remote   <- stub(okResponse)
        runtime  <- remoteGateway(remote.endpoint, config = config).interpreter
        response <- runtime.executeRequest(request)
        sent     <- remote.requests.get
        rendered  = response.errors.map(_.msg).mkString(" ")
      } yield assertTrue(
        sent.isEmpty,
        response.errors.map(_.msg) == List("Remote GraphQL request failed."),
        !rendered.contains(secret)
      )
    },
    test("retains effectful header and transport failure causes") {
      val headerCause = new RuntimeException("header failure")
      val config      = RemoteGraphQLConfig.default.withExecutionHeadersZIO(ZIO.fail(headerCause))

      for {
        http            <- GatewayHttpClient.make
        remote          <- stub(okResponse)
        headerResult    <- call(remote.endpoint, http, config)
        transportResult <- call(unreachableEndpoint, http)
      } yield assertTrue(
        headerResult.left.exists {
          case failure @ HeaderFailure(error) => (error eq headerCause) && (failure.getCause eq headerCause)
          case _                              => false
        },
        transportResult.left.exists {
          case failure @ TransportFailure(error) => failure.getCause eq error
          case _                                 => false
        }
      )
    },
    test("deduplicates concurrent identical remote queries by default") {
      val callers = 20

      for {
        ready        <- Promise.make[Nothing, Unit]
        headerRuns   <- Ref.make(0)
        config        =
          RemoteGraphQLConfig.default.withExecutionHeadersZIO(headersAfterEveryCaller(callers, headerRuns, ready))
        remote       <- blockedEndpoint(expectedCalls = 1)
        runtime      <- remoteGateway(remote.uri, config = config).interpreter
        fibers       <- ZIO.foreach(1 to callers)(_ => runtime.executeRequest(request).fork)
        _            <- remote.started.await
        _            <- Live.live(ZIO.sleep(250.millis))
        shared       <- remote.calls.get
        _            <- remote.release.succeed(())
        responses    <- ZIO.foreach(fibers)(_.join)
        totalCalls   <- remote.calls.get
        totalHeaders <- headerRuns.get
      } yield assertTrue(
        shared == 1,
        totalCalls == 1,
        totalHeaders == callers,
        responses.forall(response =>
          response.errors.isEmpty && response.data == ObjectValue(List("value" -> StringValue("ok")))
        )
      )
    },
    test("does not deduplicate concurrent different remote queries") {
      val requests = List("a", "b").map(input => GraphQLRequest(query = Some(s"""{ value(input: "$input") }""")))

      for {
        remote    <- blockedEndpoint(expectedCalls = 2)
        runtime   <- remoteGateway(remote.uri).interpreter
        fibers    <- ZIO.foreach(requests)(runtime.executeRequest(_).fork)
        started   <- Live.live(remote.started.await.timeout(2.seconds))
        _         <- remote.release.succeed(())
        responses <- ZIO.foreach(fibers)(_.join)
        calls     <- remote.calls.get
      } yield assertTrue(started.nonEmpty, calls == 2, responses.forall(_.errors.isEmpty))
    },
    test("allows disabling deduplication for concurrent identical remote queries") {
      val config = RemoteGraphQLConfig.default.withExecution(_.withInFlightQueryDeduplication(false))

      for {
        remote    <- blockedEndpoint(expectedCalls = 2)
        runtime   <- remoteGateway(remote.uri, config = config).interpreter
        fibers    <- ZIO.foreach(1 to 2)(_ => runtime.executeRequest(request).fork)
        started   <- Live.live(remote.started.await.timeout(2.seconds))
        _         <- remote.release.succeed(())
        responses <- ZIO.foreach(fibers)(_.join)
        calls     <- remote.calls.get
      } yield assertTrue(started.nonEmpty, calls == 2, responses.forall(_.errors.isEmpty))
    },
    test("shares failures and removes the in-flight entry before a retry") {
      val config = RemoteGraphQLConfig.default

      for {
        http       <- GatewayHttpClient.make
        calls      <- Ref.make(0)
        started    <- Promise.make[Nothing, Unit]
        release    <- Promise.make[Nothing, Unit]
        remote     <- postEndpoint("graphql-http") { _ =>
                        calls.updateAndGet(_ + 1).flatMap {
                          case 1 => started.succeed(()).unit *> release.await.as(unavailable)
                          case _ => ZIO.succeed(Response.json(okResponse))
                        }
                      }
        source     <- RemoteSubgraphExecutor.make("remote", remote, http, config, PhaseHooks.empty)
        fibers     <- ZIO.foreach(1 to 20)(_ => source.execute(request, OperationType.Query).either.fork)
        _          <- started.await
        _          <- Live.live(ZIO.sleep(250.millis))
        shared     <- calls.get
        _          <- release.succeed(())
        failures   <- ZIO.foreach(fibers)(_.join)
        retry      <- source.execute(request, OperationType.Query).either
        totalCalls <- calls.get
      } yield assertTrue(
        shared == 1,
        failures.forall(_ == Left(HttpFailure(503))),
        retry.isRight,
        totalCalls == 2
      )
    },
    test("owns one retry sequence for concurrent identical queries") {
      val callers = 20

      for {
        ready        <- Promise.make[Nothing, Unit]
        headerRuns   <- Ref.make(0)
        config        = RemoteGraphQLConfig.default
                          .withExecution(_.withRetries(1, Duration.Zero))
                          .withExecutionHeadersZIO(headersAfterEveryCaller(callers, headerRuns, ready))
        calls        <- Ref.make(0)
        firstStarted <- Promise.make[Nothing, Unit]
        releaseFirst <- Promise.make[Nothing, Unit]
        remote       <- postEndpoint("graphql-http") { _ =>
                          calls.updateAndGet(_ + 1).flatMap {
                            case 1 => firstStarted.succeed(()).unit *> releaseFirst.await.as(unavailable)
                            case _ => ZIO.succeed(Response.json(okResponse))
                          }
                        }
        http         <- GatewayHttpClient.make
        source       <- RemoteSubgraphExecutor.make("remote", remote, http, config, PhaseHooks.empty)
        fibers       <- ZIO.foreach(1 to callers)(_ => source.execute(request, OperationType.Query).either.fork)
        _            <- firstStarted.await
        _            <- Live.live(ZIO.sleep(100.millis))
        _            <- releaseFirst.succeed(())
        responses    <- ZIO.foreach(fibers)(_.join)
        totalCalls   <- calls.get
        totalHeaders <- headerRuns.get
      } yield assertTrue(
        responses.forall(_.isRight),
        totalCalls == 2,
        totalHeaders == callers
      )
    },
    test("keeps shared work alive when one waiter is interrupted") {
      val config = RemoteGraphQLConfig.default

      for {
        http        <- GatewayHttpClient.make
        remote      <- blockedEndpoint(expectedCalls = 1)
        source      <- RemoteSubgraphExecutor.make("remote", remote.uri, http, config, PhaseHooks.empty)
        owner       <- source.execute(request, OperationType.Query).fork
        _           <- remote.started.await
        waiter      <- source.execute(request, OperationType.Query).fork
        _           <- Live.live(ZIO.sleep(100.millis))
        waiterExit  <- waiter.interrupt
        sharedCalls <- remote.calls.get
        _           <- remote.release.succeed(())
        ownerResult <- owner.join
      } yield assertTrue(
        waiterExit.isInterrupted,
        sharedCalls == 1,
        ownerResult.errors.isEmpty
      )
    },
    test("retains each waiter's deadline while sharing remote work") {
      val config = RemoteGraphQLConfig.default

      for {
        http         <- GatewayHttpClient.make
        remote       <- blockedEndpoint(expectedCalls = 1)
        source       <- RemoteSubgraphExecutor.make("remote", remote.uri, http, config, PhaseHooks.empty)
        owner        <- Live.live(source.execute(request, OperationType.Query).timeout(100.millis)).fork
        _            <- remote.started.await
        waiter       <- Live.live(source.execute(request, OperationType.Query).timeout(2.seconds)).fork
        ownerResult  <- owner.join
        sharedCalls  <- remote.calls.get
        _            <- remote.release.succeed(())
        waiterResult <- waiter.join
      } yield assertTrue(
        ownerResult.isEmpty,
        sharedCalls == 1,
        waiterResult.exists(_.errors.isEmpty)
      )
    },
    test("interrupts shared work when its owning scope closes") {
      val config = RemoteGraphQLConfig.default

      for {
        http       <- GatewayHttpClient.make
        remote     <- blockedEndpoint(expectedCalls = 1)
        scope      <- Scope.make
        source     <- scope.extend(
                        RemoteSubgraphExecutor.make("remote", remote.uri, http, config, PhaseHooks.empty)
                      )
        owner      <- source.execute(request, OperationType.Query).fork
        _          <- remote.started.await
        waiter     <- source.execute(request, OperationType.Query).fork
        _          <- Live.live(ZIO.sleep(100.millis))
        closing    <- scope.close(Exit.unit).fork
        ownerExit  <- owner.await
        waiterExit <- waiter.await
        _          <- closing.join
        calls      <- remote.calls.get
      } yield assertTrue(
        ownerExit.isInterrupted,
        waiterExit.isInterrupted,
        calls == 1
      )
    },
    test("does not deduplicate mutations or calls with distinct request identities") {
      val config = RemoteGraphQLConfig.default

      for {
        http             <- GatewayHttpClient.make
        mutations        <- blockedEndpoint(expectedCalls = 2)
        mutationSource   <-
          RemoteSubgraphExecutor.make("remote", mutations.uri, http, config, PhaseHooks.empty)
        mutationFibers   <- ZIO.foreach(1 to 2)(_ => mutationSource.execute(request, OperationType.Mutation).fork)
        mutationsReady   <- Live.live(mutations.started.await.timeout(2.seconds))
        _                <- mutations.release.succeed(())
        _                <- ZIO.foreach(mutationFibers)(_.join)
        mutationTotal    <- mutations.calls.get
        headerRuns       <- Ref.make(0)
        headers          <- blockedEndpoint(expectedCalls = 2)
        headerConfig      = config.withExecutionHeadersZIO(
                              headerRuns
                                .updateAndGet(_ + 1)
                                .map(value => List(Header.Custom("X-Request-Identity", value.toString)))
                            )
        headerSource     <- RemoteSubgraphExecutor.make(
                              "remote",
                              headers.uri,
                              http,
                              headerConfig,
                              PhaseHooks.empty
                            )
        headerFibers     <- ZIO.foreach(1 to 2)(_ => headerSource.execute(request, OperationType.Query).fork)
        headersReady     <- Live.live(headers.started.await.timeout(2.seconds))
        _                <- headers.release.succeed(())
        _                <- ZIO.foreach(headerFibers)(_.join)
        headerTotal      <- headers.calls.get
        evaluatedHeaders <- headerRuns.get
        incoming         <- blockedEndpoint(expectedCalls = 2)
        incomingSource   <- RemoteSubgraphExecutor.make(
                              "remote",
                              incoming.uri,
                              http,
                              config.withExecution(_.forwardIncomingHeaders("X-Tenant")),
                              PhaseHooks.empty
                            )
        incomingFibers   <- ZIO.foreach(List("one", "two"))(tenant =>
                              IncomingRequestHeaders
                                .locally(List("X-Tenant" -> tenant))(
                                  incomingSource.execute(request, OperationType.Query)
                                )
                                .fork
                            )
        incomingReady    <- Live.live(incoming.started.await.timeout(2.seconds))
        _                <- incoming.release.succeed(())
        _                <- ZIO.foreach(incomingFibers)(_.join)
        incomingTotal    <- incoming.calls.get
        bodies           <- blockedEndpoint(expectedCalls = 3)
        bodySource       <- RemoteSubgraphExecutor.make("remote", bodies.uri, http, config, PhaseHooks.empty)
        bodyRequests      = List(
                              request.copy(variables = Some(Map("input" -> StringValue("one")))),
                              request.copy(variables = Some(Map("input" -> StringValue("two")))),
                              request.copy(
                                operationName = Some("Other"),
                                variables = Some(Map("input" -> StringValue("one")))
                              )
                            )
        bodyFibers       <- ZIO.foreach(bodyRequests)(bodySource.execute(_, OperationType.Query).fork)
        bodiesReady      <- Live.live(bodies.started.await.timeout(2.seconds))
        _                <- bodies.release.succeed(())
        _                <- ZIO.foreach(bodyFibers)(_.join)
        bodyTotal        <- bodies.calls.get
      } yield assertTrue(
        mutationsReady.nonEmpty,
        mutationTotal == 2,
        headersReady.nonEmpty,
        headerTotal == 2,
        evaluatedHeaders == 2,
        incomingReady.nonEmpty,
        incomingTotal == 2,
        bodiesReady.nonEmpty,
        bodyTotal == 3
      )
    },
    test("retries one logical replay-safe call and never retries mutations or GraphQL envelopes") {
      val policy = RemoteGraphQLConfig.default.withExecution(
        _.withRetries(2, Duration.Zero)
      )

      for {
        http             <- GatewayHttpClient.make
        queryCalls       <- Ref.make(0)
        headerCalls      <- Ref.make(0)
        queryEndpoint    <- postEndpoint("graphql-http") { _ =>
                              queryCalls.updateAndGet(_ + 1).map { attempt =>
                                if (attempt < 3) unavailable else Response.json(okResponse)
                              }
                            }
        countedPolicy     = policy.withExecutionHeadersZIO(
                              headerCalls.updateAndGet(_ + 1).as(List(Header.Custom("Authorization", "secret")))
                            )
        queryResult      <- call(queryEndpoint, http, countedPolicy)
        queryAttempts    <- queryCalls.get
        policyRuns       <- headerCalls.get
        mutationCalls    <- Ref.make(0)
        mutationEndpoint <- postEndpoint("graphql-http")(_ => mutationCalls.update(_ + 1).as(unavailable))
        mutationRequest   = GraphQLRequest(query = Some("mutation Update { value }"), operationName = Some("Update"))
        mutationResult   <- call(
                              mutationEndpoint,
                              http,
                              policy,
                              value = mutationRequest,
                              operation = OperationType.Mutation
                            )
        mutationAttempts <- mutationCalls.get
        rejectedCalls    <- Ref.make(0)
        rejectedEndpoint <- postEndpoint("graphql-http")(_ =>
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
        rejectedResult   <- call(rejectedEndpoint, http, policy)
        rejectedAttempts <- rejectedCalls.get
        envelopeCalls    <- Ref.make(0)
        envelopeEndpoint <- postEndpoint("graphql-http")(_ =>
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
        envelopeResult   <- call(envelopeEndpoint, http, policy, remoteErrorMessages = true)
        envelopeAttempts <- envelopeCalls.get
      } yield assertTrue(
        queryResult.isRight,
        queryAttempts == 3,
        policyRuns == 1,
        mutationResult == Left(HttpFailure(503)),
        mutationAttempts == 1,
        rejectedResult == Left(HttpFailure(400)),
        rejectedResult.left.exists(failureOutcome(_) == PhaseHooks.Outcome.RequestError),
        mutationResult.left.exists(failureOutcome(_) == PhaseHooks.Outcome.TransportError),
        rejectedAttempts == 1,
        envelopeResult.exists(_.errors.map(_.msg) == List("try later")),
        envelopeAttempts == 1
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

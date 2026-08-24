package caliban.gateway

import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.GraphQLSource._
import caliban.gateway.internal.RemoteGraphQLSource
import caliban.gateway.internal.unmanagedRemoteGraphQLSource
import caliban.parsing.adt.OperationType
import caliban.{ GraphQLRequest, IncomingRequestHeaders, ResponseValue }
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import sttp.client4.httpclient.zio.{ HttpClientZioBackend, SttpClient }
import sttp.model.{ Header => SttpHeader, Uri }
import zio._
import zio.http.{ Body, Handler, Header, Headers, Method, Request, Response, Routes, Server, Status }
import zio.stream.ZStream
import zio.test._

object GraphQLHttpSpec extends ZIOSpecDefault {

  private trait RuntimeHeaders {
    def values: UIO[List[SttpHeader]]
  }

  private val schema      = "type Query { value(input: String): String }"
  private val request     = GraphQLRequest(query = Some("query Value { value }"), operationName = Some("Value"))
  private val unavailable = Response(
    Status.ServiceUnavailable,
    Headers(Header.Custom("Content-Type", "text/plain")),
    Body.fromString("unavailable")
  )

  private def call(endpoint: Uri, backend: SttpClient) =
    unmanagedRemoteGraphQLSource(endpoint, backend).execute(request, OperationType.Query).either

  private def call[R](
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteGraphQLConfig[R],
    limits: RemoteGraphQLSource.StructuralLimits = RemoteGraphQLSource.StructuralLimits.default,
    value: GraphQLRequest = request,
    operation: OperationType = OperationType.Query
  ) =
    unmanagedRemoteGraphQLSource(endpoint, backend, config, limits).execute(value, operation).either

  private def endpoint(handler: Request => UIO[Response]): ZIO[Server with Ref[Int], Nothing, Uri] =
    postEndpoint("graphql-http")(handler)

  private def fixed(status: Status, mediaType: Option[String], body: String): ZIO[Server with Ref[Int], Nothing, Uri] =
    endpoint { _ =>
      val headers = mediaType.fold(Headers.empty)(value => Headers(Header.Custom("Content-Type", value)))
      ZIO.succeed(Response(status, headers, Body.fromString(body)))
    }

  private final case class BlockedEndpoint(
    uri: Uri,
    calls: Ref[Int],
    started: Promise[Nothing, Unit],
    release: Promise[Nothing, Unit]
  )

  private def blockedEndpoint(expectedCalls: Int): ZIO[Server with Ref[Int], Nothing, BlockedEndpoint] =
    for {
      calls   <- Ref.make(0)
      started <- Promise.make[Nothing, Unit]
      release <- Promise.make[Nothing, Unit]
      uri     <- endpoint { _ =>
                   calls.updateAndGet(_ + 1).flatMap { count =>
                     ZIO.when(count == expectedCalls)(started.succeed(()).unit) *>
                       release.await.as(Response.json("""{"data":{"value":"ok"}}"""))
                   }
                 }
    } yield BlockedEndpoint(uri, calls, started, release)

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
        graphQlResult       <- call(graphQlError, backend, disclosed)
        legacyResult        <- call(legacySuccess, backend)
        statusResult        <- call(legacyFailure, backend)
        textStatusResult    <- call(textFailure, backend)
        untypedStatusResult <- call(untypedFailure, backend)
        malformedResult     <- call(malformed, backend)
        metadataResult      <- call(malformedMetadata, backend)
        incrementalResult   <- call(incremental, backend)
        emptyResult         <- call(empty, backend)
        unsupportedResult   <- call(unsupported, backend)
        redirectResult      <- call(redirect, backend)
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
        requestResult   <- call(requestEndpoint, backend, config, structural, largeRequest)
        responseResult  <- call(oversizedBody, backend, config, structural)
        nestingResult   <- call(nestedBody, backend, config, structural)
        structureResult <- call(structuredBody, backend, config, structural)
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
          backend                                          <- HttpClientZioBackend.scoped()
          successTracked                                   <- tracked("""{"data":{"value":"ok"}}""")
          (successStream, successReleases, successReleased) = successTracked
          successEndpoint                                  <- streamingEndpoint(successStream)
          success                                          <- call(successEndpoint, backend)
          _                                                <- successReleased.await
          failureTracked                                   <- tracked("not-json")
          (failureStream, failureReleases, failureReleased) = failureTracked
          failureEndpoint                                  <- streamingEndpoint(failureStream)
          failure                                          <- call(failureEndpoint, backend)
          _                                                <- failureReleased.await
          sizeTracked                                      <- tracked("x" * 128)
          (sizeStream, sizeReleases, sizeReleased)          = sizeTracked
          sizeEndpoint                                     <- streamingEndpoint(sizeStream)
          sizeFailure                                      <-
            call(sizeEndpoint, backend, small)
          _                                                <- sizeReleased.await
          timeoutStarted                                   <- Promise.make[Nothing, Unit]
          timeoutReleases                                  <- Ref.make(0)
          timeoutReleased                                  <- Promise.make[Nothing, Unit]
          timeoutEndpoint                                  <- streamingEndpoint(
                                                                (ZStream.fromZIO(timeoutStarted.succeed(()).unit).drain ++ ZStream.never)
                                                                  .ensuring(timeoutReleases.update(_ + 1) *> timeoutReleased.succeed(()).unit)
                                                              )
          timeoutFailure                                   <-
            call(timeoutEndpoint, backend, short)
          _                                                <- timeoutStarted.await
          _                                                <- timeoutReleased.await
          interruptStarted                                 <- Promise.make[Nothing, Unit]
          interruptComplete                                <- Promise.make[Nothing, Unit]
          interruptReleases                                <- Ref.make(0)
          interruptReleased                                <- Promise.make[Nothing, Unit]
          interruptEndpoint                                <- streamingEndpoint(
                                                                (ZStream.fromZIO(interruptStarted.succeed(()).unit).drain ++
                                                                  ZStream.fromZIO(interruptComplete.await).drain)
                                                                  .ensuring(interruptReleases.update(_ + 1) *> interruptReleased.succeed(()).unit)
                                                              )
          interruptFiber                                   <-
            unmanagedRemoteGraphQLSource(interruptEndpoint, backend).execute(request, OperationType.Query).fork
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
    test("does not let an incoming Connection header strip trusted headers") {
      val config = RemoteGraphQLConfig.default
        .withExecution(
          _.forwardAllIncomingHeaders
            .withHeaders(SttpHeader("Authorization", "Bearer configured"))
        )
        .withExecutionHeadersZIO(ZIO.succeed(List(SttpHeader("X-Trusted", "effectful"))))

      for {
        remote  <- stub("""{"data":{"value":"ok"}}""")
        gateway <- Gateway.compose(Subgraph.graphql("remote", remote.endpoint, schema, config)).build
        _       <- gateway.executeRequest(
                     request,
                     List(
                       SttpHeader("Connection", "Authorization, X-Trusted"),
                       SttpHeader("Authorization", "Bearer incoming"),
                       SttpHeader("X-Trusted", "incoming")
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
    test("deduplicates concurrent identical remote queries") {
      val callers = 20

      for {
        ready        <- Promise.make[Nothing, Unit]
        headerRuns   <- Ref.make(0)
        config        = RemoteGraphQLConfig.default
                          .withExecution(_.withInFlightQueryDeduplication(true))
                          .withExecutionHeadersZIO(
                            headerRuns
                              .updateAndGet(_ + 1)
                              .flatMap(count => ready.succeed(()).unit.when(count == callers)) *>
                              ready.await.as(Nil)
                          )
        remote       <- blockedEndpoint(expectedCalls = 1)
        gateway      <- Gateway.compose(Subgraph.graphql("remote", remote.uri, schema, config)).build
        fibers       <- ZIO.foreach(1 to callers)(_ => gateway.executeRequest(request).fork)
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
    test("shares failures and removes the in-flight entry before a retry") {
      val config = RemoteGraphQLConfig.default.withExecution(_.withInFlightQueryDeduplication(true))

      for {
        backend    <- HttpClientZioBackend.scoped()
        calls      <- Ref.make(0)
        started    <- Promise.make[Nothing, Unit]
        release    <- Promise.make[Nothing, Unit]
        remote     <- endpoint { _ =>
                        calls.updateAndGet(_ + 1).flatMap {
                          case 1 => started.succeed(()).unit *> release.await.as(unavailable)
                          case _ => ZIO.succeed(Response.json("""{"data":{"value":"ok"}}"""))
                        }
                      }
        source     <- RemoteGraphQLSource.make("remote", remote, backend, config, GatewayWrapper.empty)
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
                          .withExecution(
                            _.withInFlightQueryDeduplication(true).withRetries(1, Duration.Zero)
                          )
                          .withExecutionHeadersZIO(
                            headerRuns
                              .updateAndGet(_ + 1)
                              .flatMap(count => ready.succeed(()).unit.when(count == callers)) *>
                              ready.await.as(Nil)
                          )
        calls        <- Ref.make(0)
        firstStarted <- Promise.make[Nothing, Unit]
        releaseFirst <- Promise.make[Nothing, Unit]
        remote       <- endpoint { _ =>
                          calls.updateAndGet(_ + 1).flatMap {
                            case 1 => firstStarted.succeed(()).unit *> releaseFirst.await.as(unavailable)
                            case _ => ZIO.succeed(Response.json("""{"data":{"value":"ok"}}"""))
                          }
                        }
        backend      <- HttpClientZioBackend.scoped()
        source       <- RemoteGraphQLSource.make("remote", remote, backend, config, GatewayWrapper.empty)
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
      val config = RemoteGraphQLConfig.default.withExecution(_.withInFlightQueryDeduplication(true))

      for {
        backend     <- HttpClientZioBackend.scoped()
        remote      <- blockedEndpoint(expectedCalls = 1)
        source      <- RemoteGraphQLSource.make("remote", remote.uri, backend, config, GatewayWrapper.empty)
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
      val config = RemoteGraphQLConfig.default.withExecution(_.withInFlightQueryDeduplication(true))

      for {
        backend      <- HttpClientZioBackend.scoped()
        remote       <- blockedEndpoint(expectedCalls = 1)
        source       <- RemoteGraphQLSource.make("remote", remote.uri, backend, config, GatewayWrapper.empty)
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
      val config = RemoteGraphQLConfig.default.withExecution(_.withInFlightQueryDeduplication(true))

      for {
        backend    <- HttpClientZioBackend.scoped()
        remote     <- blockedEndpoint(expectedCalls = 1)
        scope      <- Scope.make
        source     <- scope.extend(
                        RemoteGraphQLSource.make("remote", remote.uri, backend, config, GatewayWrapper.empty)
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
      val config = RemoteGraphQLConfig.default.withExecution(_.withInFlightQueryDeduplication(true))

      for {
        backend          <- HttpClientZioBackend.scoped()
        mutations        <- blockedEndpoint(expectedCalls = 2)
        mutationSource   <- RemoteGraphQLSource.make("remote", mutations.uri, backend, config, GatewayWrapper.empty)
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
                                .map(value => List(SttpHeader("X-Request-Identity", value.toString)))
                            )
        headerSource     <- RemoteGraphQLSource.make(
                              "remote",
                              headers.uri,
                              backend,
                              headerConfig,
                              GatewayWrapper.empty
                            )
        headerFibers     <- ZIO.foreach(1 to 2)(_ => headerSource.execute(request, OperationType.Query).fork)
        headersReady     <- Live.live(headers.started.await.timeout(2.seconds))
        _                <- headers.release.succeed(())
        _                <- ZIO.foreach(headerFibers)(_.join)
        headerTotal      <- headers.calls.get
        evaluatedHeaders <- headerRuns.get
        incoming         <- blockedEndpoint(expectedCalls = 2)
        incomingSource   <- RemoteGraphQLSource.make(
                              "remote",
                              incoming.uri,
                              backend,
                              config.withExecution(_.forwardIncomingHeaders("X-Tenant")),
                              GatewayWrapper.empty
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
        bodySource       <- RemoteGraphQLSource.make("remote", bodies.uri, backend, config, GatewayWrapper.empty)
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
        queryResult      <- call(queryEndpoint, backend, countedPolicy)
        queryAttempts    <- queryCalls.get
        policyRuns       <- headerCalls.get
        mutationCalls    <- Ref.make(0)
        mutationEndpoint <- endpoint(_ => mutationCalls.update(_ + 1).as(unavailable))
        mutationRequest   = GraphQLRequest(query = Some("mutation Update { value }"), operationName = Some("Update"))
        mutationResult   <- call(
                              mutationEndpoint,
                              backend,
                              policy,
                              value = mutationRequest,
                              operation = OperationType.Mutation
                            )
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
        rejectedResult   <- call(rejectedEndpoint, backend, policy)
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
        envelopeResult   <- call(envelopeEndpoint, backend, policy.withErrorDisclosure(_.withMessages(true)))
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

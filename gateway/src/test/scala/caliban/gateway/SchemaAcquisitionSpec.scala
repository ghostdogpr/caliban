package caliban.gateway

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, StringValue }
import caliban.client.CalibanClientError
import caliban.gateway.GatewayTestSupport._
import caliban.schema.{ GenericSchema, Schema }
import caliban.tools.IntrospectionClient
import caliban.{ graphQL, GraphQLResponse, RootResolver }
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import sttp.model.{ Header => SttpHeader }
import zio._
import zio.http.{ Body, Handler, Header, Headers, Method, Request, Response, Routes, Server, Status }
import zio.stream.ZStream
import zio.test._

object SchemaAcquisitionSpec extends ZIOSpecDefault {

  private object ProductsApi extends GenericSchema[Any] {
    import auto._

    final case class Product(name: String)
    final case class Query(product: Product)

    implicit val productSchema: Schema[Any, Product] = gen
    implicit val querySchema: Schema[Any, Query]     = gen

    val api = graphQL(RootResolver(Query(Product("Table"))))
  }

  private val productResponse = """{"data":{"product":{"name":"Table"}}}"""

  private val reviewsSchema =
    """
      |type Query {
      |  review: Review
      |}
      |
      |type Review {
      |  body: String!
      |}
      |""".stripMargin

  private val reviewResponse = """{"data":{"review":{"body":"Solid"}}}"""

  private def introspectionResponse: UIO[String] = {
    implicit val config: IntrospectionClient.Config = IntrospectionClient.Config.default

    for {
      interpreter <- ZIO.fromEither(ProductsApi.api.interpreterEither).orDie
      request      = IntrospectionClient.introspection.toGraphQL(dropNullInputValues = true)
      response    <- interpreter.execute(request.query)
    } yield writeToString(response)
  }

  private def serviceResponse(schema: String): String =
    writeToString(
      GraphQLResponse[Any](
        ObjectValue(List("_service" -> ObjectValue(List("sdl" -> StringValue(schema))))),
        Nil,
        None,
        None
      )
    )

  private def introspectionErrorMessages(result: Either[GatewayBuildError, Any]): List[String] =
    result match {
      case Left(
            GatewayBuildError.SubgraphLoadingFailed(
              List(SubgraphError(_, SchemaAcquisitionError.IntrospectionErrors(errors)))
            )
          ) =>
        errors.map(_.message)
      case _ => Nil
    }

  def spec = suite("SchemaAcquisitionSpec")(
    test("acquires ordinary introspection and Federation service SDL through pinned composition") {
      for {
        introspection   <- introspectionResponse
        acquiredApi     <- stub(introspection, productResponse)
        pinnedApi       <- stub(productResponse)
        federationApi   <- stub(serviceResponse(reviewsSchema), reviewResponse)
        acquired        <- Gateway
                             .compose(
                               Subgraph.graphql("products", acquiredApi.endpoint),
                               Subgraph.federation("reviews", federationApi.endpoint)
                             )
                             .interpreter
        pinned          <- Gateway
                             .compose(
                               Subgraph.graphql("products", pinnedApi.endpoint, ProductsApi.api.toDocument),
                               Subgraph.federation("reviews", federationApi.endpoint, reviewsSchema)
                             )
                             .interpreter
        acquiredResult  <- acquired.execute("{ product { name } review { body } }")
        pinnedResult    <- pinned.execute("{ product { name } review { body } }")
        ordinaryCalls   <- acquiredApi.requests.get
        federationCalls <- federationApi.requests.get
      } yield assertTrue(
        acquiredResult == pinnedResult,
        field(acquiredResult.data, "product").flatMap(field(_, "name")).contains(StringValue("Table")),
        field(acquiredResult.data, "review").flatMap(field(_, "body")).contains(StringValue("Solid")),
        ordinaryCalls.headOption.flatMap(_.query).exists(_.contains("__schema")),
        federationCalls.headOption.flatMap(_.query).exists(_.contains("_service"))
      )
    },
    test("classifies introspection GraphQL errors when data is present or null") {
      val nullDataResponse = """{"data":null,"errors":[{"message":"introspection failed"}]}"""

      for {
        introspection <- introspectionResponse
        dataResponse   = introspection.dropRight(1) + ",\"errors\":[{\"message\":\"introspection failed\"}]}"
        dataSource    <- stub(dataResponse)
        nullSource    <- stub(nullDataResponse)
        dataResult    <- Gateway.compose(Subgraph.graphql("data", dataSource.endpoint)).interpreter.either
        nullResult    <- Gateway.compose(Subgraph.graphql("null", nullSource.endpoint)).interpreter.either
      } yield assertTrue(
        introspectionErrorMessages(dataResult) == List("introspection failed"),
        introspectionErrorMessages(nullResult) == List("introspection failed")
      )
    },
    test("preserves referenced deprecation and specifiedBy metadata from acquired SDL") {
      val metadataSchema =
        """
          |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3")
          |type Query { wrapper: Wrapper }
          |type Wrapper { nested: Nested }
          |type Nested { legacy: String @deprecated state: State url: URL }
          |enum State { ACTIVE LEGACY @deprecated(reason: "Use ACTIVE") }
          |scalar URL @specifiedBy(url: "https://example.com/url")
          |""".stripMargin

      for {
        source     <- stub(serviceResponse(metadataSchema))
        gateway    <- Gateway.compose(Subgraph.federation("metadata", source.endpoint)).interpreter
        response   <-
          gateway.execute(
            "{ nested: __type(name: \"Nested\") { fields(includeDeprecated: true) { name isDeprecated deprecationReason } } state: __type(name: \"State\") { enumValues(includeDeprecated: true) { name isDeprecated deprecationReason } } scalar: __type(name: \"URL\") { specifiedByURL } }"
          )
        nested      = listValues(field(response.data, "nested").flatMap(field(_, "fields")))
        states      = listValues(field(response.data, "state").flatMap(field(_, "enumValues")))
        legacyField = nested.find(field(_, "name").contains(StringValue("legacy")))
        legacyState = states.find(field(_, "name").contains(StringValue("LEGACY")))
      } yield assertTrue(
        response.errors.isEmpty,
        legacyField.exists(value =>
          field(value, "isDeprecated").contains(BooleanValue(true)) &&
            field(value, "deprecationReason").contains(StringValue("No longer supported"))
        ),
        legacyState.exists(value =>
          field(value, "isDeprecated").contains(BooleanValue(true)) &&
            field(value, "deprecationReason").contains(StringValue("Use ACTIVE"))
        ),
        field(response.data, "scalar")
          .flatMap(field(_, "specifiedByURL"))
          .contains(StringValue("https://example.com/url"))
      )
    },
    test("acquires sibling schemas concurrently") {
      for {
        introspection     <- introspectionResponse
        ordinaryStarted   <- Promise.make[Nothing, Unit]
        federationStarted <- Promise.make[Nothing, Unit]
        release           <- Promise.make[Nothing, Unit]
        ordinary          <- stubWith(ordinaryStarted.succeed(()).unit *> release.await, introspection)
        federation        <- stubWith(federationStarted.succeed(()).unit *> release.await, serviceResponse(reviewsSchema))
        fiber             <- Gateway
                               .compose(
                                 Subgraph.graphql("products", ordinary.endpoint),
                                 Subgraph.federation("reviews", federation.endpoint)
                               )
                               .interpreter
                               .fork
        _                 <- ordinaryStarted.await
        _                 <- federationStarted.await
        _                 <- release.succeed(())
        result            <- fiber.join.either
      } yield assertTrue(result.isRight)
    },
    test("attributes one acquisition failure without constructing a partial runtime") {
      for {
        introspection <- introspectionResponse
        ordinary      <- stub(introspection)
        broken        <- stub(invalidResponse)
        result        <- Gateway
                           .compose(
                             Subgraph.graphql("products", ordinary.endpoint),
                             Subgraph.federation("reviews", broken.endpoint)
                           )
                           .interpreter
                           .either
        ordinaryCalls <- ordinary.requests.get
        brokenCalls   <- broken.requests.get
      } yield assertTrue(
        result.left.exists(_.diagnostics.exists(_.startsWith("[reviews]"))),
        result.left.exists(!_.diagnostics.exists(_.startsWith("[products]"))),
        ordinaryCalls.size == 1,
        brokenCalls.size == 1
      )
    },
    test("rejects a Federation schema response containing GraphQL errors") {
      val response = serviceResponse(reviewsSchema).dropRight(1) +
        ""","errors":[{"message":"schema acquisition failed"}]}"""

      for {
        remote <- stub(response)
        result <- Gateway.compose(Subgraph.federation("reviews", remote.endpoint)).interpreter.either
      } yield assertTrue(
        result.left.exists(
          _.diagnostics == List("[reviews] Federation service returned GraphQL errors: schema acquisition failed")
        ),
        result.left.exists {
          case GatewayBuildError.SubgraphLoadingFailed(
                List(SubgraphError("reviews", SchemaAcquisitionError.FederationErrors(errors)))
              ) =>
            errors.nonEmpty
          case _ => false
        }
      )
    },
    test("classifies malformed Federation service responses") {
      import SchemaAcquisitionError.InvalidFederationResponse._

      val cases = List(
        "[]"                           -> ExpectedResponseObject,
        """{"errors":true}"""          -> InvalidErrors,
        invalidResponse                -> MissingData,
        """{"data":{}}"""              -> MissingService,
        """{"data":{"_service":{}}}""" -> MissingSdl
      )

      ZIO
        .foreach(cases) { case (response, expected) =>
          for {
            remote <- stub(response)
            result <- Gateway.compose(Subgraph.federation("reviews", remote.endpoint)).interpreter.either
          } yield result.left.toOption.collect {
            case GatewayBuildError.SubgraphLoadingFailed(
                  List(SubgraphError("reviews", SchemaAcquisitionError.InvalidFederationResponse(reason)))
                ) =>
              reason
          }.contains(expected)
        }
        .map(results => assertTrue(results.forall(value => value)))
    },
    test("accumulates invalid names with remote acquisition failures") {
      for {
        valid          <- stub(serviceResponse(reviewsSchema))
        broken         <- stub(invalidResponse)
        result         <- Gateway
                            .compose(
                              Subgraph.federation("reviews", valid.endpoint),
                              Subgraph.federation("reviews", broken.endpoint)
                            )
                            .interpreter
                            .either
        validRequests  <- valid.requests.get
        brokenRequests <- broken.requests.get
      } yield assertTrue(
        result.left.exists {
          case GatewayBuildError.CombinedFailures(errors) =>
            errors.exists(_.isInstanceOf[GatewayBuildError.InvalidConfiguration]) &&
            errors.exists(_.isInstanceOf[GatewayBuildError.SubgraphLoadingFailed])
          case _                                          => false
        },
        result.left.exists(_.diagnostics.exists(_.contains("Name is used more than once"))),
        result.left.exists(_.diagnostics.exists(_.contains("'data' field was missing"))),
        validRequests.size == 1,
        brokenRequests.size == 1
      )
    },
    test("retains request failure causes without exposing their messages in diagnostics") {
      val cause = new RuntimeException("secret endpoint and response details")
      val error = SchemaAcquisitionError.RequestFailed(cause)

      assertTrue(
        error.getCause eq cause,
        !error.diagnostics.exists(_.contains(cause.getMessage))
      )
    },
    test("retains client decoding errors without exposing their messages in diagnostics") {
      val cause       = new RuntimeException("secret response details")
      val clientError = CalibanClientError.DecodingError("secret decoder context", Some(cause))
      val error       = SchemaAcquisitionError.InvalidIntrospectionResponse(clientError)

      assertTrue(
        error.getCause eq clientError,
        !error.diagnostics.exists(_.contains(clientError.msg)),
        !error.diagnostics.exists(_.contains(cause.getMessage))
      )
    },
    test("enforces acquisition headers, redirects, and finite response and parsing limits") {
      val headersConfig   = RemoteGraphQLConfig.default.withAcquisition(
        _.withHeaders(
          SttpHeader("Authorization", "Bearer schema"),
          SttpHeader("X-Multi", "first"),
          SttpHeader("X-Multi", "second")
        )
      )
      val protectedConfig = RemoteGraphQLConfig.default.withAcquisition(
        _.withHeaders(
          SttpHeader("Content-Type", "text/plain"),
          SttpHeader("Content-Encoding", "gzip")
        )
      )
      val responseLimit   = RemoteGraphQLConfig.default.withAcquisition(
        _.withMaxResponseBytes(32)
      )
      val parsingLimit    = RemoteGraphQLConfig.default.withAcquisition(
        _.withMaxParsingDepth(4)
      )
      val ordinaryLimit   = RemoteGraphQLConfig.default.withAcquisition(
        _.withMaxParsingDepth(32)
      )
      val nestedSchema    = reviewsSchema.replace("body: String!", "body(arg: [[[[[String]]]]]): String!")
      val nestedDefault   = List.fill(40)("[").mkString + "null" + List.fill(40)("]").mkString

      for {
        introspection    <- introspectionResponse
        headerStub       <- stub(serviceResponse(reviewsSchema), reviewResponse)
        headerGateway    <- Gateway
                              .compose(Subgraph.federation("headers", headerStub.endpoint, headersConfig))
                              .interpreter
        _                <- headerGateway.execute("{ review { body } }")
        sentHeaders      <- headerStub.headers.get
        protectedStub    <- stub(serviceResponse(reviewsSchema))
        protectedResult  <- Gateway
                              .compose(Subgraph.federation("protected", protectedStub.endpoint, protectedConfig))
                              .interpreter
                              .either
        protectedCalls   <- protectedStub.requests.get
        boundedStub      <- stub(serviceResponse(reviewsSchema))
        boundedResult    <- Gateway
                              .compose(Subgraph.federation("bounded", boundedStub.endpoint, responseLimit))
                              .interpreter
                              .either
        parsingStub      <- stub(serviceResponse(nestedSchema))
        parsingResult    <- Gateway
                              .compose(Subgraph.federation("parsing", parsingStub.endpoint, parsingLimit))
                              .interpreter
                              .either
        ordinaryStub     <- stub(
                              introspection.replaceFirst(
                                "\"defaultValue\":null",
                                "\"defaultValue\":\"" + nestedDefault + "\""
                              )
                            )
        ordinaryResult   <- Gateway
                              .compose(Subgraph.graphql("ordinary-parsing", ordinaryStub.endpoint, ordinaryLimit))
                              .interpreter
                              .either
        redirectTarget   <- stub(serviceResponse(reviewsSchema))
        redirects        <- Ref.make(0)
        redirectEndpoint <- postEndpoint("redirect")(_ =>
                              redirects
                                .update(_ + 1)
                                .as(
                                  Response(
                                    Status.TemporaryRedirect,
                                    Headers(
                                      Header.Custom("Location", redirectTarget.endpoint.toString),
                                      Header.Custom("Content-Type", "application/graphql-response+json")
                                    ),
                                    Body.fromString(serviceResponse(reviewsSchema))
                                  )
                                )
                            )
        redirectResult   <- Gateway
                              .compose(
                                Subgraph.federation(
                                  "redirect",
                                  redirectEndpoint
                                )
                              )
                              .interpreter
                              .either
        redirectCount    <- redirects.get
        targetCalls      <- redirectTarget.requests.get
        acquisitionMulti  = sentHeaders.headOption.fold(List.empty[String])(
                              _.iterator
                                .filter(_.headerName.equalsIgnoreCase("X-Multi"))
                                .map(_.renderedValue)
                                .toList
                            )
      } yield assertTrue(
        sentHeaders.headOption.flatMap(_.get("Authorization")).contains("Bearer schema"),
        acquisitionMulti == List("first", "second"),
        sentHeaders.headOption.flatMap(_.get("Content-Type")).exists(_.startsWith("application/json")),
        sentHeaders.headOption.flatMap(_.get("Accept")).exists(_.contains("application/graphql-response+json")),
        sentHeaders.lift(1).flatMap(_.get("Authorization")).isEmpty,
        sentHeaders.lift(1).flatMap(_.get("X-Multi")).isEmpty,
        protectedResult.left.exists(_.diagnostics.exists(_.contains("header 'Content-Type' is owned"))),
        protectedResult.left.exists(_.diagnostics.exists(_.contains("header 'Content-Encoding' is owned"))),
        protectedCalls.isEmpty,
        boundedResult.left.exists(_.diagnostics.exists(_.contains("response exceeded 32 bytes"))),
        parsingResult.left.exists(_.diagnostics.exists(_.contains("parsing depth exceeded 4"))),
        ordinaryResult.left.exists(_.diagnostics.exists(_.contains("parsing depth exceeded 32"))),
        redirectResult.left.exists(_.diagnostics.exists(_.startsWith("[redirect]"))),
        redirectCount == 1,
        targetCalls.isEmpty
      )
    },
    test("bounds embedded GraphQL nesting while ignoring comments and escaped string delimiters") {
      val config      = RemoteGraphQLConfig.default.withAcquisition(_.withMaxParsingDepth(32))
      val delimiters  = "[({" * 64
      val quoted      = "\"escaped \\\" " + delimiters + "\""
      val block       = "\"\"\"escaped \\\"\"\" " + delimiters + "\"\"\""
      val description = "# " + delimiters + "\r\n" + block + "\n"
      val shallow     = description + s"type Query { value(arg: String = $quoted): String }"
      val nested      = "[" * 4096 + "String" + "]" * 4096
      val deep        = description + s"type Query { value(arg: $nested): String }"

      for {
        allowedSource <- stub(serviceResponse(shallow))
        allowed       <- Gateway.compose(Subgraph.federation("shallow", allowedSource.endpoint, config)).interpreter.either
        deniedSource  <- stub(serviceResponse(deep))
        denied        <- Gateway.compose(Subgraph.federation("deep", deniedSource.endpoint, config)).interpreter.either
        introspection <- introspectionResponse
        defaultSource <- stub(
                           introspection.replaceFirst(
                             "\"defaultValue\":null",
                             "\"defaultValue\":\"" + "[" * 4096 + "null" + "]" * 4096 + "\""
                           )
                         )
        deniedDefault <- Gateway.compose(Subgraph.graphql("default", defaultSource.endpoint, config)).interpreter.either
      } yield assertTrue(
        allowed.isRight,
        denied.left.exists(_.diagnostics.exists(_.contains("parsing depth exceeded 32"))),
        deniedDefault.left.exists(_.diagnostics.exists(_.contains("parsing depth exceeded 32")))
      )
    },
    test("releases acquisition response streams on success, failure, timeout, and interruption") {
      val timeoutConfig = RemoteGraphQLConfig.default.withAcquisition(
        _.withTimeout(1.second)
      )

      for {
        successTracked                                   <- tracked(serviceResponse(reviewsSchema))
        (successStream, successReleases, successReleased) = successTracked
        successEndpoint                                  <- streamingEndpoint(successStream)
        success                                          <- Gateway.compose(Subgraph.federation("success", successEndpoint)).interpreter.either
        _                                                <- successReleased.await
        failureTracked                                   <- tracked(invalidResponse)
        (failureStream, failureReleases, failureReleased) = failureTracked
        failureEndpoint                                  <- streamingEndpoint(failureStream)
        failure                                          <- Gateway.compose(Subgraph.federation("failure", failureEndpoint)).interpreter.either
        _                                                <- failureReleased.await
        timeoutStarted                                   <- Promise.make[Nothing, Unit]
        timeoutReleases                                  <- Ref.make(0)
        timeoutReleased                                  <- Promise.make[Nothing, Unit]
        timeoutEndpoint                                  <- streamingEndpoint(
                                                              (ZStream.fromZIO(timeoutStarted.succeed(()).unit).drain ++ ZStream.never).ensuring(
                                                                timeoutReleases.update(_ + 1) *> timeoutReleased.succeed(()).unit
                                                              )
                                                            )
        timeoutFiber                                     <- Gateway
                                                              .compose(Subgraph.federation("timeout", timeoutEndpoint, timeoutConfig))
                                                              .interpreter
                                                              .either
                                                              .fork
        _                                                <- timeoutStarted.await
        _                                                <- TestClock.adjust(2.seconds)
        timeoutResult                                    <- timeoutFiber.join
        _                                                <- timeoutReleased.await
        interruptStarted                                 <- Promise.make[Nothing, Unit]
        responseComplete                                 <- Promise.make[Nothing, Unit]
        interruptReleases                                <- Ref.make(0)
        interruptReleased                                <- Promise.make[Nothing, Unit]
        interruptEndpoint                                <- streamingEndpoint(
                                                              (ZStream.fromZIO(interruptStarted.succeed(()).unit).drain ++
                                                                ZStream.fromZIO(responseComplete.await).drain).ensuring(
                                                                interruptReleases.update(_ + 1) *> interruptReleased.succeed(()).unit
                                                              )
                                                            )
        interruptFiber                                   <- Gateway.compose(Subgraph.federation("interrupt", interruptEndpoint)).interpreter.fork
        _                                                <- interruptStarted.await
        _                                                <- interruptFiber.interruptFork
        _                                                <- responseComplete.succeed(())
        interrupted                                      <- interruptFiber.await
        _                                                <- interruptReleased.await
        successCount                                     <- successReleases.get
        failureCount                                     <- failureReleases.get
        timeoutCount                                     <- timeoutReleases.get
        interruptCount                                   <- interruptReleases.get
      } yield assertTrue(
        success.isRight,
        failure.isLeft,
        timeoutResult.left.exists(_.diagnostics.exists(_.contains("timed out"))),
        interrupted.isInterrupted,
        successCount == 1,
        failureCount == 1,
        timeoutCount == 1,
        interruptCount == 1
      )
    },
    test("does not retain failed or interrupted build resources in the caller scope") {
      val protectedConfig = RemoteGraphQLConfig.default.withAcquisition(
        _.withHeaders(SttpHeader("Content-Encoding", "gzip"))
      )

      for {
        parent               <- Scope.make
        initialSize           = parent.size
        failed               <- parent.extend(
                                  Gateway
                                    .compose(
                                      Subgraph.federation(
                                        "failed",
                                        unreachableEndpoint,
                                        protectedConfig
                                      )
                                    )
                                    .interpreter
                                    .either
                                )
        sizeAfterFailure      = parent.size
        interruptStarted     <- Promise.make[Nothing, Unit]
        responseComplete     <- Promise.make[Nothing, Unit]
        interruptReleased    <- Promise.make[Nothing, Unit]
        interruptEndpoint    <- streamingEndpoint(
                                  (ZStream.fromZIO(interruptStarted.succeed(()).unit).drain ++
                                    ZStream.fromZIO(responseComplete.await).drain).ensuring(
                                    interruptReleased.succeed(()).unit
                                  )
                                )
        interruptedBuild     <-
          parent
            .extend(Gateway.compose(Subgraph.federation("interrupted", interruptEndpoint)).interpreter)
            .fork
        _                    <- interruptStarted.await
        _                    <- interruptedBuild.interruptFork
        _                    <- responseComplete.succeed(())
        interrupted          <- interruptedBuild.await
        _                    <- interruptReleased.await
        sizeAfterInterruption = parent.size
        _                    <- parent.close(Exit.succeed(()))
      } yield assertTrue(
        failed.isLeft,
        interrupted.isInterrupted,
        sizeAfterFailure == initialSize,
        sizeAfterInterruption == initialSize
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

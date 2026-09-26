package caliban.gateway

import caliban.Value.{ BooleanValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.schema.{ GenericSchema, Schema }
import caliban.{ graphQL, RootResolver }
import zio._
import zio.http.{ Body, Header, Headers, Response, Status }
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

  private def introspectionErrorMessages(result: Either[GatewayBuildError, Any]): List[String] =
    result match {
      case Left(
            GatewayBuildError.SubgraphLoadingFailed(
              List(SubgraphError(_, SubgraphAcquisitionError.IntrospectionErrors(errors)))
            )
          ) =>
        errors.map(_.msg)
      case _ => Nil
    }

  def spec = suite("SchemaAcquisitionSpec")(
    test("acquires ordinary introspection and Federation service SDL through pinned composition") {
      for {
        introspection     <- introspectionResponse(ProductsApi.api)
        acquiredApi       <- stub(introspection, productResponse)
        pinnedApi         <- stub(productResponse)
        federationApi     <- stub(serviceResponse(reviewsSchema), reviewResponse)
        acquired          <- Gateway
                               .compose(
                                 Subgraph.graphql("products", acquiredApi.endpoint),
                                 Subgraph.federation("reviews", federationApi.endpoint)
                               )
                               .interpreter
        pinned            <- Gateway
                               .compose(
                                 Subgraph.graphql("products", pinnedApi.endpoint, ProductsApi.api.toDocument),
                                 Subgraph.federation("reviews", federationApi.endpoint, reviewsSchema)
                               )
                               .interpreter
        acquiredResult    <- acquired.execute("{ product { name } review { body } }")
        pinnedResult      <- pinned.execute("{ product { name } review { body } }")
        acquiredApiSent   <- acquiredApi.requests.get
        federationApiSent <- federationApi.requests.get
      } yield assertTrue(
        acquiredResult == pinnedResult,
        field(acquiredResult.data, "product").flatMap(field(_, "name")).contains(StringValue("Table")),
        field(acquiredResult.data, "review").flatMap(field(_, "body")).contains(StringValue("Solid")),
        acquiredApiSent.headOption.flatMap(_.query).exists(_.contains("__schema")),
        federationApiSent.headOption.flatMap(_.query).exists(_.contains("_service"))
      )
    },
    test("classifies introspection GraphQL errors when data is present or null") {
      val nullDataResponse = """{"data":null,"errors":[{"message":"introspection failed"}]}"""

      for {
        introspection <- introspectionResponse(ProductsApi.api)
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
        runtime    <- Gateway.compose(Subgraph.federation("metadata", source.endpoint)).interpreter
        response   <-
          runtime.execute(
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
        introspection     <- introspectionResponse(ProductsApi.api)
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
    test("attributes one acquisition failure") {
      for {
        introspection <- introspectionResponse(ProductsApi.api)
        ordinary      <- stub(introspection)
        broken        <- stub(invalidResponse)
        result        <- Gateway
                           .compose(
                             Subgraph.graphql("products", ordinary.endpoint),
                             Subgraph.federation("reviews", broken.endpoint)
                           )
                           .interpreter
                           .either
        ordinarySent  <- ordinary.requests.get
        brokenSent    <- broken.requests.get
      } yield assertTrue(
        result.left.exists(_.diagnostics.exists(_.startsWith("[reviews]"))),
        result.left.exists(!_.diagnostics.exists(_.startsWith("[products]"))),
        ordinarySent.size == 1,
        brokenSent.size == 1
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
                List(SubgraphError("reviews", SubgraphAcquisitionError.FederationErrors(errors)))
              ) =>
            errors.nonEmpty
          case _ => false
        }
      )
    },
    test("classifies malformed Federation service responses") {
      val cases = List(
        "[]"                           -> "$",
        """{"errors":true}"""          -> "$.errors",
        invalidResponse                -> "$.data",
        """{"data":{}}"""              -> "$.data._service",
        """{"data":{"_service":{}}}""" -> "$.data._service.sdl"
      )

      ZIO
        .foreach(cases) { case (response, expected) =>
          for {
            remote <- stub(response)
            result <- Gateway.compose(Subgraph.federation("reviews", remote.endpoint)).interpreter.either
          } yield result.left.toOption.collect {
            case GatewayBuildError.SubgraphLoadingFailed(
                  List(SubgraphError("reviews", SchemaAcquisitionError.InvalidResponse(path)))
                ) =>
              path
          }.contains(expected)
        }
        .map(results => assertTrue(results.forall(value => value)))
    },
    test("rejects introspection types and type references that are nameless, unwrapped, or doubly non-null") {
      val cases = List(
        (
          """"kind":"OBJECT","name":"Product"""",
          """"kind":"OBJECT","name":null""",
          """^\$\.data\.__schema\.types\[\d+\]\.name$"""
        ),
        (
          """"types":[""",
          """"types":[{"kind":"LIST","name":"Wrapper"},""",
          """^\$\.data\.__schema\.types\[0\]\.kind$"""
        ),
        (""""name":"String","ofType":null""", """"name":null,"ofType":null""", """\.type\.ofType\.name$"""),
        (""""ofType":{"kind":"SCALAR","name":"String","ofType":null}""", """"ofType":null""", """\.type\.ofType$"""),
        (
          """"queryType":{"name":"Query"}""",
          """"queryType":{"name":null}""",
          """^\$\.data\.__schema\.queryType\.name$"""
        ),
        (
          """"ofType":{"kind":"SCALAR","name":"String","ofType":null}""",
          """"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}}""",
          """\.ofType\.kind$"""
        )
      )

      introspectionResponse(ProductsApi.api).flatMap { introspection =>
        ZIO
          .foreach(cases) { case (from, to, expected) =>
            val response = introspection.replace(from, to)
            for {
              remote <- stub(response)
              result <- Gateway.compose(Subgraph.graphql("products", remote.endpoint)).interpreter.either
            } yield response != introspection && result.left.toOption.collect {
              case GatewayBuildError.SubgraphLoadingFailed(
                    List(SubgraphError("products", SchemaAcquisitionError.InvalidResponse(path)))
                  ) =>
                path
            }.exists(expected.r.findFirstIn(_).isDefined)
          }
          .map(results => assertTrue(results == cases.map(_ => true)))
      }
    },
    test("rejects an introspected default value that is not a GraphQL literal") {
      for {
        introspection <- introspectionResponse(ProductsApi.api)
        response       = introspection.replaceFirst("\"defaultValue\":null", "\"defaultValue\":\"{ unclosed\"")
        remote        <- stub(response)
        result        <- Gateway.compose(Subgraph.graphql("products", remote.endpoint)).interpreter.either
      } yield assertTrue(
        response != introspection,
        result.left.toOption.exists {
          case GatewayBuildError.SubgraphLoadingFailed(
                List(SubgraphError("products", _: SchemaAcquisitionError.SchemaParsingFailed))
              ) =>
            true
          case _ => false
        }
      )
    },
    test("rejects duplicate subgraph names before loading remote schemas") {
      for {
        valid      <- stub(serviceResponse(reviewsSchema))
        broken     <- stub(invalidResponse)
        result     <- Gateway
                        .compose(
                          Subgraph.federation("reviews", valid.endpoint),
                          Subgraph.federation("reviews", broken.endpoint)
                        )
                        .interpreter
                        .either
        validSent  <- valid.requests.get
        brokenSent <- broken.requests.get
      } yield assertTrue(
        result.left.exists {
          case GatewayBuildError.InvalidConfiguration(_) => true
          case _                                         => false
        },
        result.left.exists(_.diagnostics.exists(_.contains("Name is used more than once"))),
        !result.left.exists(_.diagnostics.exists(_.contains("invalid at '$.data'"))),
        validSent.isEmpty,
        brokenSent.isEmpty
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
      val clientError = new RuntimeException("secret decoder context", cause)
      val error       = SchemaAcquisitionError.ResponseDecodingFailed(clientError)

      assertTrue(
        error.getCause eq clientError,
        !error.diagnostics.exists(_.contains(clientError.getMessage)),
        !error.diagnostics.exists(_.contains(cause.getMessage))
      )
    },
    test("enforces acquisition headers, redirects, and finite response limits") {
      val headersConfig   = RemoteGraphQLConfig.default.withAcquisition(
        _.withHeaders(
          Header.Custom("Authorization", "Bearer schema"),
          Header.Custom("X-Multi", "first"),
          Header.Custom("X-Multi", "second")
        )
      )
      val protectedConfig = RemoteGraphQLConfig.default.withAcquisition(
        _.withHeaders(
          Header.Custom("Content-Type", "text/plain"),
          Header.Custom("Content-Encoding", "gzip")
        )
      )
      val responseLimit   = RemoteGraphQLConfig.default.withAcquisition(
        _.withMaxResponseBytes(32)
      )

      for {
        headerStub         <- stub(serviceResponse(reviewsSchema), reviewResponse)
        headerGateway      <- Gateway
                                .compose(Subgraph.federation("headers", headerStub.endpoint, headersConfig))
                                .interpreter
        _                  <- headerGateway.execute("{ review { body } }")
        sentHeaders        <- headerStub.headers.get
        protectedStub      <- stub(serviceResponse(reviewsSchema))
        protectedResult    <- Gateway
                                .compose(Subgraph.federation("protected", protectedStub.endpoint, protectedConfig))
                                .interpreter
                                .either
        protectedStubSent  <- protectedStub.requests.get
        boundedStub        <- stub(serviceResponse(reviewsSchema))
        boundedResult      <- Gateway
                                .compose(Subgraph.federation("bounded", boundedStub.endpoint, responseLimit))
                                .interpreter
                                .either
        redirectTarget     <- stub(serviceResponse(reviewsSchema))
        redirects          <- Ref.make(0)
        redirectEndpoint   <- postEndpoint("redirect")(_ =>
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
        redirectResult     <- Gateway.compose(Subgraph.federation("redirect", redirectEndpoint)).interpreter.either
        redirectCount      <- redirects.get
        redirectTargetSent <- redirectTarget.requests.get
        acquisitionMulti    = sentHeaders.headOption.fold(List.empty[String])(renderedHeaderValues(_, "X-Multi"))
      } yield assertTrue(
        sentHeaders.headOption.flatMap(_.get("Authorization")).contains("Bearer schema"),
        acquisitionMulti == List("first, second"),
        sentHeaders.headOption.flatMap(_.get("Content-Type")).exists(_.startsWith("application/json")),
        sentHeaders.headOption.flatMap(_.get("Accept")).exists(_.contains("application/graphql-response+json")),
        sentHeaders.lift(1).flatMap(_.get("Authorization")).isEmpty,
        sentHeaders.lift(1).flatMap(_.get("X-Multi")).isEmpty,
        protectedResult.left.exists(_.diagnostics.exists(_.contains("header 'Content-Type' is owned"))),
        protectedResult.left.exists(_.diagnostics.exists(_.contains("header 'Content-Encoding' is owned"))),
        protectedStubSent.isEmpty,
        boundedResult.left.exists(_.diagnostics.exists(_.contains("response exceeded 32 bytes"))),
        redirectResult.left.exists(_.diagnostics.exists(_.startsWith("[redirect]"))),
        redirectCount == 1,
        redirectTargetSent.isEmpty
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
        introspection <- introspectionResponse(ProductsApi.api)
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
        timeoutEndpoint                                  <-
          streamingEndpoint(
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
        interruptEndpoint                                <-
          streamingEndpoint(
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
        _.withHeaders(Header.Custom("Content-Encoding", "gzip"))
      )

      for {
        parent               <- Scope.make
        initialSize           = parent.size
        failed               <-
          parent.extend(
            Gateway.compose(Subgraph.federation("failed", unreachableEndpoint, protectedConfig)).interpreter.either
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
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential @@ TestAspect.timeout(30.seconds)
}

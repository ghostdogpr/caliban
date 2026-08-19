package caliban.gateway

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.schema.{ GenericSchema, Schema }
import caliban.tools.IntrospectionClient
import caliban.{ graphQL, GraphQLResponse, RootResolver }
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import sttp.model.{ Header => SttpHeader, Uri }
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

  private def streamingEndpoint(stream: ZStream[Any, Throwable, Byte]): ZIO[Server with Ref[Int], Nothing, Uri] =
    for {
      id     <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
      path    = s"acquisition-$id"
      handler = Handler.fromFunction[Request](_ =>
                  Response(
                    Status.Ok,
                    Headers(Header.Custom("Content-Type", "application/graphql-response+json")),
                    Body.fromStreamChunked(stream)
                  )
                )
      server <- ZIO.service[Server]
      _      <- server.install(Routes(Method.POST / path -> handler))
      port   <- server.port
    } yield Uri.unsafeParse(s"http://127.0.0.1:$port/$path")

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
                             .build
        pinned          <- Gateway
                             .compose(
                               Subgraph.graphql("products", pinnedApi.endpoint, ProductsApi.api.toDocument),
                               Subgraph.federation("reviews", federationApi.endpoint, reviewsSchema)
                             )
                             .build
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
        gateway    <- Gateway.compose(Subgraph.federation("metadata", source.endpoint)).build
        response   <-
          gateway.execute(
            "{ nested: __type(name: \"Nested\") { fields(includeDeprecated: true) { name isDeprecated deprecationReason } } state: __type(name: \"State\") { enumValues(includeDeprecated: true) { name isDeprecated deprecationReason } } scalar: __type(name: \"URL\") { specifiedByURL } }"
          )
        nested      = field(response.data, "nested")
                        .flatMap(field(_, "fields"))
                        .collect { case ListValue(values) => values }
                        .getOrElse(Nil)
        states      = field(response.data, "state")
                        .flatMap(field(_, "enumValues"))
                        .collect { case ListValue(values) => values }
                        .getOrElse(Nil)
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
                               .build
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
                           .build
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
    test("enforces acquisition headers, redirects, and finite response and parsing limits") {
      val headersConfig   = SchemaAcquisition.default.withHeaders(SttpHeader("Authorization", "Bearer schema"))
      val protectedConfig = SchemaAcquisition.default.withHeaders(
        SttpHeader("Content-Type", "text/plain"),
        SttpHeader("Content-Encoding", "gzip")
      )
      val responseLimit   = SchemaAcquisition.default.withMaxResponseBytes(32)
      val parsingLimit    = SchemaAcquisition.default.withMaxParsingDepth(4)
      val ordinaryLimit   = SchemaAcquisition.default.withMaxParsingDepth(32)
      val nestedSchema    = reviewsSchema.replace("body: String!", "body(arg: [[[[[String]]]]]): String!")
      val nestedDefault   = List.fill(40)("[").mkString + "null" + List.fill(40)("]").mkString

      for {
        introspection   <- introspectionResponse
        headerStub      <- stub(serviceResponse(reviewsSchema), reviewResponse)
        headerGateway   <- Gateway
                             .compose(Subgraph.federation("headers", headerStub.endpoint, headersConfig))
                             .build
        _               <- headerGateway.execute("{ review { body } }")
        sentHeaders     <- headerStub.headers.get
        protectedStub   <- stub(serviceResponse(reviewsSchema))
        protectedResult <- Gateway
                             .compose(Subgraph.federation("protected", protectedStub.endpoint, protectedConfig))
                             .build
                             .either
        protectedCalls  <- protectedStub.requests.get
        boundedStub     <- stub(serviceResponse(reviewsSchema))
        boundedResult   <- Gateway
                             .compose(Subgraph.federation("bounded", boundedStub.endpoint, responseLimit))
                             .build
                             .either
        parsingStub     <- stub(serviceResponse(nestedSchema))
        parsingResult   <- Gateway
                             .compose(Subgraph.federation("parsing", parsingStub.endpoint, parsingLimit))
                             .build
                             .either
        ordinaryStub    <- stub(
                             introspection.replaceFirst(
                               "\"defaultValue\":null",
                               "\"defaultValue\":\"" + nestedDefault + "\""
                             )
                           )
        ordinaryResult  <- Gateway
                             .compose(Subgraph.graphql("ordinary-parsing", ordinaryStub.endpoint, ordinaryLimit))
                             .build
                             .either
        redirectTarget  <- stub(serviceResponse(reviewsSchema))
        redirects       <- Ref.make(0)
        id              <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
        path             = s"redirect-$id"
        server          <- ZIO.service[Server]
        _               <- server.install(
                             Routes(
                               Method.POST / path -> Handler.fromFunctionZIO[Request](_ =>
                                 redirects
                                   .update(_ + 1)
                                   .as(
                                     Response(
                                       Status.TemporaryRedirect,
                                       Headers(Header.Custom("Location", redirectTarget.endpoint.toString)),
                                       Body.empty
                                     )
                                   )
                               )
                             )
                           )
        port            <- server.port
        redirectResult  <- Gateway
                             .compose(
                               Subgraph.federation(
                                 "redirect",
                                 Uri.unsafeParse(s"http://127.0.0.1:$port/$path")
                               )
                             )
                             .build
                             .either
        redirectCount   <- redirects.get
        targetCalls     <- redirectTarget.requests.get
      } yield assertTrue(
        sentHeaders.headOption.flatMap(_.get("Authorization")).contains("Bearer schema"),
        sentHeaders.headOption.flatMap(_.get("Content-Type")).exists(_.startsWith("application/json")),
        sentHeaders.headOption.flatMap(_.get("Accept")).exists(_.contains("application/graphql-response+json")),
        sentHeaders.lift(1).flatMap(_.get("Authorization")).isEmpty,
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
    test("releases acquisition response streams on success, failure, timeout, and interruption") {
      def finite(body: String, released: Promise[Nothing, Unit]): ZStream[Any, Throwable, Byte] =
        ZStream
          .fromChunk(Chunk.fromArray(body.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
          .ensuring(
            released.succeed(()).unit
          )

      val timeoutConfig = SchemaAcquisition.default.withTimeout(1.second)

      for {
        successReleased   <- Promise.make[Nothing, Unit]
        successEndpoint   <- streamingEndpoint(finite(serviceResponse(reviewsSchema), successReleased))
        success           <- Gateway.compose(Subgraph.federation("success", successEndpoint)).build.either
        _                 <- successReleased.await
        failureReleased   <- Promise.make[Nothing, Unit]
        failureEndpoint   <- streamingEndpoint(finite(invalidResponse, failureReleased))
        failure           <- Gateway.compose(Subgraph.federation("failure", failureEndpoint)).build.either
        _                 <- failureReleased.await
        timeoutStarted    <- Promise.make[Nothing, Unit]
        timeoutReleased   <- Promise.make[Nothing, Unit]
        timeoutEndpoint   <- streamingEndpoint(
                               (ZStream.fromZIO(timeoutStarted.succeed(()).unit).drain ++ ZStream.never).ensuring(
                                 timeoutReleased.succeed(()).unit
                               )
                             )
        timeoutFiber      <- Gateway
                               .compose(Subgraph.federation("timeout", timeoutEndpoint, timeoutConfig))
                               .build
                               .either
                               .fork
        _                 <- timeoutStarted.await
        _                 <- TestClock.adjust(2.seconds)
        timeoutResult     <- timeoutFiber.join
        _                 <- timeoutReleased.await
        interruptStarted  <- Promise.make[Nothing, Unit]
        interruptReleased <- Promise.make[Nothing, Unit]
        interruptEndpoint <- streamingEndpoint(
                               (ZStream.fromZIO(interruptStarted.succeed(()).unit).drain ++ ZStream.never).ensuring(
                                 interruptReleased.succeed(()).unit
                               )
                             )
        interruptFiber    <- Gateway.compose(Subgraph.federation("interrupt", interruptEndpoint)).build.fork
        _                 <- interruptStarted.await
        interrupted       <- interruptFiber.interrupt
        _                 <- interruptReleased.await
      } yield assertTrue(
        success.isRight,
        failure.isLeft,
        timeoutResult.left.exists(_.diagnostics.exists(_.contains("timed out"))),
        interrupted.isInterrupted
      )
    },
    test("does not retain failed or interrupted build resources in the caller scope") {
      val protectedConfig = SchemaAcquisition.default.withHeaders(SttpHeader("Content-Encoding", "gzip"))

      for {
        parent               <- Scope.make
        initialSize           = parent.size
        failed               <- parent.extend(
                                  Gateway
                                    .compose(
                                      Subgraph.federation(
                                        "failed",
                                        Uri.unsafeParse("http://127.0.0.1:1/graphql"),
                                        protectedConfig
                                      )
                                    )
                                    .build
                                    .either
                                )
        sizeAfterFailure      = parent.size
        interruptStarted     <- Promise.make[Nothing, Unit]
        interruptReleased    <- Promise.make[Nothing, Unit]
        interruptEndpoint    <- streamingEndpoint(
                                  (ZStream.fromZIO(interruptStarted.succeed(()).unit).drain ++ ZStream.never).ensuring(
                                    interruptReleased.succeed(()).unit
                                  )
                                )
        interruptedBuild     <- parent
                                  .extend(Gateway.compose(Subgraph.federation("interrupted", interruptEndpoint)).build)
                                  .fork
        _                    <- interruptStarted.await
        interrupted          <- interruptedBuild.interrupt
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

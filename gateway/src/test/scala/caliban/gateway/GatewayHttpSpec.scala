package caliban.gateway

import caliban.Configurator.ExecutionConfiguration
import caliban.Value.{ NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.schema.{ GenericSchema, Schema }
import caliban._
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromString, writeToString }
import sttp.model.{ Header => SttpHeader }
import zio._
import zio.http._
import zio.test._

object GatewayHttpSpec extends ZIOSpecDefault {

  private final case class HttpResult(response: Response, body: String)

  private object TimeoutApi extends GenericSchema[Any] {
    import auto._
    final case class Query(delayed: UIO[String])
    implicit val querySchema: Schema[Any, Query] = gen
    val api                                      = graphQL(RootResolver(Query(ZIO.never)))
  }

  private val schema =
    """
      |schema { query: Query mutation: Mutation }
      |type Query { greeting: String! failing: String }
      |type Mutation { setValue(value: String!): String! }
      |""".stripMargin

  private val parityProductsSchema =
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"]) {
       |  query: Query
       |  mutation: Mutation
       |}
       |$federationDirectives
       |type Query { product(id: ID!): Product }
       |type Mutation { renameProduct(id: ID!, name: String!): Product! }
       |type Product @key(fields: "id") { id: ID! name: String! }
       |""".stripMargin

  private val parityReviewsSchema =
    s"""
       |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
       |$federationDirectives
       |type Product @key(fields: "id") { id: ID! @external reviews: [Review]! }
       |type Review { body: String! }
       |""".stripMargin

  private def install(adapter: QuickAdapter[Any]): ZIO[Server with Ref[Int], Nothing, URL] =
    for {
      id     <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
      path    = s"gateway-http-$id"
      server <- ZIO.service[Server]
      _      <- server.install(adapter.routes(s"/$path"))
      port   <- server.port
      url    <- ZIO.fromEither(URL.decode(s"http://127.0.0.1:$port/$path")).orDie
    } yield url

  private def post(url: URL, body: String, accept: String = "application/graphql-response+json"): Request =
    Request
      .post(url, Body.fromString(body).contentType(MediaType.application.json))
      .addHeader(Header.Custom("Accept", accept))

  private def execute(request: Request): ZIO[Client, Throwable, HttpResult] =
    Client.batched(request).flatMap(response => response.body.asString.map(HttpResult(response, _)))

  private def responseValue(body: String): ResponseValue =
    readFromString[ResponseValue](body)

  def spec = suite("Gateway HTTP")(
    suite("execution")(
      test("serves queries and mutations while preserving request and FiberRef header context") {
        for {
          executionHeader <- FiberRef.make("missing")
          source          <- stubByRequest { request =>
                               if (request.query.exists(_.contains("setValue")))
                                 """{"data":{"setValue":"saved"}}"""
                               else
                                 """{"data":{"greeting":"hello"},"extensions":{"cacheControl":{"httpHeader":"max-age=60"}}}"""
                             }
          config           = RemoteGraphQLConfig.default
                               .withExecution(
                                 _.forwardIncomingHeaders("X-Client")
                               )
                               .withExecutionHeadersZIO(
                                 executionHeader.get.map(value => List(SttpHeader("X-Fiber", value)))
                               )
          runtime         <- Gateway.compose(Subgraph.graphql("service", source.endpoint, schema, config)).interpreter
          url             <- install(
                               QuickAdapter(runtime).configure(executionHeader.locallyScoped("configured"))
                             )
          query           <- execute(
                               post(url, """{"query":"query { greeting }"}""")
                                 .addHeader(Header.Custom("X-Client", "forwarded"))
                             )
          mutation        <- execute(post(url, """{"query":"mutation { setValue(value: \"next\") }"}"""))
          headers         <- source.headers.get
        } yield assertTrue(
          query.response.status == Status.Ok,
          query.response.headers
            .get(Header.ContentType)
            .exists(_.mediaType.fullType == "application/graphql-response+json"),
          query.response.headers.get(Header.CacheControl).exists(_.renderedValue == "max-age=60"),
          field(responseValue(query.body), "data").flatMap(field(_, "greeting")).contains(StringValue("hello")),
          mutation.response.status == Status.Ok,
          field(responseValue(mutation.body), "data")
            .flatMap(field(_, "setValue"))
            .contains(StringValue("saved")),
          headers.head.get("X-Client").contains("forwarded"),
          headers.head.get("X-Fiber").contains("configured")
        )
      },
      test("preserves disabled introspection through Quick configuration") {
        for {
          source  <- stub("""{"data":{"greeting":"hello"}}""")
          runtime <- Gateway.compose(Subgraph.graphql("service", source.endpoint, schema)).interpreter
          url     <- install(QuickAdapter(runtime).configure(ExecutionConfiguration(enableIntrospection = false)))
          result  <- execute(post(url, """{"query":"{ __schema { queryType { name } } }"}"""))
          calls   <- source.requests.get
        } yield assertTrue(
          result.response.status == Status.BadRequest,
          result.body.contains("Introspection is disabled"),
          calls.isEmpty
        )
      },
      test("returns timeouts as GraphQL execution results") {
        for {
          runtime        <- Gateway
                              .compose(Subgraph.local("service", TimeoutApi.api))
                              .withConfig(_.withRequestTimeout(20.millis))
                              .interpreter
          gqlFiber       <- QuickAdapter(runtime).handlers.api
                              .runZIO(post(URL.empty, """{"query":"{ delayed }"}"""))
                              .fork
          _              <- TestClock.adjust(20.millis)
          gqlResponse    <- gqlFiber.join
          gqlBody        <- gqlResponse.body.asString.orDie
          legacyFiber    <- QuickAdapter(runtime).handlers.api
                              .runZIO(post(URL.empty, """{"query":"{ delayed }"}""", "application/json"))
                              .fork
          _              <- TestClock.adjust(20.millis)
          legacyResponse <- legacyFiber.join
          legacyBody     <- legacyResponse.body.asString.orDie
        } yield assertTrue(
          gqlResponse.status == Status.GatewayTimeout,
          gqlBody.contains("Gateway request timed out."),
          !gqlBody.contains("\"data\""),
          legacyResponse.status == Status.GatewayTimeout,
          legacyBody.contains("Gateway request timed out.")
        )
      }
    ),
    suite("encoded responses")(
      test("preserves specialized response mapping through Quick configuration") {
        for {
          executionHeader <- FiberRef.make("missing")
          mapperContext   <- Promise.make[Nothing, (String, Boolean, Boolean)]
          interpreter      = new GraphQLInterpreter[Any, Any] {
                               def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] = ZIO.unit

                               def executeRequest(request: GraphQLRequest)(implicit
                                 trace: Trace
                               ): UIO[GraphQLResponse[Any]] =
                                 ZIO.dieMessage("structured fallback was used")

                               override private[caliban] def executeRequestWith[A](
                                 request: GraphQLRequest
                               )(f: GraphQLResponseContext.Classified[GraphQLResponse[Any]] => A)(implicit
                                 trace: Trace
                               ): UIO[A] =
                                 for {
                                   mapped <- ZIO.succeed(
                                               f(
                                                 GraphQLResponseContext.Classified(
                                                   GraphQLResponse(
                                                     ResponseValue.ObjectValue(
                                                       List("status" -> StringValue("mapped"))
                                                     ),
                                                     Nil
                                                   ),
                                                   GraphQLResponseContext.Outcome.Executed
                                                 )
                                               )
                                             )
                                   header <- executionHeader.get
                                   config <- Configurator.ref.get
                                   _      <- mapperContext.succeed(
                                               (header, config.enableIntrospection, mapped.isInstanceOf[Either[_, _]])
                                             )
                                 } yield mapped
                             }
          response        <- QuickAdapter(interpreter)
                               .configure(ExecutionConfiguration(enableIntrospection = false))
                               .configure(executionHeader.locallyScoped("configured"))
                               .handlers
                               .api
                               .runZIO(post(URL.empty, """{"query":"{ status }"}"""))
          body            <- response.body.asString.orDie
          context         <- mapperContext.await
        } yield assertTrue(
          response.status == Status.Ok,
          body == """{"data":{"status":"mapped"}}""",
          context == (("configured", false, true))
        )
      },
      test("matches structured execution for joins, partial errors, null propagation, and mutations") {
        val query    = """query { product(id: "p1") { name reviews { body } } }"""
        val mutation = """mutation { renameProduct(id: "p1", name: "next") { id name } }"""

        for {
          products           <- stubByRequest { request =>
                                  if (request.query.exists(_.contains("renameProduct")))
                                    """{"data":{"renameProduct":{"id":"p1","name":"next"}}}"""
                                  else
                                    """{"data":{"product":{"id":"p1","name":"original","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
                                }
          reviews            <-
            stub(
              """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"good"},{"body":null}]}]},"errors":[{"message":"bad review","path":["_entities",0,"reviews",1,"body"]}]}"""
            )
          runtime            <- Gateway
                                  .compose(
                                    Subgraph.federation("products", products.endpoint, parityProductsSchema),
                                    Subgraph.federation("reviews", reviews.endpoint, parityReviewsSchema)
                                  )
                                  .interpreter
          url                <- install(QuickAdapter(runtime))
          structuredQuery    <- runtime.executeRequest(GraphQLRequest(query = Some(query)))
          encodedQueryResult <- execute(post(url, writeToString(GraphQLRequest(query = Some(query)))))
          encodedQuery        = readFromString[GraphQLResponse[CalibanError]](encodedQueryResult.body)
          structuredMutation <- runtime.executeRequest(GraphQLRequest(query = Some(mutation)))
          encodedMutation    <- execute(post(url, writeToString(GraphQLRequest(query = Some(mutation)))))
          decodedMutation     = readFromString[GraphQLResponse[CalibanError]](encodedMutation.body)
        } yield assertTrue(
          encodedQueryResult.response.status == Status.Ok,
          encodedQuery.toResponseValue == structuredQuery.toResponseValue,
          field(encodedQuery.data, "product")
            .flatMap(field(_, "reviews"))
            .contains(
              ResponseValue.ListValue(
                List(ResponseValue.ObjectValue(List("body" -> StringValue("good"))), NullValue)
              )
            ),
          encodedQuery.errors.nonEmpty,
          decodedMutation.toResponseValue == structuredMutation.toResponseValue
        )
      },
      test("enforces the configured encoded response limit") {
        for {
          source   <- stub("""{"data":{"greeting":"hello"}}""")
          runtime  <- Gateway.compose(Subgraph.graphql("service", source.endpoint, schema)).interpreter
          response <- QuickAdapter(runtime)
                        .withMaxResponseBodyBytes(16)
                        .handlers
                        .api
                        .runZIO(post(URL.empty, """{"query":"{ greeting }"}"""))
          body     <- response.body.asString.orDie
        } yield assertTrue(
          response.status == Status.InternalServerError,
          body == "Encoded GraphQL response exceeds the configured limit."
        )
      }
    ),
    suite("GraphQL over HTTP")(
      test("serves trusted-document IDs without query text and rejects raw-text fallback") {
        for {
          source   <- stub("""{"data":{"greeting":"hello"}}""")
          runtime  <- Gateway
                        .compose(Subgraph.graphql("service", source.endpoint, schema))
                        .withOperationResolver(
                          OperationResolver.trustedDocuments(Map("greeting-v1" -> "{ greeting }")) { request =>
                            request.extensions.flatMap(_.get("documentId")).collect { case StringValue(id) => id }
                          }
                        )
                        .interpreter
          url      <- install(QuickAdapter(runtime))
          resolved <- execute(post(url, """{"extensions":{"documentId":"greeting-v1"}}"""))
          missing  <- execute(post(url, """{"query":"{ greeting }"}"""))
          unknown  <- execute(post(url, """{"query":"{ greeting }","extensions":{"documentId":"unknown"}}"""))
          calls    <- source.requests.get
        } yield assertTrue(
          resolved.response.status == Status.Ok,
          resolved.body == """{"data":{"greeting":"hello"}}""",
          missing.response.status == Status.Ok,
          missing.body.contains("\"code\":\"TRUSTED_DOCUMENT_ID_INVALID\""),
          unknown.response.status == Status.Ok,
          unknown.body.contains("\"code\":\"TRUSTED_DOCUMENT_NOT_FOUND\""),
          calls.size == 1
        )
      },
      test("serializes public resolver rejections as HTTP 200 while internal failures remain HTTP 500") {
        ZIO
          .foreach(List(false, true)) { observed =>
            for {
              source  <- stub("""{"data":{"greeting":"hello"}}""")
              gateway  = Gateway
                           .compose(Subgraph.graphql("service", source.endpoint, schema))
                           .withOperationResolver(OperationResolver[Any] { request =>
                             if (request.query.contains("internal")) ZIO.fail(new RuntimeException("resolver-secret"))
                             else
                               ZIO.fail(OperationResolver.Rejection("Document not found.", "PERSISTED_QUERY_NOT_FOUND"))
                           })
              runtime <- (if (observed) gateway @@ GatewayMetrics.wrapper else gateway).interpreter
              url     <- install(QuickAdapter(runtime))
              results <-
                ZIO.foreach(List("application/json", "application/graphql-response+json")) { mediaType =>
                  for {
                    rejected <- execute(post(url, """{"extensions":{"documentId":"unknown"}}""", mediaType))
                    failed   <- execute(post(url, """{"query":"internal"}""", mediaType))
                  } yield assertTrue(
                    rejected.response.status == Status.Ok,
                    rejected.body ==
                      """{"data":null,"errors":[{"message":"Document not found.","extensions":{"code":"PERSISTED_QUERY_NOT_FOUND"}}]}""",
                    failed.response.status == Status.InternalServerError,
                    failed.body.contains("Operation resolution failed."),
                    !failed.body.contains("resolver-secret"),
                    !failed.body.contains("PERSISTED_QUERY_NOT_FOUND")
                  )
                }
              calls   <- source.requests.get
            } yield results.reduce(_ && _) && assertTrue(calls.isEmpty)
          }
          .map(_.reduce(_ && _))
      },
      test("uses the negotiated request-error status and response media type") {
        for {
          source    <- stubByRequest { request =>
                         if (request.query.exists(_.contains("failing")))
                           """{"data":{"failing":null},"errors":[{"message":"source failed","path":["failing"]}]}"""
                         else """{"data":{"greeting":"hello"}}"""
                       }
          runtime   <- Gateway.compose(Subgraph.graphql("service", source.endpoint, schema)).interpreter
          url       <- install(QuickAdapter(runtime))
          gqlParse  <- execute(post(url, """{"query":"query {"}"""))
          legacy    <- execute(post(url, """{"query":"{ unknown }"}""", "application/json"))
          execution <- execute(post(url, """{"query":"{ failing }"}"""))
        } yield assertTrue(
          gqlParse.response.status == Status.BadRequest,
          gqlParse.response.headers
            .get(Header.ContentType)
            .exists(_.mediaType.fullType == "application/graphql-response+json"),
          !gqlParse.body.contains("\"data\""),
          legacy.response.status == Status.Ok,
          legacy.response.headers.get(Header.ContentType).exists(_.mediaType.fullType == "application/json"),
          legacy.body.contains("\"data\":null"),
          execution.response.status == Status.Ok,
          execution.body.contains("Remote GraphQL request failed.")
        )
      },
      test("keeps valid data when a remote error entry is malformed") {
        for {
          source  <-
            stub(
              """{"data":{"greeting":"hello"},"errors":[{"message":"warning","path":["greeting",1.5]},{"path":["greeting"]}]}"""
            )
          runtime <- Gateway
                       .compose(Subgraph.graphql("service", source.endpoint, schema))
                       .withConfig(_.withRemoteErrorMessages(true))
                       .interpreter
          result  <- runtime.execute("{ greeting }")
        } yield assertTrue(
          field(result.data, "greeting").contains(StringValue("hello")),
          result.errors.size == 2
        )
      },
      test("accepts an empty remote errors array when data is present") {
        for {
          source  <- stub("""{"data":{"greeting":"hello"},"errors":[]}""")
          runtime <- Gateway.compose(Subgraph.graphql("service", source.endpoint, schema)).interpreter
          result  <- runtime.execute("{ greeting }")
        } yield assertTrue(
          field(result.data, "greeting").contains(StringValue("hello")),
          result.errors.isEmpty
        )
      },
      test("rejects mutations over GET with Allow POST") {
        for {
          source  <- stub("""{"data":{"setValue":"saved"}}""")
          runtime <- Gateway.compose(Subgraph.graphql("service", source.endpoint, schema)).interpreter
          url     <- install(QuickAdapter(runtime))
          request  = Request
                       .get(url.addQueryParam("query", "mutation { setValue(value: \"next\") }"))
                       .addHeader(Header.Custom("Accept", "application/graphql-response+json"))
          result  <- execute(request)
          calls   <- source.requests.get
        } yield assertTrue(
          result.response.status == Status.MethodNotAllowed,
          result.response.headers.get(Header.Allow).exists(_.renderedValue == "POST"),
          !result.body.contains("\"data\""),
          calls.isEmpty
        )
      },
      test("rejects unsupported methods, media types, and response encodings") {
        for {
          source          <- stub("""{"data":{"greeting":"hello"}}""")
          runtime         <- Gateway.compose(Subgraph.graphql("service", source.endpoint, schema)).interpreter
          url             <- install(QuickAdapter(runtime))
          method          <- execute(Request(method = Method.DELETE, url = url))
          contentType     <- execute(
                               Request
                                 .post(
                                   url,
                                   Body.fromString("{ greeting }").contentType(MediaType.text.plain)
                                 )
                                 .addHeader(Header.Custom("Accept", "application/graphql-response+json"))
                             )
          accept          <- execute(post(url, """{"query":"{ greeting }"}""", "text/plain"))
          fallback        <- execute(
                               post(
                                 url,
                                 """{"query":"{ greeting }"}""",
                                 "application/graphql-response+json;q=0, application/json"
                               )
                             )
          multipart       <- execute(post(url, """{"query":"{ greeting }"}""", "multipart/mixed"))
          bounded         <- execute(
                               post(
                                 url,
                                 """{"query":"{ greeting }"}""",
                                 "multipart/mixed; boundary=\"graphql\"; deferSpec=20220824"
                               )
                             )
          quoted          <- execute(
                               post(
                                 url,
                                 """{"query":"{ greeting }"}""",
                                 "multipart/mixed; boundary=\"graphql\"; deferSpec=\"20220824\""
                               )
                             )
          multipartRange  <- execute(
                               post(
                                 url,
                                 """{"query":"{ greeting }"}""",
                                 "multipart/*; boundary=\"graphql\"; deferSpec=20220824"
                               )
                             )
          invalidBoundary <- execute(
                               post(url, """{"query":"{ greeting }"}""", "application/json; boundary=graphql")
                             )
          wildcard        <- execute(
                               post(url, """{"query":"{ greeting }"}""", "application/json;q=0, */*;q=1")
                             )
          parameters      <- execute(
                               post(
                                 url,
                                 """{"query":"{ greeting }"}""",
                                 "application/json;profile=unsupported;q=1, application/graphql-response+json;q=0.5"
                               )
                             )
        } yield assertTrue(
          method.response.status == Status.NotFound,
          contentType.response.status == Status.UnsupportedMediaType,
          accept.response.status == Status.NotAcceptable,
          fallback.response.headers.get(Header.ContentType).exists(_.mediaType.fullType == "application/json"),
          multipart.response.headers.get(Header.ContentType).exists(_.mediaType.fullType == "multipart/mixed"),
          multipart.body.contains("\"greeting\":\"hello\""),
          bounded.response.headers.get(Header.ContentType).exists(_.mediaType.fullType == "multipart/mixed"),
          bounded.body.contains("\"greeting\":\"hello\""),
          quoted.response.headers.get(Header.ContentType).exists(_.mediaType.fullType == "multipart/mixed"),
          quoted.body.contains("\"greeting\":\"hello\""),
          multipartRange.response.headers.get(Header.ContentType).exists(_.mediaType.fullType == "multipart/mixed"),
          multipartRange.body.contains("\"greeting\":\"hello\""),
          invalidBoundary.response.status == Status.NotAcceptable,
          wildcard.response.headers
            .get(Header.ContentType)
            .exists(_.mediaType.fullType == "application/graphql-response+json"),
          parameters.response.headers
            .get(Header.ContentType)
            .exists(_.mediaType.fullType == "application/graphql-response+json")
        )
      },
      test("does not infer a server failure from a pathless execution error") {
        val interpreter = new GraphQLInterpreter[Any, CalibanError] {
          def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] = ZIO.unit

          def executeRequest(request: GraphQLRequest)(implicit
            trace: Trace
          ): UIO[GraphQLResponse[CalibanError]] =
            ZIO.succeed(GraphQLResponse(NullValue, List(CalibanError.ExecutionError("pathless"))))
        }

        for {
          response <- QuickAdapter(interpreter).handlers.api
                        .runZIO(post(URL.empty, """{"query":"{ greeting }"}"""))
          body     <- response.body.asString.orDie
        } yield assertTrue(response.status == Status.Ok, body.contains("pathless"))
      },
      test("rejects request bodies larger than the finite default") {
        for {
          source  <- stub("""{"data":{"greeting":"hello"}}""")
          runtime <- Gateway.compose(Subgraph.graphql("service", source.endpoint, schema)).interpreter
          url     <- install(QuickAdapter(runtime))
          body     = """{"query":"{ greeting }"}""" + (" " * (1024 * 1024))
          result  <- execute(post(url.addQueryParam("query", "{ greeting }"), body))
          calls   <- source.requests.get
        } yield assertTrue(result.response.status == Status.RequestEntityTooLarge, calls.isEmpty)
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds, ZClient.default) @@ TestAspect.sequential
}

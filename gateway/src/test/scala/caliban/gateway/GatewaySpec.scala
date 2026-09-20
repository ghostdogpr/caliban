package caliban.gateway

import caliban.InputValue.{ ListValue, ObjectValue => InputObjectValue }
import caliban.ResponseValue.{ ListValue => ResponseListValue, ObjectValue => ResponseObjectValue }
import caliban.Value.{ BooleanValue, EnumValue, IntValue, NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.execution.ResponseMerge
import caliban.parsing.Parser
import caliban.schema.{ GenericSchema, Schema }
import caliban.wrappers.ApolloPersistedQueries
import caliban.wrappers.Wrappers.maxDepth
import caliban._
import zio._
import zio.http.URL
import zio.test._

object GatewaySpec extends ZIOSpecDefault {

  private trait Greeting {
    def value: UIO[String]
  }

  private trait Audience {
    def value: UIO[String]
  }

  private object LocalSchemas {
    object GreetingApi extends GenericSchema[Greeting] {
      import auto._
      final case class Query(greeting: URIO[Greeting, String])
      implicit val querySchema: Schema[Greeting, Query] = gen
      val api                                           = graphQL(RootResolver(Query(ZIO.serviceWithZIO[Greeting](_.value))))
    }

    object AudienceApi extends GenericSchema[Audience] {
      import auto._
      final case class Query(audience: URIO[Audience, String])
      implicit val querySchema: Schema[Audience, Query] = gen
      val api                                           = graphQL(RootResolver(Query(ZIO.serviceWithZIO[Audience](_.value))))
    }

    sealed trait Status
    object Status {
      case object ACTIVE extends Status
    }

    object EnumApi extends GenericSchema[Any] {
      import auto._
      final case class Query(status: Status)
      implicit val statusSchema: Schema[Any, Status] = gen
      implicit val querySchema: Schema[Any, Query]   = gen
      val api                                        = graphQL(RootResolver(Query(Status.ACTIVE)))
    }

    object EchoApi extends GenericSchema[Any] {
      import auto._
      import caliban.schema.ArgBuilder
      final case class EchoArgs(value: String)
      final case class Query(echo: EchoArgs => String)
      implicit val argsBuilder: ArgBuilder[EchoArgs] = ArgBuilder.gen
      implicit val argsSchema: Schema[Any, EchoArgs] = gen
      implicit val querySchema: Schema[Any, Query]   = gen
      val api                                        = graphQL(RootResolver(Query(args => args.value)))
    }
  }

  private val productsSchema =
    """
      |type Query {
      |  products(ids: [ID!]!): [Product!]!
      |}
      |
      |type Product {
      |  id: ID!
      |  details: Details!
      |  reviews: [Review!]!
      |  legacyName: String @deprecated(reason: "Use details.name")
      |}
      |
      |type Details {
      |  name: String!
      |  legacyLabel: String @deprecated
      |  state: State
      |  url: URL
      |}
      |
      |type Review {
      |  body: String!
      |}
      |
      |scalar URL @specifiedBy(url: "https://example.com/url")
      |enum State { ACTIVE LEGACY @deprecated(reason: "Use ACTIVE") }
      |""".stripMargin

  private val nestedQuery =
    """
      |query Products($ids: [ID!]!, $includeReviews: Boolean!) {
      |  catalog: products(ids: $ids) {
      |    ...ProductDetails
      |    reviews @include(if: $includeReviews) {
      |      body
      |    }
      |  }
      |}
      |
      |fragment ProductDetails on Product {
      |  id
      |  details { name }
      |}
      |""".stripMargin

  private val dataResponse =
    """{"data":{"catalog":[{"id":"p1","details":{"name":"Table"},"reviews":[{"body":"Solid"}]}]}}"""

  private val errorsResponse =
    """{"errors":[{"message":"request rejected"}]}"""

  private def remoteGateway(endpoint: URL, schema: String = productsSchema, name: String = "products"): Gateway[Any] =
    Gateway.compose(Subgraph.graphql(name, endpoint, schema))

  private val echoAndStatus: Gateway[Any] =
    Gateway.compose(
      Subgraph.graphql("echo", LocalSchemas.EchoApi.api),
      Subgraph.graphql("status", LocalSchemas.EnumApi.api)
    )

  def spec = suite("GatewaySpec")(
    suite("local subgraphs")(
      test("uses explicit Federation 1 composition for local schemas without federation metadata") {
        val api = localGraph(ZIO.succeed("shared"))
        for {
          ordinary <- compositionDiagnostics(
                        Gateway.compose(Subgraph.graphql("first", api), Subgraph.graphql("second", api))
                      )
          runtime  <- Gateway.compose(Subgraph.federation("first", api), Subgraph.federation("second", api)).interpreter
          response <- runtime.execute("{ value }")
        } yield assertTrue(
          ordinary.exists(_.contains("Field is resolved by multiple ordinary subgraphs")),
          response.errors.isEmpty,
          field(response.data, "value").contains(StringValue("shared"))
        )
      },
      test("keeps ordinary local composition explicit even when the API has federation metadata") {
        val api = localGraph(ZIO.succeed("shared")) @@ caliban.federation.v2_6.federated
        compositionDiagnostics(
          Gateway.compose(Subgraph.graphql("first", api), Subgraph.graphql("second", api))
        ).map(diagnostics =>
          assertTrue(
            diagnostics.exists(message =>
              message.contains("query.value") && message.contains("Field is resolved by multiple ordinary subgraphs")
            )
          )
        )
      },
      test("executes local roots with their accumulated environments") {
        val description: Gateway[Greeting with Audience] = Gateway.compose(
          Subgraph.graphql("greeting", LocalSchemas.GreetingApi.api),
          Subgraph.graphql("audience", LocalSchemas.AudienceApi.api)
        )
        val environment                                  = ZLayer.succeed(new Greeting {
          def value: UIO[String] = ZIO.succeed("hello")
        }) ++ ZLayer.succeed(new Audience {
          def value: UIO[String] = ZIO.succeed("world")
        })

        (for {
          runtime  <- description.interpreter
          response <- runtime.execute("{ greeting audience }")
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "greeting").contains(StringValue("hello")),
          field(response.data, "audience").contains(StringValue("world"))
        )).provideSome[Scope](environment)
      },
      test("shares one cached plan and its caches across concurrent constant executions") {
        val query = """{ echo(value: "fixed") status }"""

        for {
          runtime   <- echoAndStatus.interpreter
          primed    <- runtime.execute(query)
          responses <- ZIO.foreachPar((1 to 24).toList)(_ => runtime.execute(query))
        } yield assertTrue(
          primed.errors.isEmpty,
          responses.forall(response =>
            response.errors.isEmpty &&
              field(response.data, "echo").contains(StringValue("fixed")) &&
              field(response.data, "status").contains(EnumValue("ACTIVE"))
          )
        )
      },
      test("binds concurrent variables independently across a multi-source plan") {
        val query = """query Echo($v: String!) { echo(value: $v) status }"""

        def echoRequest(value: String): GraphQLRequest =
          GraphQLRequest(
            query = Some(query),
            operationName = Some("Echo"),
            variables = Some(Map("v" -> StringValue(value)))
          )

        for {
          runtime   <- echoAndStatus.interpreter
          primed    <- runtime.executeRequest(echoRequest("primed"))
          responses <- ZIO.foreachPar((1 to 24).toList)(i => runtime.executeRequest(echoRequest(s"value-$i")))
        } yield assertTrue(
          primed.errors.isEmpty,
          field(primed.data, "echo").contains(StringValue("primed")),
          responses.forall(_.errors.isEmpty),
          responses.zipWithIndex.forall { case (response, i) =>
            field(response.data, "echo").contains(StringValue(s"value-${i + 1}")) &&
            field(response.data, "status").contains(EnumValue("ACTIVE"))
          }
        )
      },
      test("completes enum values returned by a local subgraph") {
        for {
          runtime  <- Gateway.compose(Subgraph.graphql("status", LocalSchemas.EnumApi.api)).interpreter
          response <- runtime.execute("{ status }")
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "status").contains(EnumValue("ACTIVE"))
        )
      },
      test("preserves FiberRef context and local Caliban failures") {
        for {
          context     <- FiberRef.make("initial")
          api          = {
            object Schema extends GenericSchema[Any] {
              import auto._
              final case class Query(
                context: UIO[String],
                failure: IO[CalibanError, String]
              )
              val api = graphQL(
                RootResolver(
                  Query(
                    context.get,
                    ZIO.fail(CalibanError.ExecutionError("local failure"))
                  )
                )
              )
            }
            Schema.api
          }
          interpreter <- ZIO.fromEither(api.interpreterEither).orDie
          runtime     <- Gateway.compose(Subgraph.graphql("local", api)).interpreter
          request      = GraphQLRequest(query = Some("{ context failure }"))
          direct      <- context.locally("request-context")(interpreter.executeRequest(request))
          response    <- context.locally("request-context")(runtime.executeRequest(request))
        } yield assertTrue(
          field(response.data, "context").contains(StringValue("request-context")),
          field(response.data, "failure").contains(NullValue),
          response.errors.collect { case error: CalibanError.ExecutionError => error.msg } == List("local failure"),
          response.errors == direct.errors
        )
      },
      test("isolates local request-error classification from the gateway request") {
        for {
          runtime    <- Gateway
                          .compose(Subgraph.graphql("local", localGraph(ZIO.succeed("ok")) @@ maxDepth(0)))
                          .interpreter
          classified <- GraphQLResponseContext.capture(runtime.execute("{ value }"))
        } yield assertTrue(
          classified.value.data == NullValue,
          classified.value.errors.map(_.msg) == List("Query is too deep: 1. Max depth: 0."),
          classified.outcome == GraphQLResponseContext.Outcome.Executed
        )
      },
      test("strips client extensions before executing a local subgraph") {
        val request = GraphQLRequest(
          query = Some("{ value }"),
          extensions = Some(
            Map(
              "persistedQuery" -> InputObjectValue(
                Map("sha256Hash" -> StringValue("client-query-hash"))
              )
            )
          )
        )

        for {
          runtime  <- Gateway
                        .compose(
                          Subgraph.graphql(
                            "local",
                            localGraph(ZIO.succeed("ok")) @@ ApolloPersistedQueries.wrapper
                          )
                        )
                        .interpreter
          response <- runtime.executeRequest(request)
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "value").contains(StringValue("ok"))
        )
      },
      test("preserves interruption of local Caliban execution") {
        for {
          started <- Promise.make[Nothing, Unit]
          api      = {
            object Schema extends GenericSchema[Any] {
              import auto._
              final case class Query(blocked: UIO[String])
              val api = graphQL(RootResolver(Query(started.succeed(()) *> ZIO.never)))
            }
            Schema.api
          }
          runtime <- Gateway.compose(Subgraph.graphql("local", api)).interpreter
          fiber   <- runtime.execute("{ blocked }").fork
          _       <- started.await
          exit    <- fiber.interrupt
        } yield assertTrue(exit.isInterrupted)
      }
    ),
    suite("single-source execution")(
      test("executes one pinned remote graph end to end through GatewayInterpreter") {
        for {
          remote                                            <- stub(dataResponse)
          runtime                                           <- remoteGateway(remote.endpoint).interpreter
          interpreter: GraphQLInterpreter[Any, CalibanError] = runtime
          request                                            = GraphQLRequest(
                                                                 query = Some(nestedQuery),
                                                                 operationName = Some("Products"),
                                                                 variables = Some(
                                                                   Map(
                                                                     "ids"            -> ListValue(List(StringValue("p1"))),
                                                                     "includeReviews" -> BooleanValue(true)
                                                                   )
                                                                 ),
                                                                 extensions = Some(Map("client" -> StringValue("gateway-spec")))
                                                               )
          response                                          <- interpreter.executeRequest(request)
          sent                                              <- remote.requests.get
          catalog                                            = field(response.data, "catalog")
        } yield assertTrue(
          response.errors.isEmpty,
          catalog.exists {
            case ResponseListValue(ResponseObjectValue(product) :: Nil) =>
              product.collectFirst { case ("details", ResponseObjectValue(details)) =>
                details.contains("name" -> StringValue("Table"))
              }.contains(true)
            case _                                                      => false
          },
          sent == Vector(request.copy(extensions = None))
        )
      },
      test("reuses the cached operation across sequential and concurrent identical requests") {
        val query    = """{ catalog: products(ids: ["p1"]) { id details { name } } }"""
        val response = """{"data":{"catalog":[{"id":"p1","details":{"name":"Table"}}]}}"""
        val config   = RemoteGraphQLConfig.default.withExecution(_.withInFlightQueryDeduplication(false))
        for {
          remote   <- stub(response)
          runtime  <- Gateway.compose(Subgraph.graphql("products", remote.endpoint, productsSchema, config)).interpreter
          first    <- runtime.execute(query)
          repeated <- ZIO.foreachPar(List.fill(16)(query))(runtime.execute(_))
          sent     <- remote.requests.get
        } yield assertTrue(
          first.errors.isEmpty,
          field(first.data, "catalog").contains(
            ResponseListValue(
              List(
                ResponseObjectValue(
                  List(
                    "id"      -> StringValue("p1"),
                    "details" -> ResponseObjectValue(List("name" -> StringValue("Table")))
                  )
                )
              )
            )
          ),
          repeated.forall(_ == first),
          sent.size == 17,
          sent.forall(_ == sent.head)
        )
      },
      test("binds fresh variables on every execution of a cached operation") {
        val query =
          """query Products($ids: [ID!]!) { catalog: products(ids: $ids) { id details { name } } }"""

        def clientRequest(id: String): GraphQLRequest =
          GraphQLRequest(
            query = Some(query),
            operationName = Some("Products"),
            variables = Some(Map("ids" -> ListValue(List(StringValue(id)))))
          )

        def product(id: String, name: String): ResponseValue =
          ResponseListValue(
            List(
              ResponseObjectValue(
                List(
                  "id"      -> StringValue(id),
                  "details" -> ResponseObjectValue(List("name" -> StringValue(name)))
                )
              )
            )
          )

        def requestedIds(request: GraphQLRequest): String =
          request.variables.getOrElse(Map.empty).get("ids").fold("")(_.toInputString)

        for {
          remote    <- stubByRequest { request =>
                         if (requestedIds(request).contains("p2"))
                           """{"data":{"catalog":[{"id":"p2","details":{"name":"Desk"}}]}}"""
                         else """{"data":{"catalog":[{"id":"p1","details":{"name":"Table"}}]}}"""
                       }
          runtime   <- remoteGateway(remote.endpoint).interpreter
          firstRun  <- runtime.executeRequest(clientRequest("p1"))
          secondRun <- runtime.executeRequest(clientRequest("p2"))
          sent      <- remote.requests.get
        } yield assertTrue(
          firstRun.errors.isEmpty,
          secondRun.errors.isEmpty,
          field(firstRun.data, "catalog").contains(product("p1", "Table")),
          field(secondRun.data, "catalog").contains(product("p2", "Desk")),
          sent.size == 2,
          requestedIds(sent(0)).contains("p1"),
          requestedIds(sent(1)).contains("p2")
        )
      },
      test("redacts remote errors while preserving aliases, list paths, and null completion") {
        val partialSchema   = "type Query { products: [Product] } type Product { name: String! }"
        val partialResponse =
          """{"data":{"catalog":[{"label":null},{"label":"Desk"}]},"errors":[{"message":"database password: secret","path":["catalog",0,"label"],"locations":[{"line":1,"column":2}],"extensions":{"code":"PRODUCT_DOWN","debug":"password=secret"}}]}"""

        for {
          remote   <- stub(partialResponse)
          runtime  <- remoteGateway(remote.endpoint, partialSchema).interpreter
          response <- runtime.execute("{ catalog: products { label: name } }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "catalog").contains(
            ResponseListValue(List(NullValue, ResponseObjectValue(List("label" -> StringValue("Desk")))))
          ),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(
            List(PathValue.Key("catalog"), PathValue.Index(0), PathValue.Key("label"))
          ),
          errors.forall(_.locationInfo.isEmpty),
          errors.flatMap(_.extensions).map(_.fields) == List(List("code" -> StringValue("PRODUCT_DOWN")))
        )
      },
      test("enables remote error messages globally while retaining only code extensions") {
        val globalResponse =
          """{"data":{"first":null},"errors":[{"message":"global detail","path":["first"],"extensions":{"code":"FIRST_DOWN","reason":"maintenance","secret":"hidden"}}]}"""
        val secondResponse =
          """{"data":{"second":null},"errors":[{"message":"second detail","path":["second"],"extensions":{"code":"SECOND_DOWN","reason":"private"}}]}"""

        for {
          first    <- stub(globalResponse)
          second   <- stub(secondResponse)
          runtime  <- Gateway
                        .compose(
                          Subgraph.graphql("first", first.endpoint, "type Query { first: String }"),
                          Subgraph.graphql(
                            "second",
                            second.endpoint,
                            "type Query { second: String }"
                          )
                        )
                        .withConfig(_.withRemoteErrorMessages(true))
                        .interpreter
          response <- runtime.execute("{ first second }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          errors.map(_.msg) == List("global detail", "second detail"),
          errors.map(_.path) == List(List(PathValue.Key("first")), List(PathValue.Key("second"))),
          errors.headOption
            .flatMap(_.extensions)
            .exists(
              _.fields == List("code" -> StringValue("FIRST_DOWN"))
            ),
          errors
            .drop(1)
            .headOption
            .flatMap(_.extensions)
            .exists(
              _.fields == List("code" -> StringValue("SECOND_DOWN"))
            )
        )
      },
      test("accepts a remote GraphQL errors-only response") {
        for {
          remote   <- stub(errorsResponse)
          runtime  <- remoteGateway(remote.endpoint).interpreter
          response <- runtime.execute("{ products(ids: [\"p1\"]) { id } }")
        } yield assertTrue(
          response.data == NullValue,
          response.errors.map(_.msg) == List("Remote GraphQL request failed."),
          executionErrors(response.errors).map(_.path) == List(
            List(PathValue.Key("products"))
          )
        )
      },
      test("turns an invalid remote response into a safe gateway error") {
        for {
          remote   <- stub(invalidResponse)
          runtime  <- Gateway
                        .compose(Subgraph.graphql("products", remote.endpoint, productsSchema))
                        .withConfig(_.withRemoteErrorMessages(true))
                        .interpreter
          response <- runtime.execute("{ products(ids: [\"p1\"]) { id } }")
        } yield assertTrue(
          response.data == NullValue,
          response.errors.map(_.msg) == List("Remote GraphQL request failed."),
          executionErrors(response.errors).map(_.path) == List(
            List(PathValue.Key("products"))
          )
        )
      },
      test("rejects an empty remote errors array without data") {
        for {
          remote   <- stub("""{"errors":[]}""")
          runtime  <- remoteGateway(remote.endpoint).interpreter
          response <- runtime.execute("{ products(ids: [\"p1\"]) { id } }")
        } yield assertTrue(
          response.data == NullValue,
          response.errors.map(_.msg) == List("Remote GraphQL request failed."),
          executionErrors(response.errors).map(_.path) == List(
            List(PathValue.Key("products"))
          )
        )
      },
      test("finalizes a successful single-source response") {
        val singleSchema = "type Query { product: Product } type Product { name: String! }"
        val responseBody =
          """{"data":{"product":{"name":null}},"errors":[{"message":"internal source detail","path":["product",null,"name"],"locations":[{"line":1,"column":2}]}]}"""

        for {
          remote   <- stub(responseBody)
          runtime  <- Gateway
                        .compose(Subgraph.graphql("products", remote.endpoint, singleSchema))
                        .withConfig(_.withRemoteErrorMessages(true))
                        .interpreter
          response <- runtime.execute("{ product { name } }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "product").contains(NullValue),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("product"))),
          errors.forall(_.locationInfo.isEmpty)
        )
      },
      test("completes a malformed nullable built-in scalar to null") {
        for {
          remote   <- stub("""{"data":{"value":{}}}""")
          runtime  <- remoteGateway(remote.endpoint, "type Query { value: String }", "source").interpreter
          response <- runtime.execute("{ value }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "value").contains(NullValue),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("value")))
        )
      },
      test("reports absent response fields without confusing explicit nulls") {
        val sourceSchema =
          "type Query { absent: String explicit: String nested: Nested } type Nested { present: String absent: String }"

        for {
          remote   <- stub("""{"data":{"explicit":null,"nested":{"present":"ok"}}}""")
          runtime  <- remoteGateway(remote.endpoint, sourceSchema, "source").interpreter
          response <- runtime.execute("{ absent explicit nested { present absent } }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          response.data == ResponseObjectValue(
            List(
              "absent"   -> NullValue,
              "explicit" -> NullValue,
              "nested"   -> ResponseObjectValue(List("present" -> StringValue("ok"), "absent" -> NullValue))
            )
          ),
          errors.map(_.msg) == List("Remote GraphQL request failed.", "Remote GraphQL request failed."),
          errors.map(_.path) == List(
            List(PathValue.Key("absent")),
            List(PathValue.Key("nested"), PathValue.Key("absent"))
          )
        )
      },
      test("rejects out-of-range Int values returned by a source") {
        for {
          remote   <- stub("""{"data":{"value":2147483648}}""")
          runtime  <- remoteGateway(remote.endpoint, "type Query { value: Int }", "source").interpreter
          response <- runtime.execute("{ value }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "value").contains(NullValue),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("value"))),
          !field(response.data, "value").contains(IntValue(2147483648L))
        )
      },
      test("bubbles a malformed non-null built-in scalar") {
        for {
          remote   <- stub("""{"data":{"value":{}}}""")
          runtime  <- remoteGateway(remote.endpoint, "type Query { value: String! }", "source").interpreter
          response <- runtime.execute("{ value }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          response.data == NullValue,
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("value")))
        )
      },
      test("completes a malformed nullable list to null") {
        val listSchema   = "type Query { reviews: [String!] }"
        val responseBody = """{"data":{"reviews":"invalid"}}"""

        for {
          remote   <- stub(responseBody)
          runtime  <- remoteGateway(remote.endpoint, listSchema, "reviews").interpreter
          response <- runtime.execute("{ reviews }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "reviews").contains(NullValue),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("reviews")))
        )
      },
      test("bubbles a malformed non-null list") {
        val listSchema   = "type Query { reviews: [String!]! }"
        val responseBody = """{"data":{"reviews":"invalid"}}"""

        for {
          remote   <- stub(responseBody)
          runtime  <- remoteGateway(remote.endpoint, listSchema, "reviews").interpreter
          response <- runtime.execute("{ reviews }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          response.data == NullValue,
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("reviews")))
        )
      },
      test("completes a malformed nullable object to null") {
        val objectSchema = "type Query { product: Product } type Product { name: String! }"

        for {
          remote   <- stub("""{"data":{"product":"invalid"}}""")
          runtime  <- remoteGateway(remote.endpoint, objectSchema).interpreter
          response <- runtime.execute("{ product { name } }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "product").contains(NullValue),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("product")))
        )
      },
      test("bubbles a malformed non-null object") {
        val objectSchema = "type Query { product: Product! } type Product { name: String! }"

        for {
          remote   <- stub("""{"data":{"product":[]}}""")
          runtime  <- remoteGateway(remote.endpoint, objectSchema).interpreter
          response <- runtime.execute("{ product { name } }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          response.data == NullValue,
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("product")))
        )
      },
      test("attaches a single-source failure to every affected nullable root") {
        val nullableRoots = "type Query { first: String second: String }"

        for {
          remote   <- stub(invalidResponse)
          runtime  <- remoteGateway(remote.endpoint, nullableRoots, "source").interpreter
          response <- runtime.execute("{ first second }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          response.data == ResponseObjectValue(List("first" -> NullValue, "second" -> NullValue)),
          errors.map(_.msg) == List("Remote GraphQL request failed.", "Remote GraphQL request failed."),
          errors.map(_.path) == List(
            List(PathValue.Key("first")),
            List(PathValue.Key("second"))
          )
        )
      },
      test("emits one fallback error per field for multiple path-less remote errors") {
        val nullableRoots = "type Query { first: String second: String }"
        val responseBody  =
          """{"data":{"first":null,"second":null},"errors":[{"message":"first failure"},{"message":"second failure"}]}"""

        for {
          remote   <- stub(responseBody)
          runtime  <- remoteGateway(remote.endpoint, nullableRoots, "source").interpreter
          response <- runtime.execute("{ first second }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          errors.map(_.msg) == List("Remote GraphQL request failed.", "Remote GraphQL request failed."),
          errors.map(_.path) == List(
            List(PathValue.Key("first")),
            List(PathValue.Key("second"))
          )
        )
      }
    ),
    test("merges duplicate fields last-wins on both sides of the wide-object threshold") {
      def value(size: Int): ResponseObjectValue =
        ResponseObjectValue(
          ("duplicate"   -> StringValue("first")) ::
            List.tabulate(size - 2)(index => s"field$index" -> StringValue(index.toString)) :::
            ("duplicate" -> StringValue("last")) :: Nil
        )

      val patch  = ResponseObjectValue(("duplicate" -> StringValue("merged")) :: Nil)
      val narrow = ResponseMerge.mergeObject(value(15), patch)
      val wide   = ResponseMerge.mergeObject(value(16), patch)

      def duplicateValues(value: ResponseValue): List[StringValue] =
        value match {
          case ResponseObjectValue(fields) => fields.collect { case ("duplicate", nested: StringValue) => nested }
          case _                           => Nil
        }

      assertTrue(
        duplicateValues(narrow) == List(StringValue("first"), StringValue("merged")),
        duplicateValues(wide) == List(StringValue("first"), StringValue("merged"))
      )
    },
    suite("schema and validation")(
      test("builds SDL and parsed documents through the same validated schema path") {
        for {
          remote   <- stub(dataResponse)
          document <- ZIO.fromEither(Parser.parseQuery(productsSchema))
          extended  = productsSchema + "\nextend type Query { version: String }"
          fromSdl  <- Gateway.compose(Subgraph.graphql("sdl", remote.endpoint, productsSchema)).interpreter.exit
          fromDoc  <- Gateway.compose(Subgraph.graphql("document", remote.endpoint, document)).interpreter.exit
          fromExt  <-
            remoteGateway(remote.endpoint, extended, "extended").interpreter.flatMap(_.check("{ version }")).exit
          invalid  <- remoteGateway(remote.endpoint, "type Query { broken: Missing }", "invalid").interpreter.exit
        } yield assertTrue(fromSdl.isSuccess, fromDoc.isSuccess, fromExt.isSuccess, invalid.isFailure)
      },
      test("rejects invalid client operations before contacting the remote graph") {
        for {
          remote   <- stub(dataResponse)
          runtime  <- remoteGateway(remote.endpoint).interpreter
          response <- runtime.execute("{ missing }")
          sent     <- remote.requests.get
        } yield assertTrue(response.errors.nonEmpty, sent.isEmpty)
      }
    ),
    suite("local introspection")(
      test("executes introspection locally without calling the remote graph") {
        for {
          remote       <- stub(dataResponse)
          runtime      <- remoteGateway(remote.endpoint).interpreter
          response     <- runtime.execute(
                            """{
                          |  product: __type(name: "Product") {
                          |    visible: fields { name }
                          |    all: fields(includeDeprecated: true) { name }
                          |  }
                          |  details: __type(name: "Details") {
                          |    visible: fields { name }
                          |    all: fields(includeDeprecated: true) { name isDeprecated deprecationReason }
                          |  }
                          |  state: __type(name: "State") {
                          |    visible: enumValues { name }
                          |    all: enumValues(includeDeprecated: true) { name isDeprecated deprecationReason }
                          |  }
                          |  scalar: __type(name: "URL") { specifiedByURL }
                          |}""".stripMargin
                          )
          sent         <- remote.requests.get
          product       = field(response.data, "product")
          visible       = introspectedNameStrings(product.flatMap(field(_, "visible")))
          all           = introspectedNameStrings(product.flatMap(field(_, "all")))
          details       = field(response.data, "details")
          detailVisible = introspectedNameStrings(details.flatMap(field(_, "visible")))
          detailAll     = details.flatMap(field(_, "all")).collect { case ResponseListValue(values) => values }
          state         = field(response.data, "state")
          stateVisible  = introspectedNameStrings(state.flatMap(field(_, "visible")))
          stateAll      = state.flatMap(field(_, "all")).collect { case ResponseListValue(values) => values }
          url           = field(response.data, "scalar").flatMap(field(_, "specifiedByURL"))
        } yield assertTrue(
          response.errors.isEmpty,
          visible.exists(!_.contains("legacyName")),
          all.exists(_.contains("legacyName")),
          detailVisible.exists(!_.contains("legacyLabel")),
          detailAll.exists(
            _.exists(value =>
              field(value, "name").contains(StringValue("legacyLabel")) &&
                field(value, "isDeprecated").contains(BooleanValue(true)) &&
                field(value, "deprecationReason").contains(StringValue("No longer supported"))
            )
          ),
          stateVisible.exists(!_.contains("LEGACY")),
          stateAll.exists(
            _.exists(value =>
              field(value, "name").contains(StringValue("LEGACY")) &&
                field(value, "isDeprecated").contains(BooleanValue(true)) &&
                field(value, "deprecationReason").contains(StringValue("Use ACTIVE"))
            )
          ),
          url.contains(StringValue("https://example.com/url")),
          sent.isEmpty
        )
      },
      test("reports an empty interface list on every composed object type, including the roots") {
        // graphql-js `buildClientSchema` — the client GraphiQL builds its schema with — rejects an
        // object type whose introspected `interfaces` is null, so the composed roots have to answer
        // with the empty list rather than nothing at all.
        val rootsSchema =
          """
            |schema { query: Query, mutation: Mutation, subscription: Subscription }
            |type Query { product: Product! }
            |type Mutation { addProduct(name: String!): Product! }
            |type Subscription { productAdded: Product! }
            |interface Node { id: ID! }
            |type Product implements Node { id: ID! name: String! }
            |""".stripMargin

        for {
          remote   <- stub(dataResponse)
          runtime  <- remoteGateway(remote.endpoint, rootsSchema).interpreter
          response <- runtime.execute("{ __schema { types { kind name interfaces { name } } } }")
          types     = listValues(field(response.data, "__schema").flatMap(field(_, "types")))
          objects   = types.filter(value => field(value, "kind").contains(EnumValue("OBJECT")))
          nulled    = objects
                        .filter(value => field(value, "interfaces").forall(_ == NullValue))
                        .flatMap(field(_, "name"))
                        .collect { case StringValue(name) => name }
          named     = objects.flatMap(field(_, "name")).collect { case StringValue(name) => name }
          product   = objects
                        .find(value => field(value, "name").contains(StringValue("Product")))
                        .flatMap(field(_, "interfaces"))
        } yield assertTrue(
          response.errors.isEmpty,
          // Grounds the scan: an empty selection would otherwise make `nulled` vacuously empty.
          named.contains("Query") && named.contains("Mutation") && named.contains("Subscription"),
          nulled.isEmpty,
          product.contains(ResponseListValue(List(ResponseObjectValue(List("name" -> StringValue("Node"))))))
        )
      },
      test("executes named and inline fragment-only introspection locally") {
        val named  =
          """
            |query { ...IntrospectionFields }
            |fragment IntrospectionFields on Query { __schema { queryType { name } } }
            |""".stripMargin
        val inline = "query { ... on Query { __type(name: \"Product\") { name } } }"

        for {
          remote         <- stub(dataResponse)
          runtime        <- remoteGateway(remote.endpoint).interpreter
          namedResponse  <- runtime.execute(named)
          inlineResponse <- runtime.execute(inline)
          sent           <- remote.requests.get
        } yield assertTrue(
          namedResponse.errors.isEmpty,
          field(namedResponse.data, "__schema")
            .flatMap(field(_, "queryType"))
            .flatMap(field(_, "name"))
            .contains(StringValue("Query")),
          inlineResponse.errors.isEmpty,
          field(inlineResponse.data, "__type").flatMap(field(_, "name")).contains(StringValue("Product")),
          sent.isEmpty
        )
      },
      test("keeps single-subgraph meta fields local for a custom remote root") {
        val customRootSchema =
          "schema { query: RootQuery } type RootQuery { product(id: ID!): Product } type Product { id: ID! }"
        val query            =
          """
            |query Dashboard($id: ID!) {
            |  featured: product(id: $id) { id }
            |  __typename
            |  __schema { queryType { name } }
            |  __type(name: "Product") { name }
            |}
            |""".stripMargin

        for {
          products <- stub("""{"data":{"featured":{"id":"p1"}}}""")
          runtime  <- remoteGateway(products.endpoint, customRootSchema).interpreter
          request   = GraphQLRequest(
                        query = Some(query),
                        operationName = Some("Dashboard"),
                        variables = Some(Map("id" -> StringValue("p1")))
                      )
          response <- runtime.executeRequest(request)
          sent     <- products.requests.get
          valid    <- ZIO.foreach(sent)(validateRequest(customRootSchema, _).exit)
          names     = fieldNames(response.data)
        } yield assertTrue(
          response.errors.isEmpty,
          names == List("featured", "__typename", "__schema", "__type"),
          field(response.data, "__typename").contains(StringValue("Query")),
          field(response.data, "__schema")
            .flatMap(field(_, "queryType"))
            .flatMap(field(_, "name"))
            .contains(StringValue("Query")),
          field(response.data, "__type").flatMap(field(_, "name")).contains(StringValue("Product")),
          sent.size == 1,
          sent.head != request,
          sent.head.variables.isEmpty,
          sent.head.query.exists(query =>
            query.contains("featured:product(id:\"p1\")") &&
              !query.contains("__schema") && !query.contains("__type") && !query.contains("__typename")
          ),
          valid.forall(_.isSuccess)
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

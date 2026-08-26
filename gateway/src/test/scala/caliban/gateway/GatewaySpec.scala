package caliban.gateway

import caliban.InputValue.{ ListValue, ObjectValue => InputObjectValue }
import caliban.ResponseValue.{ ListValue => ResponseListValue, ObjectValue => ResponseObjectValue }
import caliban.Value.{ BooleanValue, EnumValue, IntValue, NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.GatewayRuntimeImpl
import caliban.parsing.Parser
import caliban.schema.{ GenericSchema, Schema }
import caliban.wrappers.ApolloPersistedQueries
import caliban.wrappers.Wrappers.maxDepth
import caliban._
import zio._
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

  private val schema =
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

  private def runtime(stub: Stub): ZIO[Scope, GatewayBuildError, GatewayRuntime[Any]] =
    Gateway.compose(Subgraph.graphql("products", stub.endpoint, schema)).build

  def spec = suite("GatewaySpec")(
    suite("local subgraphs")(
      test("executes local roots with their accumulated environments") {
        val description: Gateway[Greeting with Audience] = Gateway.compose(
          Subgraph.local("greeting", LocalSchemas.GreetingApi.api),
          Subgraph.local("audience", LocalSchemas.AudienceApi.api)
        )
        val environment                                  = ZLayer.succeed(new Greeting {
          def value: UIO[String] = ZIO.succeed("hello")
        }) ++ ZLayer.succeed(new Audience {
          def value: UIO[String] = ZIO.succeed("world")
        })

        (for {
          gateway  <- description.build
          response <- gateway.execute("{ greeting audience }")
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "greeting").contains(StringValue("hello")),
          field(response.data, "audience").contains(StringValue("world"))
        )).provideSome[Scope](environment)
      },
      test("shares one cached plan and its caches across concurrent constant executions") {
        val query = """{ echo(value: "fixed") status }"""

        for {
          gateway   <- Gateway
                         .compose(
                           Subgraph.local("echo", LocalSchemas.EchoApi.api),
                           Subgraph.local("status", LocalSchemas.EnumApi.api)
                         )
                         .build
          primed    <- gateway.execute(query)
          responses <- ZIO.foreachPar((1 to 24).toList)(_ => gateway.execute(query))
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
          gateway   <- Gateway
                         .compose(
                           Subgraph.local("echo", LocalSchemas.EchoApi.api),
                           Subgraph.local("status", LocalSchemas.EnumApi.api)
                         )
                         .build
          primed    <- gateway.executeRequest(echoRequest("primed"))
          responses <- ZIO.foreachPar((1 to 24).toList)(i => gateway.executeRequest(echoRequest(s"value-$i")))
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
          gateway  <- Gateway.compose(Subgraph.local("status", LocalSchemas.EnumApi.api)).build
          response <- gateway.execute("{ status }")
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
          gateway     <- Gateway.compose(Subgraph.local("local", api)).build
          request      = GraphQLRequest(query = Some("{ context failure }"))
          direct      <- context.locally("request-context")(interpreter.executeRequest(request))
          response    <- context.locally("request-context")(gateway.executeRequest(request))
        } yield assertTrue(
          field(response.data, "context").contains(StringValue("request-context")),
          field(response.data, "failure").contains(NullValue),
          response.errors.collect { case error: CalibanError.ExecutionError => error.msg } == List("local failure"),
          response.errors == direct.errors
        )
      },
      test("isolates local request-error classification from the gateway request") {
        for {
          gateway    <- Gateway
                          .compose(Subgraph.local("local", localGraph(ZIO.succeed("ok")) @@ maxDepth(0)))
                          .build
          classified <- GraphQLResponseContext.capture(gateway.execute("{ value }"))
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
          gateway  <- Gateway
                        .compose(
                          Subgraph.local(
                            "local",
                            localGraph(ZIO.succeed("ok")) @@ ApolloPersistedQueries.wrapper
                          )
                        )
                        .build
          response <- gateway.executeRequest(request)
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
          gateway <- Gateway.compose(Subgraph.local("local", api)).build
          fiber   <- gateway.execute("{ blocked }").fork
          _       <- started.await
          exit    <- fiber.interrupt
        } yield assertTrue(exit.isInterrupted)
      }
    ),
    suite("single-source execution")(
      test("executes one pinned remote graph end to end through GatewayRuntime") {
        for {
          remote                                            <- stub(dataResponse)
          gateway                                           <- runtime(remote)
          interpreter: GraphQLInterpreter[Any, CalibanError] = gateway
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
          requests                                          <- remote.requests.get
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
          requests == Vector(request.copy(extensions = None))
        )
      },
      test("reuses the cached operation across sequential and concurrent identical requests") {
        val query    = """{ catalog: products(ids: ["p1"]) { id details { name } } }"""
        val response = """{"data":{"catalog":[{"id":"p1","details":{"name":"Table"}}]}}"""
        for {
          remote   <- stub(response)
          gateway  <- runtime(remote)
          first    <- gateway.execute(query)
          repeated <- ZIO.foreachPar(List.fill(16)(query))(gateway.execute(_))
          requests <- remote.requests.get
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
          requests.size == 17,
          requests.forall(_ == requests.head)
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
          gateway   <- runtime(remote)
          firstRun  <- gateway.executeRequest(clientRequest("p1"))
          secondRun <- gateway.executeRequest(clientRequest("p2"))
          requests  <- remote.requests.get
        } yield assertTrue(
          firstRun.errors.isEmpty,
          secondRun.errors.isEmpty,
          field(firstRun.data, "catalog").contains(product("p1", "Table")),
          field(secondRun.data, "catalog").contains(product("p2", "Desk")),
          requests.size == 2,
          requestedIds(requests(0)).contains("p1"),
          requestedIds(requests(1)).contains("p2")
        )
      },
      test("redacts remote errors while preserving aliases, list paths, and null completion") {
        val partialSchema   = "type Query { products: [Product] } type Product { name: String! }"
        val partialResponse =
          """{"data":{"catalog":[{"label":null},{"label":"Desk"}]},"errors":[{"message":"database password: secret","path":["catalog",0,"label"],"locations":[{"line":1,"column":2}],"extensions":{"code":"PRODUCT_DOWN","debug":"password=secret"}}]}"""

        for {
          remote   <- stub(partialResponse)
          gateway  <- Gateway.compose(Subgraph.graphql("products", remote.endpoint, partialSchema)).build
          response <- gateway.execute("{ catalog: products { label: name } }")
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
      test("configures remote error disclosure globally with a per-subgraph override") {
        val globalResponse   =
          """{"data":{"first":null},"errors":[{"message":"global detail","path":["first"],"extensions":{"code":"FIRST_DOWN","reason":"maintenance","secret":"hidden"}}]}"""
        val overrideResponse =
          """{"data":{"second":null},"errors":[{"message":"override detail","path":["second"],"extensions":{"code":"SECOND_DOWN","reason":"private"}}]}"""
        val overrideConfig   = RemoteGraphQLConfig.default.withErrorDisclosure(_.withMessages(false))

        for {
          first    <- stub(globalResponse)
          second   <- stub(overrideResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.graphql("first", first.endpoint, "type Query { first: String }"),
                          Subgraph.graphql(
                            "second",
                            second.endpoint,
                            "type Query { second: String }",
                            overrideConfig
                          )
                        )
                        .withConfig(
                          _.withRemoteErrorDisclosure(
                            _.withMessages(true).withAdditionalExtensionKeys("reason")
                          )
                        )
                        .build
          response <- gateway.execute("{ first second }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          errors.map(_.msg) == List("global detail", "Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("first")), List(PathValue.Key("second"))),
          errors.headOption
            .flatMap(_.extensions)
            .exists(
              _.fields == List("code" -> StringValue("FIRST_DOWN"), "reason" -> StringValue("maintenance"))
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
          gateway  <- runtime(remote)
          response <- gateway.execute("{ products(ids: [\"p1\"]) { id } }")
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
          gateway  <- Gateway
                        .compose(Subgraph.graphql("products", remote.endpoint, schema))
                        .withConfig(_.withRemoteErrorDisclosure(_.withMessages(true)))
                        .build
          response <- gateway.execute("{ products(ids: [\"p1\"]) { id } }")
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
          gateway  <- Gateway
                        .compose(Subgraph.graphql("products", remote.endpoint, singleSchema))
                        .withConfig(_.withRemoteErrorDisclosure(_.withMessages(true)))
                        .build
          response <- gateway.execute("{ product { name } }")
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
          gateway  <- Gateway.compose(Subgraph.graphql("source", remote.endpoint, "type Query { value: String }")).build
          response <- gateway.execute("{ value }")
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
          gateway  <- Gateway.compose(Subgraph.graphql("source", remote.endpoint, sourceSchema)).build
          response <- gateway.execute("{ absent explicit nested { present absent } }")
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
          gateway  <- Gateway.compose(Subgraph.graphql("source", remote.endpoint, "type Query { value: Int }")).build
          response <- gateway.execute("{ value }")
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
          gateway  <- Gateway.compose(Subgraph.graphql("source", remote.endpoint, "type Query { value: String! }")).build
          response <- gateway.execute("{ value }")
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
          gateway  <- Gateway.compose(Subgraph.graphql("reviews", remote.endpoint, listSchema)).build
          response <- gateway.execute("{ reviews }")
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
          gateway  <- Gateway.compose(Subgraph.graphql("reviews", remote.endpoint, listSchema)).build
          response <- gateway.execute("{ reviews }")
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
          gateway  <- Gateway.compose(Subgraph.graphql("products", remote.endpoint, objectSchema)).build
          response <- gateway.execute("{ product { name } }")
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
          gateway  <- Gateway.compose(Subgraph.graphql("products", remote.endpoint, objectSchema)).build
          response <- gateway.execute("{ product { name } }")
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
          gateway  <- Gateway.compose(Subgraph.graphql("source", remote.endpoint, nullableRoots)).build
          response <- gateway.execute("{ first second }")
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
          gateway  <- Gateway.compose(Subgraph.graphql("source", remote.endpoint, nullableRoots)).build
          response <- gateway.execute("{ first second }")
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
      val narrow = GatewayRuntimeImpl.mergeObject(value(15), patch)
      val wide   = GatewayRuntimeImpl.mergeObject(value(16), patch)

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
          document <- ZIO.fromEither(Parser.parseQuery(schema))
          extended  = schema + "\nextend type Query { version: String }"
          fromSdl  <- Gateway.compose(Subgraph.graphql("sdl", remote.endpoint, schema)).build.exit
          fromDoc  <- Gateway.compose(Subgraph.graphql("document", remote.endpoint, document)).build.exit
          fromExt  <- Gateway
                        .compose(Subgraph.graphql("extended", remote.endpoint, extended))
                        .build
                        .flatMap {
                          _.check("{ version }")
                        }
                        .exit
          invalid  <- Gateway
                        .compose(Subgraph.graphql("invalid", remote.endpoint, "type Query { broken: Missing }"))
                        .build
                        .exit
        } yield assertTrue(fromSdl.isSuccess, fromDoc.isSuccess, fromExt.isSuccess, invalid.isFailure)
      },
      test("rejects invalid client operations before contacting the remote graph") {
        for {
          remote   <- stub(dataResponse)
          gateway  <- runtime(remote)
          response <- gateway.execute("{ missing }")
          requests <- remote.requests.get
        } yield assertTrue(response.errors.nonEmpty, requests.isEmpty)
      }
    ),
    suite("local introspection")(
      test("executes introspection locally without calling the remote graph") {
        for {
          remote       <- stub(dataResponse)
          gateway      <- runtime(remote)
          response     <- gateway.execute(
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
          requests     <- remote.requests.get
          product       = field(response.data, "product")
          visible       = product.flatMap(field(_, "visible")).collect { case ResponseListValue(values) =>
                            values.flatMap(field(_, "name")).collect { case StringValue(value) => value }
                          }
          all           = product.flatMap(field(_, "all")).collect { case ResponseListValue(values) =>
                            values.flatMap(field(_, "name")).collect { case StringValue(value) => value }
                          }
          details       = field(response.data, "details")
          detailVisible = details.flatMap(field(_, "visible")).collect { case ResponseListValue(values) =>
                            values.flatMap(field(_, "name")).collect { case StringValue(value) => value }
                          }
          detailAll     = details.flatMap(field(_, "all")).collect { case ResponseListValue(values) => values }
          state         = field(response.data, "state")
          stateVisible  = state.flatMap(field(_, "visible")).collect { case ResponseListValue(values) =>
                            values.flatMap(field(_, "name")).collect { case StringValue(value) => value }
                          }
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
          requests.isEmpty
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
          gateway        <- runtime(remote)
          namedResponse  <- gateway.execute(named)
          inlineResponse <- gateway.execute(inline)
          requests       <- remote.requests.get
        } yield assertTrue(
          namedResponse.errors.isEmpty,
          field(namedResponse.data, "__schema")
            .flatMap(field(_, "queryType"))
            .flatMap(field(_, "name"))
            .contains(StringValue("Query")),
          inlineResponse.errors.isEmpty,
          field(inlineResponse.data, "__type").flatMap(field(_, "name")).contains(StringValue("Product")),
          requests.isEmpty
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
          gateway  <- Gateway.compose(Subgraph.graphql("products", products.endpoint, customRootSchema)).build
          request   = GraphQLRequest(
                        query = Some(query),
                        operationName = Some("Dashboard"),
                        variables = Some(Map("id" -> StringValue("p1")))
                      )
          response <- gateway.executeRequest(request)
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

package caliban.gateway

import caliban.InputValue.ListValue
import caliban.ResponseValue.{ ListValue => ResponseListValue, ObjectValue => ResponseObjectValue }
import caliban.Value.{ BooleanValue, NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.parsing.Parser
import caliban.schema.{ GenericSchema, Schema }
import caliban.{ graphQL, CalibanError, GraphQLInterpreter, GraphQLRequest, PathValue, RootResolver }
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
      |}
      |
      |type Review {
      |  body: String!
      |}
      |
      |scalar URL @specifiedBy(url: "https://example.com/url")
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

  private val partialResponse =
    """{"data":{"catalog":null},"errors":[{"message":"catalog unavailable","path":["catalog"]}]}"""

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
          requests == Vector(request)
        )
      },
      test("accepts partial data plus remote GraphQL errors") {
        for {
          remote   <- stub(partialResponse)
          gateway  <- runtime(remote)
          response <- gateway.execute(
                        nestedQuery,
                        Some("Products"),
                        Map("ids" -> ListValue(List(StringValue("p1"))), "includeReviews" -> BooleanValue(true))
                      )
        } yield assertTrue(
          response.data == NullValue,
          response.errors.map(_.msg) == List("catalog unavailable")
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
          response.errors.collect { case error: CalibanError.ExecutionError => error.path } == List(
            List(PathValue.Key("products"))
          )
        )
      },
      test("turns an invalid remote response into a safe gateway error") {
        for {
          remote   <- stub(invalidResponse)
          gateway  <- runtime(remote)
          response <- gateway.execute("{ products(ids: [\"p1\"]) { id } }")
        } yield assertTrue(
          response.data == NullValue,
          response.errors.map(_.msg) == List("Remote GraphQL request failed."),
          response.errors.collect { case error: CalibanError.ExecutionError => error.path } == List(
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
          gateway  <- Gateway.compose(Subgraph.graphql("products", remote.endpoint, singleSchema)).build
          response <- gateway.execute("{ product { name } }")
          errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
        } yield assertTrue(
          field(response.data, "product").contains(NullValue),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("product"))),
          errors.forall(_.locationInfo.isEmpty)
        )
      },
      test("completes a malformed nullable list to null") {
        val listSchema   = "type Query { reviews: [String!] }"
        val responseBody = """{"data":{"reviews":"invalid"}}"""

        for {
          remote   <- stub(responseBody)
          gateway  <- Gateway.compose(Subgraph.graphql("reviews", remote.endpoint, listSchema)).build
          response <- gateway.execute("{ reviews }")
          errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
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
          errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
        } yield assertTrue(
          response.data == NullValue,
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("reviews")))
        )
      },
      test("attaches a single-source failure to every affected nullable root") {
        val nullableRoots = "type Query { first: String second: String }"

        for {
          remote   <- stub(invalidResponse)
          gateway  <- Gateway.compose(Subgraph.graphql("source", remote.endpoint, nullableRoots)).build
          response <- gateway.execute("{ first second }")
          errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
        } yield assertTrue(
          response.data == ResponseObjectValue(List("first" -> NullValue, "second" -> NullValue)),
          errors.map(_.msg) == List("Remote GraphQL request failed.", "Remote GraphQL request failed."),
          errors.map(_.path) == List(
            List(PathValue.Key("first")),
            List(PathValue.Key("second"))
          )
        )
      }
    ),
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
          remote   <- stub(dataResponse)
          gateway  <- runtime(remote)
          response <- gateway.execute(
                        """{
                          |  product: __type(name: "Product") {
                          |    visible: fields { name }
                          |    all: fields(includeDeprecated: true) { name }
                          |  }
                          |  scalar: __type(name: "URL") { specifiedByURL }
                          |}""".stripMargin
                      )
          requests <- remote.requests.get
          product   = field(response.data, "product")
          visible   = product.flatMap(field(_, "visible")).collect { case ResponseListValue(values) =>
                        values.flatMap(field(_, "name")).collect { case StringValue(value) => value }
                      }
          all       = product.flatMap(field(_, "all")).collect { case ResponseListValue(values) =>
                        values.flatMap(field(_, "name")).collect { case StringValue(value) => value }
                      }
          url       = field(response.data, "scalar").flatMap(field(_, "specifiedByURL"))
        } yield assertTrue(
          response.errors.isEmpty,
          visible.exists(!_.contains("legacyName")),
          all.exists(_.contains("legacyName")),
          url.contains(StringValue("https://example.com/url")),
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
          names     = response.data match {
                        case ResponseObjectValue(fields) => fields.map(_._1)
                        case _                           => Nil
                      }
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

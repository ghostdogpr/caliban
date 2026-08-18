package caliban.gateway

import caliban.ResponseValue.ListValue
import caliban.Value.{ FloatValue, IntValue, NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import sttp.model.Uri
import zio._
import zio.test._

object CompositionSpec extends ZIOSpecDefault {

  private val endpoint = Uri.unsafeParse("http://127.0.0.1:1/graphql")

  private def schema(body: String, imports: String): String =
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: [$imports]) { query: Query }
       |$authoredFederationDirectives
       |$body
       |""".stripMargin

  def spec = suite("CompositionSpec")(
    suite("ownership")(
      test("routes a shareable root field deterministically") {
        val valueSchema = schema("type Query { value: String @shareable }", "\"@shareable\"")

        for {
          alpha         <- stub("""{"data":{"value":"alpha"}}""")
          beta          <- stub("""{"data":{"value":"beta"}}""")
          gateway       <- Gateway
                             .compose(
                               Subgraph.federation("beta", beta.endpoint, valueSchema),
                               Subgraph.federation("alpha", alpha.endpoint, valueSchema)
                             )
                             .build
          response      <- gateway.execute("{ value }")
          alphaRequests <- alpha.requests.get
          betaRequests  <- beta.requests.get
        } yield assertTrue(
          field(response.data, "value").contains(StringValue("alpha")),
          alphaRequests.size == 1,
          betaRequests.isEmpty
        )
      },
      test("coalesces compatible partial results for a shareable object root") {
        val namesSchema = schema(
          "type Query { product: Product! @shareable } type Product { id: ID! @shareable name: String! }",
          "\"@shareable\""
        )
        val priceSchema = schema(
          "type Query { product: Product! @shareable } type Product { id: ID! @shareable price: Int! }",
          "\"@shareable\""
        )

        for {
          names    <- stub("""{"data":{"product":{"id":"p1","name":"Table"}}}""")
          prices   <- stub("""{"data":{"product":{"id":"p1","price":10}}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("names", names.endpoint, namesSchema),
                          Subgraph.federation("prices", prices.endpoint, priceSchema)
                        )
                        .build
          response <- gateway.execute("{ product { id name price } }")
          product   = field(response.data, "product")
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "id")).contains(StringValue("p1")),
          product.flatMap(field(_, "name")).contains(StringValue("Table")),
          product.flatMap(field(_, "price")).contains(IntValue(10))
        )
      },
      test("retains an entity transition below a multiply provided root") {
        val namesSchema   = schema(
          "type Query { product: Product! @shareable } type Product @key(fields: \"id\") { id: ID! name: String! details: Details! } type Details { value: String! }",
          "\"@key\", \"@shareable\""
        )
        val pricesSchema  = schema(
          "type Query { product: Product! @shareable } type Product @key(fields: \"id\") { id: ID! price: Int! }",
          "\"@key\", \"@shareable\""
        )
        val reviewsSchema =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
             |$authoredFederationDirectives
             |type Product @key(fields: "id") { id: ID! @external reviews: [Review!]! }
             |type Review { body: String! }
             |""".stripMargin

        for {
          names         <-
            stub(
              """{"data":{"product":{"name":"Table","details":{"__typename":"Details"},"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
            )
          prices        <- stub("""{"data":{"product":{"price":10}}}""")
          reviews       <-
            stub(
              """{"data":{"_entities":[{"reviews":[{"body":"Great"}],"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product"}]}}"""
            )
          gateway       <- Gateway
                             .compose(
                               Subgraph.federation("names", names.endpoint, namesSchema),
                               Subgraph.federation("prices", prices.endpoint, pricesSchema),
                               Subgraph.federation("reviews", reviews.endpoint, reviewsSchema)
                             )
                             .build
          response      <- gateway.execute("{ product { name price details { __typename } reviews { body } } }")
          reviewCalls   <- reviews.requests.get
          priceCalls    <- prices.requests.get
          product        = field(response.data, "product")
          responseReview = product.flatMap(field(_, "reviews"))
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "name")).contains(StringValue("Table")),
          product.flatMap(field(_, "price")).contains(IntValue(10)),
          product.flatMap(field(_, "details")).flatMap(field(_, "__typename")).contains(StringValue("Details")),
          responseReview.exists(_.toString.contains("Great")),
          reviewCalls.size == 1,
          priceCalls.forall(_.query.forall(query => !query.contains("details")))
        )
      },
      test("routes an overridden root field only to the overriding source") {
        val original  = schema("type Query { feed: String }", "\"@override\"")
        val replacing = schema("type Query { feed: String @override(from: \"products\") }", "\"@override\"")

        for {
          products        <- stub("""{"data":{"feed":"old"}}""")
          inventory       <- stub("""{"data":{"feed":"new"}}""")
          gateway         <- Gateway
                               .compose(
                                 Subgraph.federation("products", products.endpoint, original),
                                 Subgraph.federation("inventory", inventory.endpoint, replacing)
                               )
                               .build
          response        <- gateway.execute("{ feed }")
          productRequests <- products.requests.get
          inventoryCalls  <- inventory.requests.get
        } yield assertTrue(
          field(response.data, "feed").contains(StringValue("new")),
          productRequests.isEmpty,
          inventoryCalls.size == 1
        )
      },
      test("rejects fields shared without compatible shareability") {
        val valueSchema = schema("type Query { value: String }", "\"@shareable\"")
        val result      = Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, valueSchema),
            Subgraph.federation("beta", endpoint, valueSchema)
          )
          .build
          .exit

        result.map(exit =>
          assertTrue(exit.causeOption.flatMap(_.failureOption).exists(_.diagnostics.exists(_.contains("shareable"))))
        )
      },
      test("requires each Federation 2 provider to make a key field shareable") {
        val keyed = schema(
          "type Query { alpha: Product } type Product @key(fields: \"id\") { id: ID! }",
          "\"@key\""
        )
        val plain = schema("type Query { beta: Product } type Product { id: ID! }", "")

        Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, keyed),
            Subgraph.federation("beta", endpoint, plain)
          )
          .build
          .exit
          .map(exit =>
            assertTrue(exit.causeOption.flatMap(_.failureOption).exists(_.diagnostics.exists(_.contains("shareable"))))
          )
      },
      test("does not treat an unimported custom directive as Federation shareability") {
        val valueSchema = schema("type Query { value: String @shareable }", "")

        Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, valueSchema),
            Subgraph.federation("beta", endpoint, valueSchema)
          )
          .build
          .exit
          .map(exit =>
            assertTrue(exit.causeOption.flatMap(_.failureOption).exists(_.diagnostics.exists(_.contains("shareable"))))
          )
      },
      test("allows an override to name a source that is no longer present") {
        val base     = schema("type Query { value: String @shareable }", "\"@shareable\"")
        val migrated = schema(
          "type Query { value: String @shareable @override(from: \"removed\") }",
          "\"@shareable\", \"@override\""
        )

        Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, base),
            Subgraph.federation("beta", endpoint, migrated)
          )
          .build
          .exit
          .map(exit => assertTrue(exit.isSuccess))
      },
      test("allows an override of an external-only declaration") {
        val external   = schema(
          "type Query { alpha: Product } type Product { code: String @external }",
          "\"@external\""
        )
        val overriding = schema(
          "type Query { beta: Product } type Product { code: String @override(from: \"alpha\") }",
          "\"@override\""
        )

        Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, external),
            Subgraph.federation("beta", endpoint, overriding)
          )
          .build
          .exit
          .map(exit => assertTrue(exit.isSuccess))
      },
      test("does not expose a field declared only as external") {
        val externalOnly = schema(
          "type Query { product: Product } type Product { id: ID! code: String @external }",
          "\"@external\""
        )

        for {
          source       <- stub("""{"data":{"product":{"id":"p1"}}}""")
          gateway      <- Gateway.compose(Subgraph.federation("products", source.endpoint, externalOnly)).build
          response     <- gateway.execute("{ product { code } }")
          introspected <- gateway.execute("{ __type(name: \"Product\") { fields { name } } }")
          requests     <- source.requests.get
        } yield assertTrue(
          response.errors.nonEmpty,
          !introspected.toResponseValue.toString.contains("code"),
          requests.isEmpty
        )
      },
      test("treats Federation 1 extension key fields as resolvable") {
        val productsSchema =
          """
            |type Query { productInA: Product }
            |type Product @key(fields: "id") { id: ID! name: String pid: ID }
            |""".stripMargin
        val pricesSchema   =
          """
            |type Query { productInB: Product }
            |extend type Product @key(fields: "id name") @key(fields: "upc") {
            |  id: ID @external
            |  name: String @external
            |  upc: String @external
            |  price: Float!
            |}
            |""".stripMargin

        for {
          products <-
            stub(
              """{"data":{"productInA":{"id":"p1","pid":"p1-pid","name":"p1-name","_caliban_gateway_key":"p1","_caliban_gateway_key_2":"p1-name","_caliban_gateway_typename":"Product"}}}"""
            )
          prices   <- stub("""{"data":{"_entities":[{"price":12.3,"upc":"upc1"}]}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("a", products.endpoint, productsSchema),
                          Subgraph.federation("b", prices.endpoint, pricesSchema)
                        )
                        .build
          response <- gateway.execute("{ productInA { id pid price upc name } }")
          sent     <- prices.requests.get
          product   = field(response.data, "productInA")
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "id")).contains(StringValue("p1")),
          product.flatMap(field(_, "pid")).contains(StringValue("p1-pid")),
          product.flatMap(field(_, "name")).contains(StringValue("p1-name")),
          product.flatMap(field(_, "upc")).contains(StringValue("upc1")),
          product.flatMap(field(_, "price")).exists {
            case value: FloatValue => value.toBigDecimal == BigDecimal("12.3")
            case _                 => false
          },
          sent.size == 1,
          sent.headOption.flatMap(_.query).exists(query => query.contains("price") && query.contains("upc"))
        )
      },
      test("rejects incompatible external and overridden declarations") {
        val idOwner    = schema("type Query { alpha: Product } type Product { id: ID }", "\"@external\"")
        val idExternal = schema(
          "type Query { beta: Product } type Product { id: Int @external }",
          "\"@external\""
        )
        val feedOwner  = schema("type Query { feed: String }", "\"@override\"")
        val feedNext   = schema("type Query { feed: Int @override(from: \"feed-owner\") }", "\"@override\"")

        Gateway
          .compose(
            Subgraph.federation("id-owner", endpoint, idOwner),
            Subgraph.federation("id-external", endpoint, idExternal),
            Subgraph.federation("feed-owner", endpoint, feedOwner),
            Subgraph.federation("feed-next", endpoint, feedNext)
          )
          .build
          .exit
          .map { exit =>
            val diagnostics = exit.causeOption.flatMap(_.failureOption).map(_.diagnostics).getOrElse(Nil)
            assertTrue(
              diagnostics.exists(_.startsWith("[type Product.id]")),
              diagnostics.exists(message =>
                message.startsWith("[query.feed]") && message.contains("'feed-owner'") && message.contains(
                  "'feed-next'"
                )
              )
            )
          }
      },
      test("attributes competing overrides to their sources") {
        val original  = schema("type Query { value: String }", "\"@override\"")
        val replacing = schema(
          "type Query { value: String @override(from: \"alpha\") }",
          "\"@override\""
        )

        Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, original),
            Subgraph.federation("beta", endpoint, replacing),
            Subgraph.federation("gamma", endpoint, replacing)
          )
          .build
          .exit
          .map { exit =>
            val diagnostics = exit.causeOption.flatMap(_.failureOption).map(_.diagnostics).getOrElse(Nil)
            assertTrue(
              diagnostics.exists(message =>
                message.contains("@override") && message.contains("'beta'") && message.contains("'gamma'")
              )
            )
          }
      }
    ),
    suite("visibility")(
      test("removes inaccessible fields and enum values from the client schema") {
        val hiddenSchema = schema(
          """
            |type Query { product: Product! hiddenState: State secret: String @inaccessible }
            |type Product { id: ID! state: State! internal: String @inaccessible }
            |enum State { ACTIVE HIDDEN @inaccessible }
            |""".stripMargin,
          "\"@inaccessible\""
        )

        for {
          source       <- stub(
                            """{"data":{"product":{"id":"p1","state":"ACTIVE"}}}""",
                            """{"data":{"hiddenState":"HIDDEN"}}"""
                          )
          gateway      <- Gateway.compose(Subgraph.federation("products", source.endpoint, hiddenSchema)).build
          visible      <- gateway.execute("{ product { id state } }")
          hidden       <- gateway.execute("{ secret product { internal } }")
          introspected <- gateway.execute("{ __type(name: \"State\") { enumValues { name } } }")
          value        <- gateway.execute("{ hiddenState }")
        } yield assertTrue(
          visible.errors.isEmpty,
          hidden.data == NullValue,
          hidden.errors.nonEmpty,
          introspected.toResponseValue.toString.contains("ACTIVE"),
          !introspected.toResponseValue.toString.contains("HIDDEN"),
          field(value.data, "hiddenState").contains(NullValue),
          value.errors.nonEmpty
        )
      },
      test("hides an argument when any contribution marks it inaccessible") {
        val visible = schema(
          "type Query { search(term: String): String @shareable }",
          "\"@shareable\", \"@inaccessible\""
        )
        val hidden  = schema(
          "type Query { search(term: String @inaccessible): String @shareable }",
          "\"@shareable\", \"@inaccessible\""
        )

        for {
          alpha    <- stub("""{"data":{"search":"ok"}}""")
          beta     <- stub("""{"data":{"search":"ok"}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("alpha", alpha.endpoint, visible),
                          Subgraph.federation("beta", beta.endpoint, hidden)
                        )
                        .build
          response <- gateway.execute("{ search(term: \"secret\") }")
          requests <- alpha.requests.get.zip(beta.requests.get)
        } yield assertTrue(response.errors.nonEmpty, requests._1.isEmpty, requests._2.isEmpty)
      },
      test("rejects visible input coordinates that reference inaccessible types") {
        val argumentSchema = schema(
          "type Query { search(secret: Secret): String } input Secret @inaccessible { value: String }",
          "\"@inaccessible\""
        )
        val inputSchema    = schema(
          "type Query { find(filter: Filter): String } input Filter { secret: Secret } input Secret @inaccessible { value: String }",
          "\"@inaccessible\""
        )

        Gateway
          .compose(
            Subgraph.federation("arguments", endpoint, argumentSchema),
            Subgraph.federation("inputs", endpoint, inputSchema)
          )
          .build
          .exit
          .map { exit =>
            val diagnostics = exit.causeOption.flatMap(_.failureOption).map(_.diagnostics).getOrElse(Nil)
            assertTrue(
              diagnostics.exists(_.startsWith("[arguments]")),
              diagnostics.exists(_.startsWith("[inputs]"))
            )
          }
      },
      test("keeps query and mutation argument visibility separate") {
        val operationSchema =
          s"""
             |schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@inaccessible"]) {
             |  query: Query
             |  mutation: Mutation
             |}
             |$authoredFederationDirectives
             |type Query { foo(secret: Secret @inaccessible): String }
             |type Mutation { foo(secret: Secret): String }
             |input Secret @inaccessible { value: String }
             |""".stripMargin

        Gateway
          .compose(Subgraph.federation("operations", endpoint, operationSchema))
          .build
          .exit
          .map(exit =>
            assertTrue(
              exit.causeOption
                .flatMap(_.failureOption)
                .exists(
                  _.diagnostics.exists(message => message.startsWith("[operations]") && message.contains("foo.secret"))
                )
            )
          )
      },
      test("does not expose inaccessible implemented interfaces") {
        val hiddenSchema = schema(
          """
            |type Query { product: Product! }
            |interface HiddenInterface @inaccessible { id: ID! }
            |type Product implements HiddenInterface { id: ID! }
            |""".stripMargin,
          "\"@inaccessible\""
        )

        for {
          source   <- stub("""{"data":{"product":{"id":"p1"}}}""")
          gateway  <- Gateway.compose(Subgraph.federation("products", source.endpoint, hiddenSchema)).build
          response <-
            gateway.execute(
              "{ visible: __type(name: \"Product\") { interfaces { name } } hidden: __type(name: \"HiddenInterface\") { name } }"
            )
        } yield assertTrue(
          field(response.data, "visible").flatMap(field(_, "interfaces")).contains(ListValue(Nil)),
          field(response.data, "hidden").contains(NullValue)
        )
      }
    ),
    suite("type merging")(
      test("intersects input-object fields across subgraphs") {
        val alphaSchema =
          "type Query { alpha(filter: Filter!): Int! } input Filter { required: Int! alphaOnly: Int }"
        val betaSchema  =
          "type Query { beta(filter: Filter!): Int! } input Filter { required: Int! betaOnly: Int }"

        for {
          alpha   <- stub("""{"data":{"alpha":1}}""")
          beta    <- stub("""{"data":{"beta":2}}""")
          gateway <- Gateway
                       .compose(
                         Subgraph.graphql("alpha", alpha.endpoint, alphaSchema),
                         Subgraph.graphql("beta", beta.endpoint, betaSchema)
                       )
                       .build
          valid   <- gateway.execute("{ alpha(filter: { required: 1 }) beta(filter: { required: 2 }) }")
          invalid <- gateway.execute("{ alpha(filter: { required: 1, alphaOnly: 2 }) }")
        } yield assertTrue(
          field(valid.data, "alpha").contains(IntValue(1)),
          field(valid.data, "beta").contains(IntValue(2)),
          invalid.data == NullValue,
          invalid.errors.nonEmpty
        )
      },
      test("unions compatible union-member contributions") {
        val alphaSchema =
          "type Query { alpha: Search } union Search = Product type Product { id: ID! }"
        val betaSchema  =
          "type Query { beta: Search } union Search = Review type Review { body: String! }"

        for {
          alpha    <- stub("""{"data":{"alpha":{"__typename":"Product","id":"p1"}}}""")
          beta     <- stub("""{"data":{"beta":{"__typename":"Review","body":"good"}}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.graphql("alpha", alpha.endpoint, alphaSchema),
                          Subgraph.graphql("beta", beta.endpoint, betaSchema)
                        )
                        .build
          response <- gateway.execute(
                        "{ alpha { ... on Product { id } } beta { ... on Review { body } } }"
                      )
        } yield assertTrue(response.errors.isEmpty)
      },
      test("rejects incompatible enums used as both input and output") {
        val alphaSchema =
          "type Query { alpha(value: State): State } enum State { ACTIVE HIDDEN }"
        val betaSchema  =
          "type Query { beta(value: State): State } enum State { ACTIVE }"

        Gateway
          .compose(
            Subgraph.graphql("alpha", endpoint, alphaSchema),
            Subgraph.graphql("beta", endpoint, betaSchema)
          )
          .build
          .exit
          .map(exit =>
            assertTrue(
              exit.causeOption.flatMap(_.failureOption).exists(_.diagnostics.exists(_.contains("Input/output enum")))
            )
          )
      },
      test("rejects incompatible argument and input-field defaults") {
        val alphaSchema =
          "type Query { value(filter: Filter, count: Int = 1): Int } input Filter { limit: Int = 1 }"
        val betaSchema  =
          "type Query { value(filter: Filter, count: Int = 2): Int } input Filter { limit: Int = 2 }"

        Gateway
          .compose(
            Subgraph.graphql("alpha", endpoint, alphaSchema),
            Subgraph.graphql("beta", endpoint, betaSchema)
          )
          .build
          .exit
          .map { exit =>
            val diagnostics = exit.causeOption.flatMap(_.failureOption).map(_.diagnostics).getOrElse(Nil)
            assertTrue(
              diagnostics.exists(_.startsWith("[query.value]")),
              diagnostics.exists(_.startsWith("[type Filter.limit]"))
            )
          }
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

package caliban.gateway

import caliban.InputValue.{ ListValue, ObjectValue => InputObjectValue }
import caliban.ResponseValue.{ ListValue => ResponseListValue, ObjectValue => ResponseObjectValue }
import caliban.Value.IntValue.IntNumber
import caliban.Value.{ NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.InputValue
import sttp.model.Uri
import zio._
import zio.test._

object EntityRoutingSpec extends ZIOSpecDefault {

  private val authoredProductsFederationSchema =
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"]) { query: Query }
       |$authoredFederationDirectives
       |type Query { product(id: ID!): Product }
       |type Product @key(fields: "id") { id: ID! name: String! }
       |""".stripMargin

  private val authoredReviewsFederationSchema =
    s"""
       |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
       |$authoredFederationDirectives
       |type Product @key(fields: "id") { id: ID! @external reviews: [Review!]! }
       |type Review { body: String! }
       |""".stripMargin

  def spec = suite("EntityRoutingSpec")(
    suite("Federation schemas and routing")(
      test("selects a satisfiable compound key from competing Federation keys") {
        val productsSchema  =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
             |$authoredFederationDirectives
             |type Query { product: Product }
             |type Product @key(fields: "id organization { id }") {
             |  id: ID!
             |  sku: ID @external
             |  organization: Organization!
             |  name: String!
             |}
             |type Organization { id: ID! }
             |""".stripMargin
        val reviewsSchema   =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
             |$authoredFederationDirectives
             |type Product
             |  @key(fields: "sku")
             |  @key(fields: "id organization { id }") {
             |  id: ID! @external
             |  sku: ID
             |  organization: Organization! @external
             |  reviews: [Review!]!
             |}
             |type Organization { id: ID! }
             |type Review { body: String! }
             |""".stripMargin
        val productResponse =
          """{"data":{"product":{"id":"p1","name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_key_2":{"id":"o1"},"_caliban_gateway_typename":"Product"}}}"""
        val reviewResponse  =
          """{"data":{"_entities":[{"reviews":[{"body":"Solid"}]}]}}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("reviews", reviews.endpoint, reviewsSchema)
                        )
                        .build
          response <- gateway.execute("{ product { id name reviews { body } } }")
          sent     <- reviews.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "id")).contains(StringValue("p1")),
          field(response.data, "product").flatMap(field(_, "reviews")).exists {
            case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
              review.contains("body" -> StringValue("Solid"))
            case _                                                     => false
          },
          sent.headOption
            .flatMap(_.variables)
            .contains(
              Map(
                "representations" -> ListValue(
                  List(
                    InputObjectValue(
                      Map(
                        "__typename"   -> StringValue("Product"),
                        "id"           -> StringValue("p1"),
                        "organization" -> InputObjectValue(Map("id" -> StringValue("o1")))
                      )
                    )
                  )
                )
              )
            )
        )
      },
      test("routes through an intermediate key source and skips null intermediate entities") {
        val booksSchema        =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"])
             |$authoredFederationDirectives
             |type Query { books: [Book!]! }
             |type Book @key(fields: "upc") { upc: ID! }
             |""".stripMargin
        val identitiesSchema   =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"])
             |$authoredFederationDirectives
             |type Book @key(fields: "id") @key(fields: "upc") { id: ID! upc: ID! }
             |""".stripMargin
        val authorsSchema      =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"])
             |$authoredFederationDirectives
             |type Book @key(fields: "id") { id: ID! author: Author }
             |type Author { name: String! }
             |""".stripMargin
        val booksResponse      =
          """{"data":{"books":[{"upc":"b1","_caliban_gateway_key":"b1","_caliban_gateway_typename":"Book"},{"upc":"b2","_caliban_gateway_key":"b2","_caliban_gateway_typename":"Book"},{"upc":"b3","_caliban_gateway_key":"b3","_caliban_gateway_typename":"Book"}]}}"""
        val identitiesResponse =
          """{"data":{"_entities":[{"_caliban_gateway_key":"1","_caliban_gateway_typename":"Book","_caliban_gateway_entity_key":"b1","_caliban_gateway_entity_typename":"Book"},{"_caliban_gateway_key":"2","_caliban_gateway_typename":"Book","_caliban_gateway_entity_key":"b2","_caliban_gateway_entity_typename":"Book"},null]}}"""
        val authorsResponse    =
          """{"data":{"_entities":[{"author":{"name":"Alice"}},{"author":{"name":"Bob"}}]}}"""

        for {
          books       <- stub(booksResponse)
          identities  <- stub(identitiesResponse)
          authors     <- stub(authorsResponse)
          gateway     <- Gateway
                           .compose(
                             Subgraph.federation("books", books.endpoint, booksSchema),
                             Subgraph.federation("identities", identities.endpoint, identitiesSchema),
                             Subgraph.federation("authors", authors.endpoint, authorsSchema)
                           )
                           .build
          response    <- gateway.execute("{ books { upc author { name } } }")
          authorCalls <- authors.requests.get
          values       = field(response.data, "books").collect { case ResponseListValue(items) => items }.getOrElse(Nil)
        } yield assertTrue(
          response.errors.isEmpty,
          values.headOption.flatMap(field(_, "author")).flatMap(field(_, "name")).contains(StringValue("Alice")),
          values.lift(1).flatMap(field(_, "author")).flatMap(field(_, "name")).contains(StringValue("Bob")),
          values.lift(2).flatMap(field(_, "author")).contains(NullValue),
          authorCalls.size == 1,
          authorCalls.headOption
            .flatMap(_.variables)
            .contains(
              Map(
                "representations" -> ListValue(
                  List(
                    InputObjectValue(Map("__typename" -> StringValue("Book"), "id" -> StringValue("1"))),
                    InputObjectValue(Map("__typename" -> StringValue("Book"), "id" -> StringValue("2")))
                  )
                )
              )
            )
        )
      },
      test("tries later bridge sources when the first declared bridge is unreachable") {
        val rootsSchema       =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"])
             |$authoredFederationDirectives
             |type Query { thing: Thing }
             |type Thing @key(fields: "a") { a: ID! }
             |""".stripMargin
        val unreachableSchema =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
             |$authoredFederationDirectives
             |type Thing @key(fields: "a") { a: ID! @external d: ID! @external }
             |""".stripMargin
        val bridgeSchema      =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"])
             |$authoredFederationDirectives
             |type Thing @key(fields: "a") @key(fields: "d") { a: ID! d: ID! }
             |""".stripMargin
        val targetSchema      =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"])
             |$authoredFederationDirectives
             |type Thing @key(fields: "d") { d: ID! label: String! }
             |""".stripMargin
        val rootResponse      =
          """{"data":{"thing":{"a":"a1","_caliban_gateway_key":"a1","_caliban_gateway_typename":"Thing"}}}"""
        val bridgeResponse    =
          """{"data":{"_entities":[{"_caliban_gateway_key":"d1","_caliban_gateway_typename":"Thing"}]}}"""
        val targetResponse    =
          """{"data":{"_entities":[{"label":"reachable"}]}}"""

        for {
          roots       <- stub(rootResponse)
          unreachable <- stub("""{"data":{"_entities":[]}}""")
          bridge      <- stub(bridgeResponse)
          target      <- stub(targetResponse)
          gateway     <- Gateway
                           .compose(
                             Subgraph.federation("a-roots", roots.endpoint, rootsSchema),
                             Subgraph.federation("b-unreachable", unreachable.endpoint, unreachableSchema),
                             Subgraph.federation("c-bridge", bridge.endpoint, bridgeSchema),
                             Subgraph.federation("d-target", target.endpoint, targetSchema)
                           )
                           .build
          response    <- gateway.execute("{ thing { a label } }")
          bCalls      <- unreachable.requests.get
          cCalls      <- bridge.requests.get
          dCalls      <- target.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "thing").flatMap(field(_, "label")).contains(StringValue("reachable")),
          bCalls.isEmpty,
          cCalls.size == 1,
          dCalls.size == 1
        )
      },
      test("executes independent entity routes concurrently") {
        val productsSchema  =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"])
             |$authoredFederationDirectives
             |type Query { product: Product }
             |type Product @key(fields: "id") { id: ID! }
             |""".stripMargin
        val reviewsSchema   =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
             |$authoredFederationDirectives
             |type Product @key(fields: "id") { id: ID! @external reviews: [Review!]! }
             |type Review { body: String! }
             |""".stripMargin
        val pricesSchema    =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
             |$authoredFederationDirectives
             |type Product @key(fields: "id") { id: ID! @external price: Int! }
             |""".stripMargin
        val productResponse =
          """{"data":{"product":{"id":"p1","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product"}}}"""

        for {
          ready    <- Ref.make(0)
          released <- Promise.make[Nothing, Unit]
          barrier   = ready.updateAndGet(_ + 1).flatMap(count => released.succeed(()).when(count == 2)) *> released.await
          products <- stub(productResponse)
          reviews  <- stubWith(barrier, """{"data":{"_entities":[{"reviews":[{"body":"Solid"}]}]}}""")
          prices   <- stubWith(barrier, """{"data":{"_entities":[{"price":100}]}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("reviews", reviews.endpoint, reviewsSchema),
                          Subgraph.federation("prices", prices.endpoint, pricesSchema)
                        )
                        .build
          response <- gateway.execute("{ product { id reviews { body } price } }")
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "price")).contains(IntNumber(100)),
          field(response.data, "product").flatMap(field(_, "reviews")).exists {
            case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
              review.contains("body" -> StringValue("Solid"))
            case _                                                     => false
          }
        )
      } @@ TestAspect.timeout(5.seconds),
      test("routes an interface key using the concrete runtime typename") {
        val nodesSchema     =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"])
             |$authoredFederationDirectives
             |type Query { node: Node }
             |interface Node @key(fields: "id") { id: ID! }
             |type Product implements Node @key(fields: "id") { id: ID! }
             |""".stripMargin
        val detailsSchema   =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
             |$authoredFederationDirectives
             |interface Node @key(fields: "id") { id: ID! @external label: String! }
             |type Product implements Node @key(fields: "id") { id: ID! @external label: String! }
             |""".stripMargin
        val nodeResponse    =
          """{"data":{"node":{"id":"p1","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val detailsResponse =
          """{"data":{"_entities":[{"label":"Table"}]}}"""

        for {
          nodes    <- stub(nodeResponse)
          details  <- stub(detailsResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("nodes", nodes.endpoint, nodesSchema),
                          Subgraph.federation("details", details.endpoint, detailsSchema)
                        )
                        .build
          response <- gateway.execute("{ node { id label } }")
          sent     <- details.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "node").flatMap(field(_, "id")).contains(StringValue("p1")),
          field(response.data, "node").flatMap(field(_, "label")).contains(StringValue("Table")),
          sent.headOption
            .flatMap(_.variables)
            .contains(
              Map(
                "representations" -> ListValue(
                  InputObjectValue(Map("__typename" -> StringValue("Product"), "id" -> StringValue("p1"))) :: Nil
                )
              )
            )
        )
      },
      test("moves an unresolvable child selection to a resolvable parent entity") {
        val productsSchema   =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@shareable"])
             |$authoredFederationDirectives
             |type Query { products: [Product!]! }
             |type Product @key(fields: "id pid") { id: ID! pid: ID! category: Category @shareable }
             |type Category @key(fields: "id") { id: ID! }
             |""".stripMargin
        val detailsSchema    =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@shareable"])
             |$authoredFederationDirectives
             |type Product @key(fields: "id pid") { id: ID! pid: ID! category: Category @shareable }
             |type Category { details: Details }
             |type Details { products: Int! }
             |""".stripMargin
        val productsResponse =
          """{"data":{"products":[{"id":"p1","category":{"id":"c1"},"_caliban_gateway_key":"p1","_caliban_gateway_key_2":"parent-1","_caliban_gateway_typename":"Product"}]}}"""
        val detailsResponse  =
          """{"data":{"_entities":[{"category":{"details":{"products":2}}}]}}"""

        for {
          products <- stub(productsResponse)
          details  <- stub(detailsResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("details", details.endpoint, detailsSchema)
                        )
                        .build
          response <- gateway.execute("{ products { id category { id details { products } } } }")
          sent     <- details.requests.get
          category  = field(response.data, "products").collect { case ResponseListValue(product :: Nil) => product }
                        .flatMap(field(_, "category"))
        } yield assertTrue(
          response.errors.isEmpty,
          category.flatMap(field(_, "id")).contains(StringValue("c1")),
          category.flatMap(field(_, "details")).flatMap(field(_, "products")).contains(IntNumber(2)),
          sent.headOption
            .flatMap(_.variables)
            .contains(
              Map(
                "representations" -> ListValue(
                  InputObjectValue(
                    Map(
                      "__typename" -> StringValue("Product"),
                      "id"         -> StringValue("p1"),
                      "pid"        -> StringValue("parent-1")
                    )
                  ) :: Nil
                )
              )
            )
        )
      },
      test("retains a conventional Query root alongside a schema link extension") {
        val linkedSchema =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"])
             |$authoredFederationDirectives
             |type Query { product: Product }
             |type Product @key(fields: "id") { id: ID! name: String! }
             |""".stripMargin

        for {
          products <- stub("""{"data":{"product":{"id":"p1","name":"Table"}}}""")
          gateway  <- Gateway.compose(Subgraph.federation("products", products.endpoint, linkedSchema)).build
          response <- gateway.execute("{ product { id name } }")
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "id")).contains(StringValue("p1")),
          field(response.data, "product").flatMap(field(_, "name")).contains(StringValue("Table"))
        )
      },
      test("executes a join from entity-only authored Federation service SDL with namespaced metadata") {
        val productResponse =
          """{"data":{"product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val reviewResponse  =
          """{"data":{"_entities":[{"reviews":[{"body":"Solid"}]}]}}"""
        val productsSchema  = authoredProductsFederationSchema
          .replace(", import: [\"@key\"]", ", as: \"fed\"")
          .replace("federation__FieldSet", "fed__FieldSet")
          .replace("@key", "@fed__key")
        val reviewsSchema   = authoredReviewsFederationSchema
          .replace(", import: [\"@key\", \"@external\"]", ", as: \"fed\"")
          .replace("federation__FieldSet", "fed__FieldSet")
          .replace("@key", "@fed__key")
          .replace("@external", "@fed__external")

        for {
          products  <- stub(productResponse)
          reviews   <- stub(reviewResponse)
          gateway   <- Gateway
                         .compose(
                           Subgraph.federation("products", products.endpoint, productsSchema),
                           Subgraph.federation("reviews", reviews.endpoint, reviewsSchema)
                         )
                         .build
          response  <- gateway.execute("{ product(id: \"p1\") { name reviews { body } } }")
          metadata  <-
            gateway.execute(
              "{ transport: __type(name: \"fed__FieldSet\") { name } schema: __schema { directives { name } } }"
            )
          sentA     <- products.requests.get
          sentB     <- reviews.requests.get
          directives = field(metadata.data, "schema")
                         .flatMap(field(_, "directives"))
                         .collect { case ResponseListValue(values) =>
                           values.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                         }
        } yield assertTrue(
          response.errors.isEmpty,
          metadata.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "name")).contains(StringValue("Table")),
          field(response.data, "product").flatMap(field(_, "reviews")).exists {
            case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
              review.contains("body" -> StringValue("Solid"))
            case _                                                     => false
          },
          field(metadata.data, "transport").contains(NullValue),
          directives.exists(names => !names.contains("fed__key") && !names.contains("fed__external")),
          sentA.size == 1,
          sentB.size == 1,
          sentB.head.query.exists(query =>
            query.contains("_entities") && !query.contains("_caliban_gateway_entity_key")
          )
        )
      },
      test("hides imported Federation directive aliases from the client schema") {
        val endpoint = Uri.unsafeParse("http://127.0.0.1:1/graphql")
        val products = productsFederationSchema
          .replace("import: [\"@key\"]", "import: [{ name: \"@key\", as: \"@entityKey\" }]")
          .replace("directive @key", "directive @entityKey")
          .replace("@key(fields:", "@entityKey(fields:")
        val reviews  = reviewsFederationSchema
          .replace("directive @key", "directive @entityKey")
          .replace("@key(fields:", "@entityKey(fields:")
          .replace("directive @external", "directive @outside")
          .replace("id: ID! @external", "id: ID! @outside")
          .replace(
            "import: [\"@key\", \"@external\"]",
            "import: [{ name: \"@key\", as: \"@entityKey\" }, { name: \"@external\", as: \"@outside\" }]"
          )

        for {
          gateway       <- Gateway
                             .compose(
                               Subgraph.federation("products", endpoint, products),
                               Subgraph.federation("reviews", endpoint, reviews)
                             )
                             .build
          introspection <- gateway.execute("{ __schema { directives { name } } }")
          directives     = field(introspection.data, "__schema")
                             .flatMap(field(_, "directives"))
                             .collect { case ResponseListValue(values) =>
                               values.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                             }
        } yield assertTrue(
          introspection.errors.isEmpty,
          directives.exists(names => !names.contains("entityKey") && !names.contains("outside"))
        )
      },
      test("rejects derived entity dependency cycles before contacting a subgraph") {
        val rootsSchema =
          """
            |type Query { thing: Thing }
            |type Thing { seed: ID! }
            |""".stripMargin
        val leftSchema  =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
             |$authoredFederationDirectives
             |type Thing @key(fields: "c") { b: ID! c: ID! @external }
             |""".stripMargin
        val rightSchema =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
             |$authoredFederationDirectives
             |type Thing @key(fields: "b") { b: ID! @external c: ID! }
             |""".stripMargin

        for {
          roots     <- stub("""{"data":{"thing":{"seed":"root"}}}""")
          left      <- stub("""{"data":{"_entities":[]}}""")
          right     <- stub("""{"data":{"_entities":[]}}""")
          gateway   <- Gateway
                         .compose(
                           Subgraph.graphql("roots", roots.endpoint, rootsSchema),
                           Subgraph.federation("left", left.endpoint, leftSchema),
                           Subgraph.federation("right", right.endpoint, rightSchema)
                         )
                         .build
          response  <- gateway.execute("{ thing { b c } }")
          rootSent  <- roots.requests.get
          leftSent  <- left.requests.get
          rightSent <- right.requests.get
        } yield assertTrue(
          response.errors.map(_.msg) == List("Entity routing dependency cycle detected."),
          rootSent.isEmpty,
          leftSent.isEmpty,
          rightSent.isEmpty
        )
      },
      test("does not route fields declared only as external") {
        val rootsSchema    =
          """
            |type Query { thing: Thing }
            |type Thing { id: ID! }
            |""".stripMargin
        val externalSchema =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
             |$authoredFederationDirectives
             |type Thing @key(fields: "id") { id: ID! @external ghost: String @external }
             |""".stripMargin

        for {
          roots     <- stub("""{"data":{"thing":{"id":"t1"}}}""")
          externalA <- stub("""{"data":{"_entities":[]}}""")
          externalB <- stub("""{"data":{"_entities":[]}}""")
          gateway   <- Gateway
                         .compose(
                           Subgraph.graphql("roots", roots.endpoint, rootsSchema),
                           Subgraph.federation("external-a", externalA.endpoint, externalSchema),
                           Subgraph.federation("external-b", externalB.endpoint, externalSchema)
                         )
                         .build
          response  <- gateway.execute("{ thing { ghost } }")
          rootSent  <- roots.requests.get
          sentA     <- externalA.requests.get
          sentB     <- externalB.requests.get
        } yield assertTrue(
          response.errors.map(_.msg) == List("Field 'ghost' does not exist on type 'Thing'."),
          rootSent.isEmpty,
          sentA.isEmpty,
          sentB.isEmpty
        )
      },
      test("rejects unsatisfied entity routing obligations before contacting a subgraph") {
        val missingKeyProducts = productsFederationSchema.replace("id: ID! name: String!", "name: String!")
        val noLookupReviews    = reviewsFederationSchema.replace(
          "@key(fields: \"id\")",
          "@key(fields: \"id\", resolvable: false)"
        )
        val wrongEntityReviews = reviewsFederationSchema.replace("union _Entity = Product", "union _Entity = Review")
        val cycleProducts      = productsFederationSchema.replace(
          "type Product @key(fields: \"id\") { id: ID! name: String! }",
          "type Product @key(fields: \"id\") { id: ID! name: String! } type Review @key(fields: \"id\") { id: ID! product: Product! }"
        )
        val cycleReviews       = reviewsFederationSchema.replace(
          "type Review { body: String! }",
          "type Review @key(fields: \"id\") { id: ID! body: String! product: Product! @external }"
        )

        def rejected(productsSchema: String, reviewsSchema: String, query: String) =
          for {
            products <- stub("""{"data":{"product":null}}""")
            reviews  <- stub("""{"data":{"_entities":[]}}""")
            gateway  <- Gateway
                          .compose(
                            Subgraph.federation("products", products.endpoint, productsSchema),
                            Subgraph.federation("reviews", reviews.endpoint, reviewsSchema)
                          )
                          .build
            response <- gateway.execute(query)
            sentA    <- products.requests.get
            sentB    <- reviews.requests.get
          } yield (response.errors.map(_.msg), sentA, sentB)

        for {
          missing     <- rejected(
                           missingKeyProducts,
                           reviewsFederationSchema,
                           "{ product(id: \"p1\") { reviews { body } } }"
                         )
          lookup      <- rejected(
                           productsFederationSchema,
                           noLookupReviews,
                           "{ product(id: \"p1\") { reviews { body } } }"
                         )
          wrongEntity <- rejected(
                           productsFederationSchema,
                           wrongEntityReviews,
                           "{ product(id: \"p1\") { reviews { body } } }"
                         )
          cycle       <- rejected(
                           cycleProducts,
                           cycleReviews,
                           "{ product(id: \"p1\") { reviews { product { name } } } }"
                         )
        } yield assertTrue(
          missing._1 == List("Entity routing obligations are unsatisfied: 'reviews:reviews.body'."),
          lookup._1 == List("Entity routing obligations are unsatisfied: 'reviews:reviews.body'."),
          wrongEntity._1 == List("Entity routing obligations are unsatisfied: 'reviews:reviews.body'."),
          cycle._1 == List("Entity routing cycle detected: products -> reviews for Product(reviews.product.name)."),
          missing._2.isEmpty,
          missing._3.isEmpty,
          lookup._2.isEmpty,
          lookup._3.isEmpty,
          wrongEntity._2.isEmpty,
          wrongEntity._3.isEmpty,
          cycle._2.isEmpty,
          cycle._3.isEmpty
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

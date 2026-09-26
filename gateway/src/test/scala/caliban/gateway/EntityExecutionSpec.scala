package caliban.gateway

import caliban.ResponseValue.{ ListValue => ResponseListValue, ObjectValue => ResponseObjectValue }
import caliban.Value.IntValue.IntNumber
import caliban.Value.{ BooleanValue, NullValue, StringValue }
import caliban.federation.EntityResolver
import caliban.federation.v2_6.{ federated, GQLKey }
import caliban.gateway.GatewayTestSupport._
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
import caliban.{ graphQL, CalibanError, GraphQLRequest, GraphQLResponse, PathValue, RootResolver }
import zio._
import zio.query.ZQuery
import zio.test._

object EntityExecutionSpec extends ZIOSpecDefault {

  private val listProductsFederationSchema =
    productsFederationSchema.replace("product(id: ID!): Product", "products: [Product!]!")

  private val statusProductsSchema =
    productsFederationSchema.replace("  product(id: ID!): Product", "  product(id: ID!): Product\n  status: String!")

  private val tableProductResponse =
    """{"data":{"product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""

  private val statusProductResponse =
    """{"data":{"status":"available","product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""

  private val repeatedProductsResponse =
    """{"data":{"first":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}],"second":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}]}}"""

  private val solidReviewsResponse =
    """{"data":{"_entities":[{"reviews":[{"body":"Solid"}]}]}}"""

  private val nullableReviewsSchema = reviewsFederationSchema.replace("reviews: [Review!]!", "reviews: [Review!]")

  private val limitedReviewsSchema =
    reviewsFederationSchema.replace("reviews: [Review!]!", "reviews(limit: Int!): [Review!]!")

  private val pricingDownExtensions =
    ResponseObjectValue(List("code" -> StringValue("PRICING_DOWN"), "debug" -> StringValue("local detail")))

  private object StockApi extends GenericSchema[Any] {
    import auto._

    final case class ProductArgs(id: Long)
    final case class Product(id: Long, stock: Int)
    final case class Query(warehouse: String)

    implicit val productArgsSchema: Schema[Any, ProductArgs] = Schema.gen
    implicit val productArgsBuilder: ArgBuilder[ProductArgs] = ArgBuilder.gen
    implicit val productSchema: Schema[Any, Product]         =
      obj("Product", directives = List(GQLKey("id").directive))(implicit attributes =>
        List(
          field("id")(_.id),
          field("stock")(_.stock)
        )
      )

    val api = graphQL(RootResolver(Query("main"))) @@ federated(
      EntityResolver.from[ProductArgs](args => ZQuery.succeed(Some(Product(args.id, 7))))
    )
  }

  private val longKeyProductsSchema =
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"]) { query: Query }
       |$federationDirectives
       |scalar Long
       |union _Entity = Product
       |type Query {
       |  product: Product
       |  _entities(representations: [_Any!]!): [_Entity]!
       |  _service: _Service!
       |}
       |type Product @key(fields: "id") { id: Long! name: String! }
       |""".stripMargin

  private trait Pricing {
    def currency: UIO[String]
    def price(id: String): UIO[Int]
  }

  private object PricingApi extends GenericSchema[Pricing] {
    import auto._

    final case class ProductId(value: String)
    final case class ProductArgs(id: ProductId)
    final case class Product(id: ProductId, price: Int)
    final case class Query(currency: URIO[Pricing, String])

    implicit val productIdSchema: Schema[Any, ProductId]     =
      Schema.scalarSchema("ID", None, None, None, id => StringValue(id.value))
    implicit val productIdBuilder: ArgBuilder[ProductId]     = ArgBuilder.string.map(ProductId(_))
    implicit val productArgsSchema: Schema[Any, ProductArgs] = Schema.gen
    implicit val productArgsBuilder: ArgBuilder[ProductArgs] = ArgBuilder.gen
    implicit val querySchema: Schema[Pricing, Query]         = gen
    implicit val productSchema: Schema[Any, Product]         =
      obj("Product", directives = List(GQLKey("id").directive))(implicit attributes =>
        List(
          field("id")(_.id),
          field("price")(_.price)
        )
      )

    val api = graphQL(
      RootResolver(Query(ZIO.serviceWithZIO[Pricing](_.currency)))
    ) @@ federated(
      EntityResolver.from[ProductArgs](args =>
        ZQuery
          .fromZIO(ZIO.serviceWithZIO[Pricing](_.price(args.id.value)))
          .map(price => Some(Product(args.id, price)))
      )
    )

    val failingApi = graphQL(
      RootResolver(Query(ZIO.serviceWithZIO[Pricing](_.currency)))
    ) @@ federated(
      EntityResolver.from[ProductArgs] { _ =>
        val failure: ZQuery[Any, CalibanError, Option[Product]] = ZQuery.fail(
          CalibanError.ExecutionError(
            "local pricing unavailable",
            path = List(PathValue.Key("_pricing_internal")),
            extensions = Some(pricingDownExtensions)
          )
        )
        failure
      }
    )
  }

  private def progressiveProductSchema(query: String, product: String): String =
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.7", import: ["@key", "@override"]) { query: Query }
       |$federationDirectives
       |directive @override(from: String!, label: String) on FIELD_DEFINITION
       |union _Entity = Product
       |type Query {
       |  $query
       |  _entities(representations: [_Any!]!): [_Entity]!
       |  _service: _Service!
       |}
       |type Product @key(fields: "id") { $product }
       |""".stripMargin

  private def progressiveThingSchema(query: String, name: String): String =
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.7", import: ["@key", "@override", "@shareable"]) { query: Query }
       |$federationDirectives
       |directive @override(from: String!, label: String) on FIELD_DEFINITION
       |union _Entity = Product
       |type Query {
       |  $query
       |  _entities(representations: [_Any!]!): [_Entity]!
       |  _service: _Service!
       |}
       |interface Thing { id: ID! name: String }
       |type Product implements Thing @key(fields: "id") { id: ID! name: String $name }
       |""".stripMargin

  private def thingStub(name: String) =
    stubByRequest { request =>
      val query = request.query.getOrElse("")
      if (query.contains(EntitiesField)) s"""{"data":{"_entities":[{"name":"$name"}]}}"""
      else if (query.contains("_caliban_gateway_key"))
        """{"data":{"things":[{"_caliban_gateway_typename":"Product","_caliban_gateway_runtime_typename":"Product","_caliban_gateway_key":"p1"}]}}"""
      else s"""{"data":{"things":[{"_caliban_gateway_runtime_typename":"Product","name":"$name"}]}}"""
    }

  private def thingNames(label: String, originalRoot: String, replacementRoot: String) = {
    val originalSchema    = progressiveThingSchema(originalRoot, "")
    val replacementSchema =
      progressiveThingSchema(replacementRoot, s"""@override(from: "original", label: "$label")""")
    for {
      original    <- thingStub("original")
      replacement <- thingStub("replacement")
      runtime     <- progressiveGateway(original, originalSchema, replacement, replacementSchema).interpreter
      response    <- runtime.execute("{ things { name } }")
    } yield response
  }

  private def thingName(response: GraphQLResponse[CalibanError]) =
    field(response.data, "things").collect { case ResponseListValue(things) => things.flatMap(field(_, "name")) }

  def spec = suite("EntityExecutionSpec")(
    suite("entity execution")(
      test("routes a progressive entity field override to the selected subgraph") {
        val originalSchema    = progressiveProductSchema("product(id: ID!): Product", "id: ID! name: String!")
        val replacementSchema =
          progressiveProductSchema("", """id: ID! name: String! @override(from: "original", label: "percent(100)")""")

        for {
          original    <-
            stub(
              """{"data":{"product":{"name":"original","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
            )
          replacement <-
            stub(
              """{"data":{"_entities":[{"name":"replacement"}]}}"""
            )
          runtime     <- progressiveGateway(original, originalSchema, replacement, replacementSchema).interpreter
          response    <- runtime.execute("{ product(id: \"p1\") { name } }")
          sent        <- replacement.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "name")).contains(StringValue("replacement")),
          sent.size == 1
        )
      },
      test("keeps a progressively overridden field nullable while the original still serves it") {
        val originalSchema    = progressiveProductSchema("product(id: ID!): Product", "id: ID! name: String sku: String!")
        val replacementSchema =
          progressiveProductSchema("", """id: ID! name: String! @override(from: "original", label: "percent(0)")""")

        for {
          original    <- stub("""{"data":{"product":{"name":null,"sku":"sku-1"}}}""")
          replacement <- stub("""{"data":{"_entities":[{"name":"replacement"}]}}""")
          runtime     <- progressiveGateway(original, originalSchema, replacement, replacementSchema).interpreter
          response    <- runtime.execute("{ product(id: \"p1\") { name sku } }")
          sent        <- replacement.requests.get
          product      = field(response.data, "product")
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "name")).contains(NullValue),
          product.flatMap(field(_, "sku")).contains(StringValue("sku-1")),
          sent.isEmpty
        )
      },
      test("routes an interface field to the progressive override of its implementation") {
        for {
          active   <- thingNames("percent(100)", "things: [Thing]", "")
          inactive <- thingNames("percent(0)", "things: [Thing]", "")
        } yield assertTrue(
          active.errors.isEmpty,
          thingName(active).contains(List(StringValue("replacement"))),
          inactive.errors.isEmpty,
          thingName(inactive).contains(List(StringValue("original")))
        )
      },
      test("routes an interface field under a shareable root to the progressive override of its implementation") {
        val root = "things: [Thing] @shareable"
        for {
          active   <- thingNames("percent(100)", root, root)
          inactive <- thingNames("percent(0)", root, root)
        } yield assertTrue(
          active.errors.isEmpty,
          thingName(active).contains(List(StringValue("replacement"))),
          inactive.errors.isEmpty,
          thingName(inactive).contains(List(StringValue("original")))
        )
      },
      test("executes remote Products, local Pricing, and remote Reviews in one operation") {
        val productsResponse =
          """{"data":{"product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product"}}}"""
        val pricing          = new Pricing {
          def currency: UIO[String]       = ZIO.succeed("USD")
          def price(id: String): UIO[Int] = ZIO.succeed(if (id == "p1") 125 else 0)
        }

        for {
          products <- stub(productsResponse)
          reviews  <- stub(solidReviewsResponse)
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsFederationSchema),
                          Subgraph.federation("pricing", PricingApi.api),
                          Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                        )
                        .interpreter
          response <- runtime
                        .execute("{ product(id: \"p1\") { name price reviews { body } } currency }")
                        .provideEnvironment(ZEnvironment(pricing))
          product   = field(response.data, "product")
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "currency").contains(StringValue("USD")),
          product.flatMap(field(_, "name")).contains(StringValue("Table")),
          product.flatMap(field(_, "price")).contains(IntNumber(125)),
          onlyNested(product, "reviews").exists(_.contains("body" -> StringValue("Solid")))
        )
      },
      test("correlates entity keys that local and remote subgraphs encode as different number types") {
        val productsResponse =
          """{"data":{"product":{"name":"Table","_caliban_gateway_key":1,"_caliban_gateway_typename":"Product"}}}"""

        for {
          products <- stub(productsResponse)
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, longKeyProductsSchema),
                          Subgraph.federation("stock", StockApi.api)
                        )
                        .interpreter
          response <- runtime.execute("{ product { name stock } }")
          product   = field(response.data, "product")
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "name")).contains(StringValue("Table")),
          product.flatMap(field(_, "stock")).contains(IntNumber(7))
        )
      },
      test("rejects Federation entity transport registered as ordinary GraphQL") {
        for {
          local  <- compositionDiagnostics(Gateway.compose(Subgraph.graphql("pricing", PricingApi.api)))
          remote <- compositionDiagnostics(
                      Gateway.compose(Subgraph.graphql("products", unreachableEndpoint, productsFederationSchema))
                    )
        } yield assertTrue(
          local.exists(message => message.startsWith("[pricing]") && message.contains("Subgraph.federation")),
          remote.exists(message => message.startsWith("[products]") && message.contains("Subgraph.federation"))
        )
      },
      test("preserves local entity failures while retaining independent remote data") {
        val pricing = new Pricing {
          def currency: UIO[String]       = ZIO.succeed("USD")
          def price(id: String): UIO[Int] = ZIO.succeed(0)
        }

        for {
          products <- stub(statusProductResponse)
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, statusProductsSchema),
                          Subgraph.federation("pricing", PricingApi.failingApi)
                        )
                        .interpreter
          response <- runtime
                        .execute("{ status product(id: \"p1\") { name price } }")
                        .provideEnvironment(ZEnvironment(pricing))
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "status").contains(StringValue("available")),
          field(response.data, "product").contains(NullValue),
          errors.map(_.msg) == List("local pricing unavailable"),
          errors.map(_.path) == List(List(PathValue.Key("product"))),
          errors.map(_.extensions) == List(Some(pricingDownExtensions)),
          errors.forall(_.msg != "Remote GraphQL request failed."),
          errors.forall(!_.msg.startsWith("Entity lookup response"))
        )
      },
      test("executes an entity join, keeps private aliases apart and hides transport types from introspection") {
        val aliasedProductResponse   =
          """{"data":{"product":{"productId":"p1","__typename":"Product","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val collidingProductResponse =
          """{"data":{"product":{"id":"Table","__typename":"Table","_caliban_gateway_key":"Table","_caliban_gateway_typename":"Table","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product"}}}"""
        val query                    =
          """query Product {
            |  product(id: "p1") {
            |    name
            |    reviews { body }
            |  }
            |}""".stripMargin
        val conditionalQuery         =
          """query Product($includeReviews: Boolean!) {
            |  product(id: "p1") {
            |    name
            |    reviews @include(if: $includeReviews) { body }
            |  }
            |}""".stripMargin

        for {
          products        <- stub(tableProductResponse, aliasedProductResponse, collidingProductResponse)
          reviews         <- stub(solidReviewsResponse)
          runtime         <- productsAndReviews(products, reviews).interpreter
          explanation     <- runtime.explain(query, Some("Product"))
          withoutReviews  <- runtime.explain(
                               GraphQLRequest(
                                 query = Some(conditionalQuery),
                                 operationName = Some("Product"),
                                 variables = Some(Map("includeReviews" -> BooleanValue(false)))
                               )
                             )
          withReviews     <- runtime.explain(
                               GraphQLRequest(
                                 query = Some(conditionalQuery),
                                 operationName = Some("Product"),
                                 variables = Some(Map("includeReviews" -> BooleanValue(true)))
                               )
                             )
          response        <- runtime.execute(query, Some("Product"))
          explicit        <- runtime.execute("{ product(id: \"p1\") { productId: id __typename reviews { body } } }")
          colliding       <-
            runtime.execute(
              "{ product(id: \"p1\") { id: name __typename: name _caliban_gateway_key: name _caliban_gateway_typename: name reviews { body } } }"
            )
          introspection   <- runtime.execute(
                               """{
                               |  query: __type(name: "Query") { fields { name } }
                               |  transport: __type(name: "_Service") { name }
                               |  linkPurpose: __type(name: "link__Purpose") { name }
                               |  schema: __schema { directives { name } }
                               |}""".stripMargin
                             )
          productsSent    <- products.requests.get
          reviewsSent     <- reviews.requests.get
          productsValid   <- ZIO.foreach(productsSent)(validateRequest(productsFederationSchema, _).exit)
          reviewsValid    <- ZIO.foreach(reviewsSent)(validateRequest(reviewsFederationSchema, _).exit)
          product          = field(response.data, "product")
          explicitProduct  = field(explicit.data, "product")
          collidingProduct = field(colliding.data, "product")
          queryFields      = introspectedNameStrings(field(introspection.data, "query").flatMap(field(_, "fields")))
          directives       =
            introspectedNameStrings(field(introspection.data, "schema").flatMap(field(_, "directives")))
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "name")).contains(StringValue("Table")),
          onlyNested(product, "reviews").exists(_.contains("body" -> StringValue("Solid"))),
          product.flatMap(field(_, "id")).isEmpty,
          product.flatMap(field(_, "__typename")).isEmpty,
          explicitProduct.flatMap(field(_, "productId")).contains(StringValue("p1")),
          explicitProduct.flatMap(field(_, "id")).isEmpty,
          explicitProduct.flatMap(field(_, "__typename")).contains(StringValue("Product")),
          queryFields.contains(List("product")),
          field(introspection.data, "transport").contains(NullValue),
          field(introspection.data, "linkPurpose").contains(NullValue),
          directives.exists(names => !names.contains("link") && !names.contains("key")),
          colliding.errors.isEmpty,
          collidingProduct.flatMap(field(_, "id")).contains(StringValue("Table")),
          collidingProduct.flatMap(field(_, "__typename")).contains(StringValue("Table")),
          collidingProduct.flatMap(field(_, "_caliban_gateway_key")).contains(StringValue("Table")),
          collidingProduct.flatMap(field(_, "_caliban_gateway_typename")).contains(StringValue("Table")),
          collidingProduct.flatMap(field(_, "_caliban_gateway_key_2")).isEmpty,
          collidingProduct.flatMap(field(_, "_caliban_gateway_typename_2")).isEmpty,
          productsSent.size == 3,
          reviewsSent.size == 3,
          productsValid.forall(_.isSuccess),
          reviewsValid.forall(_.isSuccess),
          productsSent.head.query.exists(rendered =>
            rendered.contains("product(id:\"p1\")") &&
              rendered.contains("name") && rendered.contains("_caliban_gateway_key:id") &&
              !rendered.contains("_caliban_gateway_typename") &&
              !rendered.contains("reviews")
          ),
          reviewsSent.head.query.exists(rendered =>
            rendered.contains("_entities") && rendered.contains("...on Product") &&
              rendered.contains("reviews{body}")
          ),
          representations(reviewsSent.head) == List(representation("Product", "id" -> StringValue("p1"))),
          explanation ==
            """query
              |fetch products at $.product fields [name, id (key)]
              |fetch reviews after products at $.product via Product(id) fields [reviews.body]""".stripMargin,
          !withoutReviews.contains("fetch reviews"),
          withReviews.contains("fetch reviews")
        )
      },
      test("deduplicates identical entity lookups across concurrent requests") {
        val callers = 2
        val query   = "{ product(id: \"p1\") { name reviews { body } } }"

        for {
          ready        <- Promise.make[Nothing, Unit]
          headerRuns   <- Ref.make(0)
          reviewsConfig = RemoteGraphQLConfig.default
                            .withExecutionHeadersZIO(headersAfterEveryCaller(callers, headerRuns, ready))
          products     <- stub(tableProductResponse)
          reviews      <- stub(solidReviewsResponse)
          runtime      <- Gateway
                            .compose(
                              Subgraph.federation("products", products.endpoint, productsFederationSchema),
                              Subgraph.federation(
                                "reviews",
                                reviews.endpoint,
                                reviewsFederationSchema,
                                reviewsConfig
                              )
                            )
                            .interpreter
          fibers       <- ZIO.foreach(1 to callers)(_ => runtime.execute(query).fork)
          responses    <- ZIO.foreach(fibers)(_.join)
          sent         <- reviews.requests.get
          totalHeaders <- headerRuns.get
        } yield assertTrue(
          responses.forall(_.errors.isEmpty),
          sent.size == 1,
          totalHeaders == callers
        )
      },
      test("skips an entity lookup when the nullable parent is null") {
        for {
          products     <- stub("""{"data":{"product":null}}""")
          reviews      <- stub("""{"data":{"_entities":[]}}""")
          runtime      <- productsAndReviews(products, reviews).interpreter
          response     <- runtime.execute("{ product(id: \"missing\") { reviews { body } } }")
          productsSent <- products.requests.get
          reviewsSent  <- reviews.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").contains(NullValue),
          productsSent.size == 1,
          reviewsSent.isEmpty
        )
      },
      test("propagates a nested entity null to the nearest nullable boundary") {
        val reviewResponse =
          """{"data":{"_entities":[{"reviews":[{"body":null}]}]},"errors":[{"message":"review body unavailable","path":["_entities",0,"reviews",0,"body"],"locations":[{"line":1,"column":2}],"extensions":{"code":"REVIEW_DOWN"}}]}"""

        for {
          products <- stub(statusProductResponse)
          reviews  <- stub(reviewResponse)
          runtime  <- productsAndReviews(products, reviews, statusProductsSchema)
                        .withConfig(_.withRemoteErrorMessages(true))
                        .interpreter
          response <- runtime.execute("{ status product(id: \"p1\") { name reviews { body } } }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "status").contains(StringValue("available")),
          field(response.data, "product").contains(NullValue),
          errors.map(_.msg) == List("review body unavailable"),
          errors.map(_.path) == List(
            List(
              PathValue.Key("product"),
              PathValue.Key("reviews"),
              PathValue.Index(0),
              PathValue.Key("body")
            )
          ),
          errors.forall(_.locationInfo.isEmpty),
          errors.flatMap(_.extensions).exists(_.fields.contains("code" -> StringValue("REVIEW_DOWN")))
        )
      },
      test("creates a non-null violation when a source returns null without an error") {
        val reviewResponse =
          """{"data":{"_entities":[{"reviews":[{"body":null}]}]}}"""

        for {
          products <- stub(tableProductResponse)
          reviews  <- stub(reviewResponse)
          runtime  <- productsAndReviews(products, reviews).interpreter
          response <- runtime.execute("{ product(id: \"p1\") { name reviews { body } } }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "product").contains(NullValue),
          errors.map(_.msg) == List("Cannot return null for non-nullable field Review.body."),
          errors.map(_.path) == List(
            List(
              PathValue.Key("product"),
              PathValue.Key("reviews"),
              PathValue.Index(0),
              PathValue.Key("body")
            )
          )
        )
      },
      test("preserves independent data when an entity transport fails") {
        for {
          products <- stub(statusProductResponse)
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, statusProductsSchema),
                          Subgraph.federation("reviews", unreachableEndpoint, reviewsFederationSchema)
                        )
                        .interpreter
          response <- runtime.execute("{ status product(id: \"p1\") { name reviews { body } } }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "status").contains(StringValue("available")),
          field(response.data, "product").contains(NullValue),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("product")))
        )
      },
      test("attaches unusable entity error paths safely at the merge location") {
        val reviewResponse   =
          """{"data":{"_entities":[{"reviews":[{"body":"Solid"}]}]},"errors":[{"message":"internal source detail","path":["_entities","unknown"]}]}"""
        val indexedResponse  =
          """{"data":{"_entities":[{"reviews":[{"body":"Solid"}]}]},"errors":[{"message":"misdirected source detail","path":["_entities",999,"reviews",0,"body"]}]}"""
        val negativeResponse =
          """{"data":{"_entities":[{"reviews":[{"body":"Solid"}]}]},"errors":[{"message":"negative source detail","path":["_entities",0,"reviews",-1,"body"]}]}"""

        for {
          products      <- stub(tableProductResponse)
          reviews       <- stub(reviewResponse, indexedResponse, negativeResponse)
          runtime       <- productsAndReviews(products, reviews).interpreter
          response      <- runtime.execute("{ product(id: \"p1\") { name reviews { body } } }")
          indexed       <- runtime.execute("{ product(id: \"p1\") { name reviews { body } } }")
          negative      <- runtime.execute("{ product(id: \"p1\") { name reviews { body } } }")
          errors         = executionErrors(response.errors)
          indexedErrors  = executionErrors(indexed.errors)
          negativeErrors = executionErrors(negative.errors)
        } yield assertTrue(
          field(response.data, "product").flatMap(field(_, "name")).contains(StringValue("Table")),
          onlyNested(field(response.data, "product"), "reviews")
            .exists(_.contains("body" -> StringValue("Solid"))),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("product"))),
          indexedErrors.map(_.msg) == List("Remote GraphQL request failed."),
          indexedErrors.map(_.path) == List(List(PathValue.Key("product"))),
          negativeErrors.map(_.msg) == List("Remote GraphQL request failed."),
          negativeErrors.map(_.path) == List(List(PathValue.Key("product")))
        )
      }
    ),
    suite("batching and correlation")(
      test("batches and correlates list-valued entity joins") {
        val productResponse =
          """{"data":{"products":[{"name":"First","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},{"name":"Second","_caliban_gateway_key":"p2","_caliban_gateway_typename":"Product"},{"name":"First again","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}]}}"""
        val reviewResponse  =
          """{"data":{"_entities":[{"reviews":[{"body":"First review"}]},{"reviews":[{"body":"Second review"}]}]}}"""

        for {
          products      <- stub(productResponse)
          reviews       <- stub(reviewResponse)
          runtime       <- productsAndReviews(products, reviews, listProductsFederationSchema).interpreter
          response      <- runtime.execute("{ products { name reviews { body } } }")
          productsSent  <- products.requests.get
          reviewsSent   <- reviews.requests.get
          productsValid <- ZIO.foreach(productsSent)(validateRequest(listProductsFederationSchema, _).exit)
          reviewsValid  <- ZIO.foreach(reviewsSent)(validateRequest(reviewsFederationSchema, _).exit)
          values         = listValues(field(response.data, "products"))
        } yield assertTrue(
          response.errors.isEmpty,
          values.flatMap(field(_, "name")) == List(
            StringValue("First"),
            StringValue("Second"),
            StringValue("First again")
          ),
          values.flatMap(value =>
            onlyNested(Some(value), "reviews").flatMap(
              _.collectFirst { case ("body", StringValue(body)) => body }
            )
          ) == List("First review", "Second review", "First review"),
          values.forall(value =>
            field(value, "id").isEmpty &&
              field(value, "__typename").isEmpty &&
              field(value, "_caliban_gateway_key").isEmpty &&
              field(value, "_caliban_gateway_typename").isEmpty
          ),
          productsSent.size == 1,
          reviewsSent.size == 1,
          productsValid.forall(_.isSuccess),
          reviewsValid.forall(_.isSuccess),
          representations(reviewsSent.head) == List(
            representation("Product", "id" -> StringValue("p1")),
            representation("Product", "id" -> StringValue("p2"))
          )
        )
      },
      test("reports surplus federation results") {
        val productResponse =
          """{"data":{"products":[{"name":"First","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},{"name":"Second","_caliban_gateway_key":"p2","_caliban_gateway_typename":"Product"}]}}"""
        val reviewResponse  =
          """{"data":{"_entities":[{"reviews":[{"body":"First review"}]},{"reviews":[{"body":"Second review"}]},null]}}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          runtime  <-
            productsAndReviews(products, reviews, listProductsFederationSchema, nullableReviewsSchema).interpreter
          response <- runtime.execute("{ products { name reviews { body } } }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          errors.map(_.msg) == List("Entity lookup response contained an unexpected result for 'Product(id)'.")
        )
      },
      test("deduplicates compatible entity routes across the operation") {
        val orderedReviews = reviewsFederationSchema.replace(
          "type Review { body: String! }",
          "type Review { body: String! rating: Int! }"
        )
        val reviewResponse =
          """{"data":{"_entities":[{"reviews":[{"body":"Shared","rating":5}]}]}}"""

        for {
          products <- stub(repeatedProductsResponse)
          reviews  <- stub(reviewResponse)
          runtime  <- productsAndReviews(products, reviews, listProductsFederationSchema, orderedReviews).interpreter
          response <- runtime.execute(
                        "{ first: products { reviews { body rating } } second: products { reviews { rating body } } }"
                      )
          sent     <- reviews.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          firstNestedObject(response.data, "first", "reviews").exists(_.contains("body" -> StringValue("Shared"))),
          firstNestedObject(response.data, "second", "reviews").exists(_.contains("body" -> StringValue("Shared"))),
          sent.size == 1,
          representations(sent.head) == List(representation("Product", "id" -> StringValue("p1")))
        )
      },
      test("attributes combined entity call errors to their own group") {
        val argumentReviews = limitedReviewsSchema
          .replace("type Review { body: String! }", "type Review { body: String }")
        val entity          =
          """{"reviews":[{"body":"First"}]}"""
        val failedEntity    =
          """{"reviews":[{"body":null}]}"""
        val failedResponse  =
          s"""{"data":{"_entities":[$failedEntity]},"errors":[{"message":"Boom","path":["_entities",0,"reviews",0,"body"]}]}"""

        for {
          products <- stub(repeatedProductsResponse)
          reviews  <- stubByRequest { request =>
                        if (request.query.exists(_.contains("reviews(limit:2)"))) failedResponse
                        else s"""{"data":{"_entities":[$entity]}}"""
                      }
          runtime  <- productsAndReviews(products, reviews, listProductsFederationSchema, argumentReviews).interpreter
          response <-
            runtime.execute(
              """{
                |  first: products { reviews(limit: 1) { body } }
                |  second: products { reviews(limit: 2) { body } }
                |}""".stripMargin
            )
          combined <- reviews.combined.get
        } yield assertTrue(
          combined.size == 1,
          response.errors.collect { case error: CalibanError.ExecutionError => error.path } == List(
            List(
              PathValue.Key("second"),
              PathValue.Index(0),
              PathValue.Key("reviews"),
              PathValue.Index(0),
              PathValue.Key("body")
            )
          ),
          firstNestedObject(response.data, "first", "reviews").exists(_.contains("body" -> StringValue("First"))),
          firstNestedObject(response.data, "second", "reviews").exists(_.contains("body" -> NullValue))
        )
      },
      test("sends separate entity calls when the combined request exceeds the size limit") {
        val entity =
          """{"reviews":[{"body":"Body"}]}"""
        val config = RemoteGraphQLConfig.default.withExecution(_.withMaxRequestBytes(512))

        for {
          products <- stub(repeatedProductsResponse)
          reviews  <- stub(s"""{"data":{"_entities":[$entity]}}""")
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, listProductsFederationSchema),
                          Subgraph.federation("reviews", reviews.endpoint, limitedReviewsSchema, config)
                        )
                        .interpreter
          response <-
            runtime.execute(
              """{
                |  first: products { reviews(limit: 1) { body } }
                |  second: products { reviews(limit: 2) { body } }
                |}""".stripMargin
            )
          sent     <- reviews.requests.get
          combined <- reviews.combined.get
        } yield assertTrue(
          response.errors.isEmpty,
          sent.size == 2,
          combined.isEmpty,
          firstNestedObject(response.data, "first", "reviews").exists(_.contains("body" -> StringValue("Body"))),
          firstNestedObject(response.data, "second", "reviews").exists(_.contains("body" -> StringValue("Body")))
        )
      },
      test("keeps incompatible entity routes in separate groups") {
        val argumentReviews                      = limitedReviewsSchema
          .replace("type Review { body: String! }", "type Review { body: String! rating: Int! }")
        val productResponse                      =
          """{"data":{"first":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}],"second":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}],"third":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}],"fourth":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}]}}"""
        val firstEntity                          =
          """{"reviews":[{"body":"First"}]}"""
        val secondEntity                         =
          """{"reviews":[{"body":"Second"}]}"""
        val aliasedEntity                        =
          """{"feedback":[{"body":"Aliased"}]}"""
        val shapedEntity                         =
          """{"reviews":[{"body":"Shaped","rating":5}]}"""
        def entityFor(selection: String): String =
          if (selection.contains("reviews(limit:2)")) secondEntity
          else if (selection.contains("feedback:reviews")) aliasedEntity
          else if (selection.contains("{body rating}")) shapedEntity
          else firstEntity

        for {
          products <- stub(productResponse)
          reviews  <- stubByRequest(request => s"""{"data":{"_entities":[${entityFor(request.query.getOrElse(""))}]}}""")
          runtime  <- productsAndReviews(products, reviews, listProductsFederationSchema, argumentReviews).interpreter
          response <-
            runtime.execute(
              """{
                |  first: products { reviews(limit: 1) { body } }
                |  second: products { reviews(limit: 2) { body } }
                |  third: products { feedback: reviews(limit: 1) { body } }
                |  fourth: products { reviews(limit: 1) { body rating } }
                |}""".stripMargin
            )
          sent     <- reviews.requests.get
          combined <- reviews.combined.get
          valid    <- ZIO.foreach(sent)(validateRequest(argumentReviews, _).exit)
        } yield assertTrue(
          response.errors.isEmpty,
          sent.size == 4,
          combined.size == 1,
          combined.flatMap(_.query).exists("_caliban_gateway_entities_".r.findAllIn(_).size == 4),
          valid.forall(_.isSuccess),
          sent.flatMap(_.query).exists(_.contains("reviews(limit:1)")),
          sent.flatMap(_.query).exists(_.contains("reviews(limit:2)")),
          sent.flatMap(_.query).exists(_.contains("feedback:reviews(limit:1){body}")),
          sent.flatMap(_.query).exists(_.contains("reviews(limit:1){body rating}")),
          firstNestedObject(response.data, "first", "reviews")
            .exists(_.contains("body" -> StringValue("First"))),
          firstNestedObject(response.data, "second", "reviews")
            .exists(_.contains("body" -> StringValue("Second"))),
          firstNestedObject(response.data, "third", "feedback")
            .exists(_.contains("body" -> StringValue("Aliased"))),
          firstNestedObject(response.data, "fourth", "reviews")
            .exists(review =>
              review.contains("body" -> StringValue("Shaped")) && review.contains("rating" -> IntNumber(5))
            )
        )
      },
      test("fans entity errors out to duplicate client locations") {
        val productResponse =
          """{"data":{"products":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}]}}"""
        val reviewResponse  =
          """{"data":{"_entities":[{"reviews":[{"_caliban_gateway_entity_key":null}]}]},"errors":[{"message":"review unavailable","path":["_entities",0,"reviews",0,"_caliban_gateway_entity_key"]}]}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          runtime  <- productsAndReviews(products, reviews, listProductsFederationSchema)
                        .withConfig(_.withRemoteErrorMessages(true))
                        .interpreter
          response <- runtime.execute("{ products { reviews { _caliban_gateway_entity_key: body } } }")
          sent     <- reviews.requests.get
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          errors.map(_.msg) == List("review unavailable", "review unavailable"),
          errors.map(_.path) == List(
            List(
              PathValue.Key("products"),
              PathValue.Index(0),
              PathValue.Key("reviews"),
              PathValue.Index(0),
              PathValue.Key("_caliban_gateway_entity_key")
            ),
            List(
              PathValue.Key("products"),
              PathValue.Index(1),
              PathValue.Key("reviews"),
              PathValue.Index(0),
              PathValue.Key("_caliban_gateway_entity_key")
            )
          ),
          sent.size == 1,
          representations(sent.head) == List(representation("Product", "id" -> StringValue("p1")))
        )
      },
      test("does not duplicate an unindexed lookup failure with missing-result errors") {
        val productResponse =
          """{"data":{"products":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},{"_caliban_gateway_key":"p2","_caliban_gateway_typename":"Product"}]}}"""
        val reviewResponse  =
          """{"data":null,"errors":[{"message":"reviews unavailable"}]}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          runtime  <- productsAndReviews(products, reviews, listProductsFederationSchema).interpreter
          response <- runtime.execute("{ products { reviews { body } } }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("products")))
        )
      },
      test("handles null and missing entity results by position") {
        val productResponse =
          """{"data":{"products":[{"name":"First","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},{"name":"Second","_caliban_gateway_key":"p2","_caliban_gateway_typename":"Product"},{"name":"Third","_caliban_gateway_key":"p3","_caliban_gateway_typename":"Product"}]}}"""
        val reviewResponse  =
          """{"data":{"_entities":[null,{"reviews":[{"body":"Second review"}]}]}}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          runtime  <-
            productsAndReviews(products, reviews, listProductsFederationSchema, nullableReviewsSchema).interpreter
          response <- runtime.execute("{ products { name reviews { body } } }")
          sent     <- reviews.requests.get
          values    = listValues(field(response.data, "products"))
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          values.flatMap(field(_, "name")) == List(StringValue("First"), StringValue("Second"), StringValue("Third")),
          values.headOption.flatMap(field(_, "reviews")).contains(NullValue),
          onlyNested(values.lift(1), "reviews").exists(_.contains("body" -> StringValue("Second review"))),
          values.lift(2).flatMap(field(_, "reviews")).contains(NullValue),
          errors.map(_.msg) == List("Entity lookup response omitted a result for 'Product(id)'."),
          errors.map(_.path) == List(List(PathValue.Key("products"), PathValue.Index(2))),
          sent.size == 1
        )
      },
      test("relocates an error on a null entity to the client entity path") {
        val productResponse =
          """{"data":{"products":[{"name":"First","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},{"name":"Second","_caliban_gateway_key":"p2","_caliban_gateway_typename":"Product"}]}}"""
        val reviewResponse  =
          """{"data":{"_entities":[null,{"reviews":[{"body":"Second review"}]}]},"errors":[{"message":"first unavailable","path":["_entities",0]}]}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          runtime  <-
            productsAndReviews(products, reviews, listProductsFederationSchema, nullableReviewsSchema)
              .withConfig(_.withRemoteErrorMessages(true))
              .interpreter
          response <- runtime.execute("{ products { name reviews { body } } }")
          values    = listValues(field(response.data, "products"))
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          values.flatMap(field(_, "name")) == List(StringValue("First"), StringValue("Second")),
          values.headOption.flatMap(field(_, "reviews")).contains(NullValue),
          onlyNested(values.lift(1), "reviews").exists(_.contains("body" -> StringValue("Second review"))),
          errors.map(_.msg) == List("first unavailable"),
          errors.map(_.path) == List(List(PathValue.Key("products"), PathValue.Index(0)))
        )
      },
      test("correlates duplicate entity keys with distinct requirement values by position") {
        val productsSchema  =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"])
             |$authoredFederationDirectives
             |type Query { products: [Product!]! }
             |type Product @key(fields: "id") { id: ID! price: Int! }
             |""".stripMargin
        val inventorySchema =
          s"""
             |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external", "@requires"])
             |$authoredFederationDirectives
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  price: Int! @external
             |  shippingEstimate: Int! @requires(fields: "price")
             |}
             |""".stripMargin
        val productResponse =
          """{"data":{"products":[{"_caliban_gateway_requirement_price":10,"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},{"_caliban_gateway_requirement_price":20,"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}]}}"""

        for {
          products  <- stub(productResponse)
          inventory <-
            stub(
              """{"data":{"_entities":[{"shippingEstimate":100},{"shippingEstimate":200}]},"errors":[{"message":"estimate warning","path":["_entities",1,"shippingEstimate"]}]}"""
            )
          runtime   <- Gateway
                         .compose(
                           Subgraph.federation("products", products.endpoint, productsSchema),
                           Subgraph.federation("inventory", inventory.endpoint, inventorySchema)
                         )
                         .withConfig(_.withRemoteErrorMessages(true))
                         .interpreter
          response  <- runtime.execute("{ products { shippingEstimate } }")
          sent      <- inventory.requests.get
          errors     = executionErrors(response.errors)
          values     = listValues(field(response.data, "products"))
        } yield assertTrue(
          errors.map(_.msg) == List("estimate warning"),
          errors.map(_.path) == List(
            List(PathValue.Key("products"), PathValue.Index(1), PathValue.Key("shippingEstimate"))
          ),
          values.flatMap(field(_, "shippingEstimate")) == List(IntNumber(100), IntNumber(200)),
          sent.size == 1,
          sent.headOption
            .map(representations)
            .contains(
              List(
                representation("Product", "id" -> StringValue("p1"), "price" -> IntNumber(10)),
                representation("Product", "id" -> StringValue("p1"), "price" -> IntNumber(20))
              )
            )
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

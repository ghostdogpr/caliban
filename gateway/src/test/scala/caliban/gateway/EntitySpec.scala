package caliban.gateway

import caliban.InputValue.{ ListValue, ObjectValue => InputObjectValue }
import caliban.ResponseValue.{ ListValue => ResponseListValue, ObjectValue => ResponseObjectValue }
import caliban.Value.IntValue.IntNumber
import caliban.Value.{ BooleanValue, NullValue, StringValue }
import caliban.federation.EntityResolver
import caliban.federation.v2_6.{ federated, GQLKey }
import caliban.gateway.GatewayTestSupport._
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
import caliban.{ graphQL, CalibanError, GraphQLRequest, PathValue, RootResolver }
import sttp.model.Uri
import zio._
import zio.query.ZQuery
import zio.test._

object EntitySpec extends ZIOSpecDefault {

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
            extensions = Some(ResponseObjectValue(List("code" -> StringValue("PRICING_DOWN"))))
          )
        )
        failure
      }
    )
  }

  private val federationDirectives =
    """
      |directive @link(url: String!, as: String, import: [link__Import], for: link__Purpose) repeatable on SCHEMA
      |directive @key(fields: federation__FieldSet!, resolvable: Boolean = true) repeatable on OBJECT | INTERFACE
      |directive @external on FIELD_DEFINITION
      |directive @shareable repeatable on OBJECT | FIELD_DEFINITION
      |scalar link__Import
      |enum link__Purpose { SECURITY EXECUTION }
      |scalar federation__FieldSet
      |scalar _Any
      |type _Service { sdl: String! }
      |""".stripMargin

  private val productsFederationSchema =
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"]) { query: Query }
       |$federationDirectives
       |union _Entity = Product
       |type Query {
       |  product(id: ID!): Product
       |  _entities(representations: [_Any!]!): [_Entity]!
       |  _service: _Service!
       |}
       |type Product @key(fields: "id") { id: ID! name: String! }
       |""".stripMargin

  private val reviewsFederationSchema =
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"]) { query: Query }
       |$federationDirectives
       |union _Entity = Product
       |type Query {
       |  _entities(representations: [_Any!]!): [_Entity]!
       |  _service: _Service!
       |}
       |type Product @key(fields: "id") { id: ID! @external reviews: [Review!]! }
       |type Review { body: String! }
       |""".stripMargin

  private val authoredFederationDirectives =
    """
      |directive @link(url: String!, as: String, import: [link__Import], for: link__Purpose) repeatable on SCHEMA
      |directive @key(fields: federation__FieldSet!, resolvable: Boolean = true) repeatable on OBJECT | INTERFACE
      |directive @external on FIELD_DEFINITION
      |directive @shareable repeatable on OBJECT | FIELD_DEFINITION
      |scalar link__Import
      |enum link__Purpose { SECURITY EXECUTION }
      |scalar federation__FieldSet
      |""".stripMargin

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

  def spec = suite("EntitySpec")(
    suite("entity execution")(
      test("executes remote Products, local Pricing, and remote Reviews in one operation") {
        val productsResponse =
          """{"data":{"product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product"}}}"""
        val reviewsResponse  =
          """{"data":{"_entities":[{"reviews":[{"body":"Solid"}],"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product"}]}}"""
        val pricing          = new Pricing {
          def currency: UIO[String]       = ZIO.succeed("USD")
          def price(id: String): UIO[Int] = ZIO.succeed(if (id == "p1") 125 else 0)
        }

        (for {
          products <- stub(productsResponse)
          reviews  <- stub(reviewsResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsFederationSchema),
                          Subgraph.local("pricing", PricingApi.api),
                          Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                        )
                        .build
          response <- gateway
                        .execute("{ product(id: \"p1\") { name price reviews { body } } currency }")
                        .provideEnvironment(ZEnvironment(pricing))
          product   = field(response.data, "product")
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "currency").contains(StringValue("USD")),
          product.flatMap(field(_, "name")).contains(StringValue("Table")),
          product.flatMap(field(_, "price")).contains(IntNumber(125)),
          product.flatMap(field(_, "reviews")).exists {
            case ResponseListValue(ResponseObjectValue(fields) :: Nil) =>
              fields.contains("body" -> StringValue("Solid"))
            case _                                                     => false
          }
        ))
      },
      test("preserves local entity failures while retaining independent remote data") {
        val productsSchema   = productsFederationSchema.replace(
          "  product(id: ID!): Product",
          "  product(id: ID!): Product\n  status: String!"
        )
        val productsResponse =
          """{"data":{"status":"available","product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val pricing          = new Pricing {
          def currency: UIO[String]       = ZIO.succeed("USD")
          def price(id: String): UIO[Int] = ZIO.succeed(0)
        }
        val extensions       = ResponseObjectValue(List("code" -> StringValue("PRICING_DOWN")))

        for {
          products <- stub(productsResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.local("pricing", PricingApi.failingApi)
                        )
                        .build
          response <- gateway
                        .execute("{ status product(id: \"p1\") { name price } }")
                        .provideEnvironment(ZEnvironment(pricing))
          errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
        } yield assertTrue(
          field(response.data, "status").contains(StringValue("available")),
          field(response.data, "product").contains(NullValue),
          errors.map(_.msg) == List("local pricing unavailable"),
          errors.map(_.path) == List(List(PathValue.Key("product"))),
          errors.map(_.extensions) == List(Some(extensions)),
          errors.forall(_.msg != "Remote GraphQL request failed."),
          errors.forall(!_.msg.startsWith("Entity lookup response"))
        )
      },
      test("executes one Federation entity join through the executable plan") {
        val productResponse          =
          """{"data":{"product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val aliasedProductResponse   =
          """{"data":{"product":{"productId":"p1","__typename":"Product","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val collidingProductResponse =
          """{"data":{"product":{"id":"Table","__typename":"Table","_caliban_gateway_key":"Table","_caliban_gateway_typename":"Table","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product"}}}"""
        val reviewResponse           =
          """{"data":{"_entities":[{"reviews":[{"body":"Solid"}],"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product"}]}}"""
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
          products        <- stub(productResponse, aliasedProductResponse, collidingProductResponse)
          reviews         <- stub(reviewResponse)
          gateway         <- Gateway
                               .compose(
                                 Subgraph.federation("products", products.endpoint, productsFederationSchema),
                                 Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                               )
                               .build
          explanation     <- gateway.explain(query, Some("Product"))
          withoutReviews  <- gateway.explain(
                               GraphQLRequest(
                                 query = Some(conditionalQuery),
                                 operationName = Some("Product"),
                                 variables = Some(Map("includeReviews" -> BooleanValue(false)))
                               )
                             )
          withReviews     <- gateway.explain(
                               GraphQLRequest(
                                 query = Some(conditionalQuery),
                                 operationName = Some("Product"),
                                 variables = Some(Map("includeReviews" -> BooleanValue(true)))
                               )
                             )
          response        <- gateway.execute(query, Some("Product"))
          explicit        <- gateway.execute("{ product(id: \"p1\") { productId: id __typename reviews { body } } }")
          colliding       <-
            gateway.execute(
              "{ product(id: \"p1\") { id: name __typename: name _caliban_gateway_key: name _caliban_gateway_typename: name reviews { body } } }"
            )
          introspection   <- gateway.execute(
                               """{
                               |  query: __type(name: "Query") { fields { name } }
                               |  transport: __type(name: "_Service") { name }
                               |  linkPurpose: __type(name: "link__Purpose") { name }
                               |  schema: __schema { directives { name } }
                               |}""".stripMargin
                             )
          productSent     <- products.requests.get
          reviewSent      <- reviews.requests.get
          productValid    <- ZIO.foreach(productSent)(validateRequest(productsFederationSchema, _).exit)
          reviewValid     <- ZIO.foreach(reviewSent)(validateRequest(reviewsFederationSchema, _).exit)
          product          = field(response.data, "product")
          explicitProduct  = field(explicit.data, "product")
          collidingProduct = field(colliding.data, "product")
          queryFields      = field(introspection.data, "query")
                               .flatMap(field(_, "fields"))
                               .collect { case ResponseListValue(values) =>
                                 values.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                               }
          directives       = field(introspection.data, "schema")
                               .flatMap(field(_, "directives"))
                               .collect { case ResponseListValue(values) =>
                                 values.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                               }
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "name")).contains(StringValue("Table")),
          product.flatMap(field(_, "reviews")).exists {
            case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
              review.contains("body" -> StringValue("Solid"))
            case _                                                     => false
          },
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
          productSent.size == 3,
          reviewSent.size == 3,
          productValid.forall(_.isSuccess),
          reviewValid.forall(_.isSuccess),
          productSent.head.query.exists(rendered =>
            rendered.contains("product(id:\"p1\")") &&
              rendered.contains("name") && rendered.contains("_caliban_gateway_key:id") &&
              rendered.contains("_caliban_gateway_typename:__typename") &&
              !rendered.contains("reviews")
          ),
          reviewSent.head.query.exists(rendered =>
            rendered.contains("_entities") && rendered.contains("...on Product") &&
              rendered.contains("reviews{body}") &&
              rendered.contains("_caliban_gateway_entity_key:id") &&
              rendered.contains("_caliban_gateway_entity_typename:__typename")
          ),
          reviewSent.head.variables.contains(
            Map(
              "representations" -> ListValue(
                List(InputObjectValue(Map("__typename" -> StringValue("Product"), "id" -> StringValue("p1"))))
              )
            )
          ),
          explanation ==
            """query
              |fetch products at $.product fields [name, id (key), __typename (key)]
              |fetch reviews after products at $.product via Product(id) fields [reviews.body]""".stripMargin,
          !withoutReviews.contains("fetch reviews"),
          withReviews.contains("fetch reviews")
        )
      },
      test("skips an entity lookup when the nullable parent is null") {
        for {
          products <- stub("""{"data":{"product":null}}""")
          reviews  <- stub("""{"data":{"_entities":[]}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsFederationSchema),
                          Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                        )
                        .build
          response <- gateway.execute("{ product(id: \"missing\") { reviews { body } } }")
          sentA    <- products.requests.get
          sentB    <- reviews.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").contains(NullValue),
          sentA.size == 1,
          sentB.isEmpty
        )
      },
      test("propagates a nested entity null to the nearest nullable boundary") {
        val productsSchema  = productsFederationSchema.replace(
          "  product(id: ID!): Product",
          "  product(id: ID!): Product\n  status: String!"
        )
        val productResponse =
          """{"data":{"status":"available","product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val reviewResponse  =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"body":null}]}]},"errors":[{"message":"review body unavailable","path":["_entities",0,"reviews",0,"body"],"locations":[{"line":1,"column":2}],"extensions":{"code":"REVIEW_DOWN"}}]}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                        )
                        .build
          response <- gateway.execute("{ status product(id: \"p1\") { name reviews { body } } }")
          errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
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
        val productResponse =
          """{"data":{"product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val reviewResponse  =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"body":null}]}]}}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsFederationSchema),
                          Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                        )
                        .build
          response <- gateway.execute("{ product(id: \"p1\") { name reviews { body } } }")
          errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
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
        val productsSchema  = productsFederationSchema.replace(
          "  product(id: ID!): Product",
          "  product(id: ID!): Product\n  status: String!"
        )
        val productResponse =
          """{"data":{"status":"available","product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val unavailable     = Uri.unsafeParse("http://127.0.0.1:1/graphql")

        for {
          products <- stub(productResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("reviews", unavailable, reviewsFederationSchema)
                        )
                        .build
          response <- gateway.execute("{ status product(id: \"p1\") { name reviews { body } } }")
          errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
        } yield assertTrue(
          field(response.data, "status").contains(StringValue("available")),
          field(response.data, "product").contains(NullValue),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("product")))
        )
      },
      test("attaches unusable entity error paths safely at the merge location") {
        val productResponse  =
          """{"data":{"product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val reviewResponse   =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"Solid"}]}]},"errors":[{"message":"internal source detail","path":["_entities","unknown"]}]}"""
        val indexedResponse  =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"Solid"}]}]},"errors":[{"message":"misdirected source detail","path":["_entities",999,"reviews",0,"body"]}]}"""
        val negativeResponse =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"Solid"}]}]},"errors":[{"message":"negative source detail","path":["_entities",0,"reviews",-1,"body"]}]}"""

        for {
          products      <- stub(productResponse)
          reviews       <- stub(reviewResponse, indexedResponse, negativeResponse)
          gateway       <- Gateway
                             .compose(
                               Subgraph.federation("products", products.endpoint, productsFederationSchema),
                               Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                             )
                             .build
          response      <- gateway.execute("{ product(id: \"p1\") { name reviews { body } } }")
          indexed       <- gateway.execute("{ product(id: \"p1\") { name reviews { body } } }")
          negative      <- gateway.execute("{ product(id: \"p1\") { name reviews { body } } }")
          errors         = response.errors.collect { case error: CalibanError.ExecutionError => error }
          indexedErrors  = indexed.errors.collect { case error: CalibanError.ExecutionError => error }
          negativeErrors = negative.errors.collect { case error: CalibanError.ExecutionError => error }
        } yield assertTrue(
          field(response.data, "product").flatMap(field(_, "name")).contains(StringValue("Table")),
          field(response.data, "product")
            .flatMap(field(_, "reviews"))
            .exists {
              case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
                review.contains("body" -> StringValue("Solid"))
              case _                                                     => false
            },
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
        val listProducts    = productsFederationSchema.replace("product(id: ID!): Product", "products: [Product!]!")
        val productResponse =
          """{"data":{"products":[{"name":"First","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},{"name":"Second","_caliban_gateway_key":"p2","_caliban_gateway_typename":"Product"},{"name":"First again","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}]}}"""
        val reviewResponse  =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p2","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"Second review"}]},{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"First review"}]}]}}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, listProducts),
                          Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                        )
                        .build
          response <- gateway.execute("{ products { name reviews { body } } }")
          sentA    <- products.requests.get
          sentB    <- reviews.requests.get
          validA   <- ZIO.foreach(sentA)(validateRequest(listProducts, _).exit)
          validB   <- ZIO.foreach(sentB)(validateRequest(reviewsFederationSchema, _).exit)
          values    = field(response.data, "products").collect { case ResponseListValue(values) => values }.getOrElse(Nil)
        } yield assertTrue(
          response.errors.isEmpty,
          values.flatMap(field(_, "name")) == List(
            StringValue("First"),
            StringValue("Second"),
            StringValue("First again")
          ),
          values
            .flatMap(field(_, "reviews"))
            .collect { case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
              review.collectFirst { case ("body", StringValue(body)) => body }
            }
            .flatten == List("First review", "Second review", "First review"),
          values.forall(value =>
            field(value, "id").isEmpty &&
              field(value, "__typename").isEmpty &&
              field(value, "_caliban_gateway_key").isEmpty &&
              field(value, "_caliban_gateway_typename").isEmpty
          ),
          sentA.size == 1,
          sentB.size == 1,
          validA.forall(_.isSuccess),
          validB.forall(_.isSuccess),
          sentB.head.variables.contains(
            Map(
              "representations" -> ListValue(
                List(
                  InputObjectValue(Map("__typename" -> StringValue("Product"), "id" -> StringValue("p1"))),
                  InputObjectValue(Map("__typename" -> StringValue("Product"), "id" -> StringValue("p2")))
                )
              )
            )
          ),
          sentB.head.query.exists(rendered =>
            rendered.contains("_caliban_gateway_entity_key:id") &&
              rendered.contains("_caliban_gateway_entity_typename:__typename")
          )
        )
      },
      test("deduplicates compatible entity routes across the operation") {
        val listProducts    = productsFederationSchema.replace("product(id: ID!): Product", "products: [Product!]!")
        val orderedReviews  = reviewsFederationSchema.replace(
          "type Review { body: String! }",
          "type Review { body: String! rating: Int! }"
        )
        val productResponse =
          """{"data":{"first":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}],"second":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}]}}"""
        val reviewResponse  =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"Shared","rating":5}]}]}}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, listProducts),
                          Subgraph.federation("reviews", reviews.endpoint, orderedReviews)
                        )
                        .build
          response <- gateway.execute(
                        "{ first: products { reviews { body rating } } second: products { reviews { rating body } } }"
                      )
          sentB    <- reviews.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "first").exists {
            case ResponseListValue(ResponseObjectValue(product) :: Nil) =>
              product.collectFirst { case ("reviews", value) => value }.exists {
                case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
                  review.contains("body" -> StringValue("Shared"))
                case _                                                     => false
              }
            case _                                                      => false
          },
          field(response.data, "second").exists {
            case ResponseListValue(ResponseObjectValue(product) :: Nil) =>
              product.collectFirst { case ("reviews", value) => value }.exists {
                case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
                  review.contains("body" -> StringValue("Shared"))
                case _                                                     => false
              }
            case _                                                      => false
          },
          sentB.size == 1,
          sentB.head.variables.contains(
            Map(
              "representations" -> ListValue(
                List(InputObjectValue(Map("__typename" -> StringValue("Product"), "id" -> StringValue("p1"))))
              )
            )
          )
        )
      },
      test("keeps incompatible entity routes in separate groups") {
        val listProducts    = productsFederationSchema.replace("product(id: ID!): Product", "products: [Product!]!")
        val argumentReviews = reviewsFederationSchema
          .replace(
            "reviews: [Review!]!",
            "reviews(limit: Int!): [Review!]!"
          )
          .replace("type Review { body: String! }", "type Review { body: String! rating: Int! }")
        val productResponse =
          """{"data":{"first":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}],"second":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}],"third":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}],"fourth":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}]}}"""
        val firstResponse   =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"First"}]}]}}"""
        val secondResponse  =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"Second"}]}]}}"""
        val aliasedResponse =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","feedback":[{"body":"Aliased"}]}]}}"""
        val shapedResponse  =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"Shaped","rating":5}]}]}}"""

        for {
          products <- stub(productResponse)
          reviews  <- stubByRequest { request =>
                        val query = request.query.getOrElse("")
                        if (query.contains("reviews(limit:2)")) secondResponse
                        else if (query.contains("feedback:reviews")) aliasedResponse
                        else if (query.contains("{body rating}")) shapedResponse
                        else firstResponse
                      }
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, listProducts),
                          Subgraph.federation("reviews", reviews.endpoint, argumentReviews)
                        )
                        .build
          response <-
            gateway.execute(
              """{
                |  first: products { reviews(limit: 1) { body } }
                |  second: products { reviews(limit: 2) { body } }
                |  third: products { feedback: reviews(limit: 1) { body } }
                |  fourth: products { reviews(limit: 1) { body rating } }
                |}""".stripMargin
            )
          sentB    <- reviews.requests.get
          validB   <- ZIO.foreach(sentB)(validateRequest(argumentReviews, _).exit)
        } yield assertTrue(
          response.errors.isEmpty,
          sentB.size == 4,
          validB.forall(_.isSuccess),
          sentB.flatMap(_.query).exists(_.contains("reviews(limit:1)")),
          sentB.flatMap(_.query).exists(_.contains("reviews(limit:2)")),
          sentB.flatMap(_.query).exists(_.contains("feedback:reviews(limit:1){body}")),
          sentB.flatMap(_.query).exists(_.contains("reviews(limit:1){body rating}")),
          field(response.data, "first").collect { case ResponseListValue(ResponseObjectValue(product) :: Nil) =>
            product
          }
            .flatMap(_.collectFirst { case ("reviews", ResponseListValue(ResponseObjectValue(review) :: Nil)) =>
              review
            })
            .exists(_.contains("body" -> StringValue("First"))),
          field(response.data, "second").collect { case ResponseListValue(ResponseObjectValue(product) :: Nil) =>
            product
          }
            .flatMap(_.collectFirst { case ("reviews", ResponseListValue(ResponseObjectValue(review) :: Nil)) =>
              review
            })
            .exists(_.contains("body" -> StringValue("Second"))),
          field(response.data, "third").collect { case ResponseListValue(ResponseObjectValue(product) :: Nil) =>
            product
          }
            .flatMap(_.collectFirst { case ("feedback", ResponseListValue(ResponseObjectValue(review) :: Nil)) =>
              review
            })
            .exists(_.contains("body" -> StringValue("Aliased"))),
          field(response.data, "fourth").collect { case ResponseListValue(ResponseObjectValue(product) :: Nil) =>
            product
          }
            .flatMap(_.collectFirst { case ("reviews", ResponseListValue(ResponseObjectValue(review) :: Nil)) =>
              review
            })
            .exists(review =>
              review.contains("body" -> StringValue("Shaped")) && review.contains("rating" -> IntNumber(5))
            )
        )
      },
      test("fans entity errors out to duplicate client locations") {
        val listProducts    = productsFederationSchema.replace("product(id: ID!): Product", "products: [Product!]!")
        val productResponse =
          """{"data":{"products":[{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}]}}"""
        val reviewResponse  =
          """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product","reviews":[{"_caliban_gateway_entity_key":null}]}]},"errors":[{"message":"review unavailable","path":["_entities",0,"reviews",0,"_caliban_gateway_entity_key"]}]}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, listProducts),
                          Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                        )
                        .build
          response <- gateway.execute("{ products { reviews { _caliban_gateway_entity_key: body } } }")
          sentB    <- reviews.requests.get
          errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
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
          sentB.size == 1,
          sentB.head.variables.exists { case variables =>
            variables
              .get("representations")
              .contains(
                ListValue(
                  List(InputObjectValue(Map("__typename" -> StringValue("Product"), "id" -> StringValue("p1"))))
                )
              )
          }
        )
      },
      test("handles null, missing, extra, and duplicate entity results deterministically") {
        val listProducts    = productsFederationSchema.replace("product(id: ID!): Product", "products: [Product!]!")
        val nullableReviews = reviewsFederationSchema.replace("reviews: [Review!]!", "reviews: [Review!]")
        val productResponse =
          """{"data":{"products":[{"name":"First","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},{"name":"Second","_caliban_gateway_key":"p2","_caliban_gateway_typename":"Product"},{"name":"Third","_caliban_gateway_key":"p3","_caliban_gateway_typename":"Product"}]}}"""
        val reviewResponse  =
          """{"data":{"_entities":[null,{"_caliban_gateway_entity_key":"p2","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"Second review"}]},{"_caliban_gateway_entity_key":"p2","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"Duplicate"}]},{"_caliban_gateway_entity_key":"extra","_caliban_gateway_entity_typename":"Product","reviews":[{"body":"Extra"}]}]}}"""

        for {
          products <- stub(productResponse)
          reviews  <- stub(reviewResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, listProducts),
                          Subgraph.federation("reviews", reviews.endpoint, nullableReviews)
                        )
                        .build
          response <- gateway.execute("{ products { name reviews { body } } }")
          sentB    <- reviews.requests.get
          values    = field(response.data, "products").collect { case ResponseListValue(values) => values }.getOrElse(Nil)
          errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
        } yield assertTrue(
          values.flatMap(field(_, "name")) == List(StringValue("First"), StringValue("Second"), StringValue("Third")),
          values.headOption.flatMap(field(_, "reviews")).contains(NullValue),
          values.lift(1).flatMap(field(_, "reviews")).exists {
            case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
              review.contains("body" -> StringValue("Second review"))
            case _                                                     => false
          },
          values.lift(2).flatMap(field(_, "reviews")).contains(NullValue),
          errors.map(_.msg) == List(
            "Entity lookup response contained a duplicate result for 'Product(id)'.",
            "Entity lookup response contained an unexpected result for 'Product(id)'.",
            "Entity lookup response omitted a result for 'Product(id)'."
          ),
          errors.map(_.path) == List(
            List(PathValue.Key("products")),
            List(PathValue.Key("products")),
            List(PathValue.Key("products"), PathValue.Index(2))
          ),
          sentB.size == 1
        )
      }
    ),
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
          .replace(
            "import: [\"@key\", \"@external\"]",
            "import: [{ name: \"@key\", as: \"@entityKey\" }, { name: \"@external\", as: \"@outside\" }]"
          )
          .replace("directive @key", "directive @entityKey")
          .replace("directive @external", "directive @outside")
          .replace("@key(fields:", "@entityKey(fields:")
          .replace("@external", "@outside")

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
          response.errors.map(_.msg) == List("No subgraph owns field 'Thing.ghost'."),
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

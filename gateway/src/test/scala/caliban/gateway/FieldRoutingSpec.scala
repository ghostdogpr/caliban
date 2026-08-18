package caliban.gateway

import caliban.InputValue.{ ListValue, ObjectValue => InputObjectValue }
import caliban.ResponseValue.{ ObjectValue => ResponseObjectValue }
import caliban.Value.IntValue.IntNumber
import caliban.Value.{ BooleanValue, NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.InputValue
import sttp.model.Uri
import zio.Scope
import zio.test._

object FieldRoutingSpec extends ZIOSpecDefault {

  def spec = suite("FieldRoutingSpec")(
    suite("requirements and provided fields")(
      test("rejects invalid requirement and provision field sets with source diagnostics") {
        val endpoint          = Uri.unsafeParse("http://127.0.0.1:1/graphql")
        val malformedRequires =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Query { product: Product }
             |type Product @key(fields: "id") {
             |  id: ID!
             |  price: Int! @external
             |  shippingEstimate: Int! @requires(fields: "price(")
             |}
             |""".stripMargin
        val invalidProvides   =
          s"""
             |${federationSchemaPreamble("@key", "@provides")}
             |type Query { review: Review }
             |type Review { product: Product @provides(fields: "missing") }
             |type Product @key(fields: "id") { id: ID! }
             |""".stripMargin

        for {
          requires           <- Gateway
                                  .compose(Subgraph.federation("inventory", endpoint, malformedRequires))
                                  .build
                                  .exit
          provides           <- Gateway
                                  .compose(Subgraph.federation("reviews", endpoint, invalidProvides))
                                  .build
                                  .exit
          requiresDiagnostics = buildDiagnostics(requires)
          providesDiagnostics = buildDiagnostics(provides)
        } yield assertTrue(
          requires.isFailure,
          provides.isFailure,
          requiresDiagnostics.exists(message =>
            message.contains("[inventory]") && message.contains("Product.shippingEstimate")
          ),
          providesDiagnostics.exists(message => message.contains("[reviews]") && message.contains("Review.product"))
        )
      },
      test("injects argument-bearing requirements without projecting them") {
        val productsSchema    =
          s"""
             |${federationSchemaPreamble("@key", "@shareable")}
             |type Query { product: Product }
             |type Product @key(fields: "id") {
             |  id: ID!
             |  price(multiplier: Int!): Int!
             |  weight: Int!
             |}
             |""".stripMargin
        val inventorySchema   =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  price(multiplier: Int!): Int! @external
             |  weight: Int! @external
             |  shippingEstimate: Int! @requires(fields: "price(multiplier: 2) weight")
             |}
             |""".stripMargin
        val productResponse   =
          """{"data":{"product":{"_caliban_gateway_requirement_price_multiplier_2":11,"_caliban_gateway_requirement_weight":2,"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val inventoryResponse =
          """{"data":{"_entities":[{"shippingEstimate":220}]}}"""

        for {
          products  <- stub(productResponse)
          inventory <- stub(inventoryResponse)
          gateway   <- Gateway
                         .compose(
                           Subgraph.federation("products", products.endpoint, productsSchema),
                           Subgraph.federation("inventory", inventory.endpoint, inventorySchema)
                         )
                         .build
          response  <- gateway.execute("{ product { shippingEstimate } }")
          sentA     <- products.requests.get
          sentB     <- inventory.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "shippingEstimate")).contains(IntNumber(220)),
          field(response.data, "product").forall {
            case ResponseObjectValue(fields) => fields.map(_._1) == List("shippingEstimate")
            case _                           => false
          },
          sentA.headOption
            .flatMap(_.query)
            .exists(query => query.contains("price(multiplier:2)") && query.contains("weight")),
          sentB.headOption
            .flatMap(_.variables)
            .contains(
              Map(
                "representations" -> ListValue(
                  InputObjectValue(
                    Map(
                      "__typename" -> StringValue("Product"),
                      "id"         -> StringValue("p1"),
                      "price"      -> IntNumber(11),
                      "weight"     -> IntNumber(2)
                    )
                  ) :: Nil
                )
              )
            )
        )
      },
      test("evaluates nested fragment requirements for the returned runtime type") {
        val productsSchema    =
          s"""
             |${federationSchemaPreamble("@key", "@shareable")}
             |type Query { product: Product }
             |type Product @key(fields: "id") { id: ID! details: ProductDetails! }
             |interface ProductDetails { code: String! }
             |type PhysicalDetails implements ProductDetails @shareable { code: String! dimensions: Int! }
             |type DigitalDetails implements ProductDetails @shareable { code: String! downloadSize: Int! }
             |""".stripMargin
        val inventorySchema   =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires", "@shareable")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  details: ProductDetails! @external
             |  shippingEstimate: Int! @requires(fields: "details { code ... on PhysicalDetails { dimensions } ... on DigitalDetails { downloadSize } }")
             |}
             |interface ProductDetails { code: String! }
             |type PhysicalDetails implements ProductDetails @shareable { code: String! dimensions: Int! }
             |type DigitalDetails implements ProductDetails @shareable { code: String! downloadSize: Int! }
             |""".stripMargin
        val productResponse   =
          """{"data":{"product":{"_caliban_gateway_requirement_details_code_dimensions_PhysicalDetails_downloadSize_DigitalDetails":{"code":"box","_caliban_gateway_requirement_typename":"PhysicalDetails","dimensions":4},"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val inventoryResponse =
          """{"data":{"_entities":[{"shippingEstimate":40}]}}"""

        for {
          products  <- stub(productResponse)
          inventory <- stub(inventoryResponse)
          gateway   <- Gateway
                         .compose(
                           Subgraph.federation("products", products.endpoint, productsSchema),
                           Subgraph.federation("inventory", inventory.endpoint, inventorySchema)
                         )
                         .build
          response  <- gateway.execute("{ product { shippingEstimate } }")
          sentA     <- products.requests.get
          sentB     <- inventory.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "shippingEstimate")).contains(IntNumber(40)),
          sentA.headOption
            .flatMap(_.query)
            .exists(query => query.contains("...on PhysicalDetails{dimensions}") && query.contains("__typename")),
          sentB.headOption
            .flatMap(_.variables)
            .contains(
              Map(
                "representations" -> ListValue(
                  InputObjectValue(
                    Map(
                      "__typename" -> StringValue("Product"),
                      "id"         -> StringValue("p1"),
                      "details"    -> InputObjectValue(Map("code" -> StringValue("box"), "dimensions" -> IntNumber(4)))
                    )
                  ) :: Nil
                )
              )
            )
        )
      },
      test("uses provided fields only within the annotated result scope") {
        val productsSchema   =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@provides")}
             |type Query {
             |  featured: Product @provides(fields: "name")
             |  regular: Product
             |}
             |type Product @key(fields: "id") { id: ID! name: String! @external }
             |""".stripMargin
        val catalogSchema    =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Product @key(fields: "id") { id: ID! name: String! }
             |""".stripMargin
        val productsResponse =
          """{"data":{"featured":{"name":"Provided"},"regular":{"_caliban_gateway_key":"r1","_caliban_gateway_typename":"Product"}}}"""
        val catalogResponse  =
          """{"data":{"_entities":[{"name":"Routed"}]}}"""

        for {
          products <- stub(productsResponse)
          catalog  <- stub(catalogResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("catalog", catalog.endpoint, catalogSchema)
                        )
                        .build
          response <- gateway.execute("{ featured { name } regular { name } }")
          sentA    <- products.requests.get
          sentB    <- catalog.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "featured").flatMap(field(_, "name")).contains(StringValue("Provided")),
          field(response.data, "regular").flatMap(field(_, "name")).contains(StringValue("Routed")),
          sentA.headOption
            .flatMap(_.query)
            .exists(query => query.contains("featured{name}") && !query.contains("regular{name}")),
          sentB.size == 1
        )
      },
      test("uses a path-scoped provision to satisfy a downstream requirement") {
        val productsSchema   =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@provides")}
             |type Query {
             |  featured: Product @provides(fields: "price")
             |  regular: Product
             |}
             |type Product @key(fields: "id") { id: ID! price: Int! @external }
             |""".stripMargin
        val pricingSchema    =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Product @key(fields: "id") { id: ID! price: Int! }
             |""".stripMargin
        val shippingSchema   =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  price: Int! @external
             |  shipping: Int! @requires(fields: "price")
             |}
             |""".stripMargin
        val productsResponse =
          """{"data":{"featured":{"_caliban_gateway_requirement_price":10,"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},"regular":{"_caliban_gateway_key":"r1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"r1","_caliban_gateway_typename_2":"Product"}}}"""

        for {
          products            <- stub(productsResponse)
          pricing             <- stub("""{"data":{"_entities":[{"_caliban_gateway_requirement_price":20}]}}""")
          shipping            <- stubByRequest(request =>
                                   request.variables
                                     .flatMap(_.get("representations"))
                                     .collect { case ListValue(InputObjectValue(fields) :: Nil) => fields.get("price") }
                                     .flatten match {
                                     case Some(IntNumber(10)) => """{"data":{"_entities":[{"shipping":100}]}}"""
                                     case _                   => """{"data":{"_entities":[{"shipping":200}]}}"""
                                   }
                                 )
          gateway             <- Gateway
                                   .compose(
                                     Subgraph.federation("products", products.endpoint, productsSchema),
                                     Subgraph.federation("pricing", pricing.endpoint, pricingSchema),
                                     Subgraph.federation("shipping", shipping.endpoint, shippingSchema)
                                   )
                                   .build
          response            <- gateway.execute("{ featured { shipping } regular { shipping } }")
          sentA               <- products.requests.get
          sentB               <- pricing.requests.get
          sentC               <- shipping.requests.get
          priceRepresentations = sentB
                                   .flatMap(_.variables)
                                   .flatMap(_.get("representations"))
                                   .collect { case ListValue(values) => values }
                                   .flatten
                                   .toList
          shippingPrices       = sentC
                                   .flatMap(_.variables)
                                   .flatMap(_.get("representations"))
                                   .collect { case ListValue(values) => values }
                                   .flatten
                                   .collect { case InputObjectValue(fields) => fields.get("price") }
                                   .flatten
                                   .toList
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "featured").flatMap(field(_, "shipping")).contains(IntNumber(100)),
          field(response.data, "regular").flatMap(field(_, "shipping")).contains(IntNumber(200)),
          sentA.headOption
            .flatMap(_.query)
            .exists(query =>
              query.contains("featured{_caliban_gateway_requirement_price:price") &&
                !query.contains("regular{_caliban_gateway_requirement_price:price")
            ),
          priceRepresentations.size == 1,
          priceRepresentations.collect { case InputObjectValue(fields) => fields.get("id") }.flatten == List(
            StringValue("r1")
          ),
          shippingPrices == List(IntNumber(10), IntNumber(20))
        )
      },
      test("separates conflicting argument-bearing requirement groups") {
        val productsSchema  =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { product: Product }
             |type Product @key(fields: "id") {
             |  id: ID!
             |  price(multiplier: Int!): Int!
             |  weight: Int!
             |}
             |""".stripMargin
        val inventorySchema =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  price(multiplier: Int!): Int! @external
             |  weight: Int! @external
             |  shippingEstimate: Int! @requires(fields: "price(multiplier: 1) weight")
             |  shippingEstimateDouble: Int! @requires(fields: "price(multiplier: 2) weight")
             |}
             |""".stripMargin
        val productResponse =
          """{"data":{"product":{"_caliban_gateway_requirement_price_multiplier_1":10,"_caliban_gateway_requirement_weight":2,"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product","_caliban_gateway_requirement_price_multiplier_2":20,"_caliban_gateway_requirement_weight_2":2,"_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product"}}}"""

        for {
          products  <- stub(productResponse)
          inventory <- stubByRequest(request =>
                         if (request.query.exists(_.contains("shippingEstimateDouble")))
                           """{"data":{"_entities":[{"shippingEstimateDouble":40}]}}"""
                         else """{"data":{"_entities":[{"shippingEstimate":20}]}}"""
                       )
          gateway   <- Gateway
                         .compose(
                           Subgraph.federation("products", products.endpoint, productsSchema),
                           Subgraph.federation("inventory", inventory.endpoint, inventorySchema)
                         )
                         .build
          response  <- gateway.execute("{ product { shippingEstimate shippingEstimateDouble } }")
          sentA     <- products.requests.get
          sentB     <- inventory.requests.get
          values     = field(response.data, "product")
          prices     = sentB
                         .flatMap(_.variables)
                         .flatMap(_.get("representations"))
                         .collect { case ListValue(InputObjectValue(fields) :: Nil) =>
                           fields.get("price")
                         }
                         .flatten
                         .toSet
        } yield assertTrue(
          response.errors.isEmpty,
          values.flatMap(field(_, "shippingEstimate")).contains(IntNumber(20)),
          values.flatMap(field(_, "shippingEstimateDouble")).contains(IntNumber(40)),
          sentA.headOption
            .flatMap(_.query)
            .exists(query => query.contains("price(multiplier:1)") && query.contains("price(multiplier:2)")),
          sentB.size == 2,
          prices == Set[InputValue](IntNumber(10), IntNumber(20))
        )
      },
      test("orders recursive requirements before their dependents") {
        val rootsSchema    =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { product: Product }
             |type Product @key(fields: "id") { id: ID! }
             |""".stripMargin
        val priceSchema    =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |type Product @key(fields: "id") { id: ID! @external price: Int! }
             |""".stripMargin
        val ratingSchema   =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  price: Int! @external
             |  expensive: Boolean! @requires(fields: "price")
             |}
             |""".stripMargin
        val labelSchema    =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  expensive: Boolean! @external
             |  label: String! @requires(fields: "expensive")
             |}
             |""".stripMargin
        val rootResponse   =
          """{"data":{"product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product","_caliban_gateway_key_3":"p1","_caliban_gateway_typename_3":"Product"}}}"""
        val priceResponse  =
          """{"data":{"_entities":[{"_caliban_gateway_requirement_price":100}]}}"""
        val ratingResponse =
          """{"data":{"_entities":[{"expensive":true,"_caliban_gateway_requirement_expensive":true}]}}"""

        for {
          roots      <- stub(rootResponse)
          prices     <- stub(priceResponse)
          ratings    <- stub(ratingResponse)
          labels     <- stub("""{"data":{"_entities":[{"label":"premium"}]}}""")
          gateway    <- Gateway
                          .compose(
                            Subgraph.federation("roots", roots.endpoint, rootsSchema),
                            Subgraph.federation("prices", prices.endpoint, priceSchema),
                            Subgraph.federation("ratings", ratings.endpoint, ratingSchema),
                            Subgraph.federation("labels", labels.endpoint, labelSchema)
                          )
                          .build
          response   <- gateway.execute("{ product { expensive label } }")
          priceSent  <- prices.requests.get
          ratingSent <- ratings.requests.get
          labelSent  <- labels.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          field(response.data, "product").flatMap(field(_, "label")).contains(StringValue("premium")),
          priceSent.size == 1,
          ratingSent.size == 2,
          ratingSent.headOption.flatMap(_.variables).exists(_.toString.contains("100")),
          labelSent.headOption.flatMap(_.variables).exists(_.toString.contains("true"))
        )
      },
      test("blocks dependent requirement routes while preserving independent root data") {
        val rootsSchema  =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { status: String! product: Product }
             |type Product @key(fields: "id") { id: ID! }
             |""".stripMargin
        val priceSchema  =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |type Product @key(fields: "id") { id: ID! @external price: Int! }
             |""".stripMargin
        val ratingSchema =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  price: Int! @external
             |  expensive: Boolean! @requires(fields: "price")
             |}
             |""".stripMargin
        val labelSchema  =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  expensive: Boolean! @external
             |  label: String! @requires(fields: "expensive")
             |}
             |""".stripMargin
        val rootResponse =
          """{"data":{"status":"ok","product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product","_caliban_gateway_key_3":"p1","_caliban_gateway_typename_3":"Product"}}}"""

        for {
          roots      <- stub(rootResponse)
          prices     <- stub("""{"data":{"_entities":[null]}}""")
          ratings    <- stub("""{"data":{"_entities":[{"_caliban_gateway_requirement_expensive":true}]}}""")
          labels     <- stub("""{"data":{"_entities":[{"label":"premium"}]}}""")
          gateway    <- Gateway
                          .compose(
                            Subgraph.federation("roots", roots.endpoint, rootsSchema),
                            Subgraph.federation("prices", prices.endpoint, priceSchema),
                            Subgraph.federation("ratings", ratings.endpoint, ratingSchema),
                            Subgraph.federation("labels", labels.endpoint, labelSchema)
                          )
                          .build
          response   <- gateway.execute("{ status product { label } }")
          priceSent  <- prices.requests.get
          ratingSent <- ratings.requests.get
          labelSent  <- labels.requests.get
        } yield assertTrue(
          field(response.data, "status").contains(StringValue("ok")),
          field(response.data, "product").contains(NullValue),
          response.errors.nonEmpty,
          priceSent.size == 1,
          ratingSent.isEmpty,
          labelSent.isEmpty
        )
      },
      test("executes a locally owned dependent field after its remote requirements") {
        val productsSchema =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Query { product: Product }
             |type Product @key(fields: "id") {
             |  id: ID!
             |  price: Int! @external
             |  label: String! @requires(fields: "price")
             |}
             |""".stripMargin
        val pricingSchema  =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Product @key(fields: "id") { id: ID! price: Int! }
             |""".stripMargin
        val rootResponse   =
          """{"data":{"product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product"}}}"""

        for {
          products <- stubByRequest(request =>
                        if (request.query.exists(_.contains("_entities")))
                          """{"data":{"_entities":[{"label":"premium"}]}}"""
                        else rootResponse
                      )
          pricing  <- stub("""{"data":{"_entities":[{"_caliban_gateway_requirement_price":100}]}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("pricing", pricing.endpoint, pricingSchema)
                        )
                        .build
          response <- gateway.execute("{ product { label } }")
          sentA    <- products.requests.get
          sentB    <- pricing.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "label")).contains(StringValue("premium")),
          sentA.size == 2,
          sentA.headOption.flatMap(_.query).forall(!_.contains("label")),
          sentB.size == 1,
          sentA.lastOption.flatMap(_.variables).exists(_.toString.contains("100"))
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

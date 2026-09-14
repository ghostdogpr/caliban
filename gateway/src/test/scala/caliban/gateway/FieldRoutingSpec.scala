package caliban.gateway

import caliban.InputValue.{ ListValue, ObjectValue => InputObjectValue }
import caliban.ResponseValue.{ ObjectValue => ResponseObjectValue }
import caliban.Value.IntValue.IntNumber
import caliban.Value.{ BooleanValue, EnumValue, NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.{ GraphQLRequest, InputValue }
import zio.http.URL
import zio.{ Scope, ZIO }
import zio.test._

object FieldRoutingSpec extends ZIOSpecDefault {

  private val productRoots =
    s"""
       |${federationSchemaPreamble("@key")}
       |type Query { product: Product }
       |type Product @key(fields: "id") { id: ID! }
       |""".stripMargin

  private val productRoot =
    """{"data":{"product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""

  private val nodeRoots =
    s"""
       |${federationSchemaPreamble("@key")}
       |type Query { node: Node }
       |interface Node @key(fields: "id") { id: ID! }
       |type Product implements Node @key(fields: "id") { id: ID! }
       |type Service implements Node @key(fields: "id") { id: ID! }
       |""".stripMargin

  private def nodeRoot(typename: String, id: String): String =
    s"""{"data":{"node":{"_caliban_gateway_key":"$id","_caliban_gateway_key_2":"$id","_caliban_gateway_key_3":"$id","_caliban_gateway_typename":"$typename","_caliban_gateway_typename_2":"$typename","_caliban_gateway_runtime_typename":"$typename"}}}"""

  private def productRatings(price: String, expensive: String): String =
    s"""
       |${federationSchemaPreamble("@key", "@external", "@requires")}
       |type Product @key(fields: "id") {
       |  id: ID! @external
       |  price: $price @external
       |  expensive: $expensive @requires(fields: "price")
       |}
       |""".stripMargin

  def spec = suite("FieldRoutingSpec")(
    suite("requirements and provided fields")(
      test("rejects invalid requirement and provision field sets with source diagnostics") {
        val endpoint          = unreachableEndpoint
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
        val unknownRequires   = List(
          malformedRequires.replace("price(", "missing"),
          malformedRequires
            .replace("price: Int! @external", "details: Details @external")
            .replace("price(", "details { missing }") + "type Details { existing: Int }"
        )

        for {
          requires           <- Gateway
                                  .compose(Subgraph.federation("inventory", endpoint, malformedRequires))
                                  .interpreter
                                  .exit
          provides           <- Gateway
                                  .compose(Subgraph.federation("reviews", endpoint, invalidProvides))
                                  .interpreter
                                  .exit
          unknown            <- ZIO.foreach(unknownRequires)(schema =>
                                  Gateway.compose(Subgraph.federation("inventory", endpoint, schema)).interpreter.exit
                                )
          requiresDiagnostics = buildDiagnostics(requires)
          providesDiagnostics = buildDiagnostics(provides)
        } yield assertTrue(
          requires.isFailure,
          provides.isFailure,
          unknown.forall(exit =>
            buildDiagnostics(exit).exists(message =>
              message.contains("[inventory]") && message.contains("Product.shippingEstimate") &&
                message.contains("Field 'missing' does not exist")
            )
          ),
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
                         .interpreter
          response  <- gateway.execute("{ product { shippingEstimate } }")
          sentA     <- products.requests.get
          sentB     <- inventory.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "shippingEstimate")).contains(IntNumber(220)),
          field(response.data, "product").forall(fieldNames(_) == List("shippingEstimate")),
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
      test("forwards list and nested-list requirement values") {
        val productsSchema  =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { product: Product }
             |type Product @key(fields: "id") {
             |  id: ID!
             |  tags: [[String!]!]!
             |}
             |""".stripMargin
        val shippingSchema  =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  tags: [[String!]!]! @external
             |  shipping: Int! @requires(fields: "tags")
             |}
             |""".stripMargin
        val productResponse =
          """{"data":{"product":{"_caliban_gateway_requirement_tags":[["fragile","large"],["priority"]],"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""

        for {
          products <- stub(productResponse)
          shipping <- stub("""{"data":{"_entities":[{"shipping":5}]}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("shipping", shipping.endpoint, shippingSchema)
                        )
                        .interpreter
          response <- gateway.execute("{ product { shipping } }")
          sent     <- shipping.requests.get
          tags      = sent.headOption
                        .flatMap(_.variables)
                        .flatMap(_.get("representations"))
                        .collect { case ListValue(InputObjectValue(fields) :: Nil) => fields.get("tags") }
                        .flatten
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "shipping")).contains(IntNumber(5)),
          tags.contains(
            ListValue(
              List(
                ListValue(List(StringValue("fragile"), StringValue("large"))),
                ListValue(List(StringValue("priority")))
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
             |  shippingEstimate: Int! @requires(fields: "details { __typename code ... on PhysicalDetails { dimensions } ... on DigitalDetails { downloadSize } }")
             |}
             |interface ProductDetails { code: String! }
             |type PhysicalDetails implements ProductDetails @shareable { code: String! dimensions: Int! }
             |type DigitalDetails implements ProductDetails @shareable { code: String! downloadSize: Int! }
             |""".stripMargin
        val productResponse   =
          """{"data":{"product":{"_caliban_gateway_requirement_details___typename_code_dimensions_PhysicalItem_downloadSize_DigitalDetails":{"__typename":"PhysicalDetails","code":"box","_caliban_gateway_requirement_typename":"PhysicalDetails","_caliban_gateway_runtime_typename":"PhysicalDetails","dimensions":4},"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
        val inventoryResponse =
          """{"data":{"_entities":[{"shippingEstimate":40}]}}"""

        for {
          products  <- stub(productResponse)
          inventory <- stub(inventoryResponse)
          gateway   <- Gateway
                         .compose(
                           Subgraph
                             .federation("products", products.endpoint, productsSchema)
                             .transform(SchemaTransformation.renameType("PhysicalDetails", "PhysicalItem")),
                           Subgraph
                             .federation("inventory", inventory.endpoint, inventorySchema)
                             .transform(SchemaTransformation.renameType("PhysicalDetails", "PhysicalItem"))
                         )
                         .interpreter
          response  <- gateway.execute("{ product { shippingEstimate } }")
          sentA     <- products.requests.get
          sentB     <- inventory.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "shippingEstimate")).contains(IntNumber(40)),
          sentA.headOption
            .flatMap(_.query)
            .exists(query => query.contains("...on PhysicalDetails{dimensions code}") && query.contains("__typename")),
          sentB.headOption
            .flatMap(_.variables)
            .contains(
              Map(
                "representations" -> ListValue(
                  InputObjectValue(
                    Map(
                      "__typename" -> StringValue("Product"),
                      "id"         -> StringValue("p1"),
                      "details"    -> InputObjectValue(
                        Map(
                          "__typename" -> StringValue("PhysicalDetails"),
                          "code"       -> StringValue("box"),
                          "dimensions" -> IntNumber(4)
                        )
                      )
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
                        .interpreter
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
          shipping            <- stubByRequest { request =>
                                   val entities = request.variables
                                     .flatMap(_.get("representations"))
                                     .collect { case ListValue(values) => values }
                                     .getOrElse(Nil)
                                     .map {
                                       case InputObjectValue(fields) if fields.get("price").contains(IntNumber(10)) =>
                                         """{"shipping":100}"""
                                       case _                                                                       =>
                                         """{"shipping":200}"""
                                     }
                                   s"""{"data":{"_entities":[${entities.mkString(",")}]}}"""
                                 }
          gateway             <- Gateway
                                   .compose(
                                     Subgraph.federation("products", products.endpoint, productsSchema),
                                     Subgraph.federation("pricing", pricing.endpoint, pricingSchema),
                                     Subgraph.federation("shipping", shipping.endpoint, shippingSchema)
                                   )
                                   .interpreter
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
          sentC.size == 1,
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
                         .interpreter
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
      test("reuses an identical requirement across entity fetches") {
        val productsSchema  =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { product: Product }
             |type Product @key(fields: "id") { id: ID! price: Int! }
             |""".stripMargin
        val shippingSchema  =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  price: Int! @external
             |  shipping: Int! @requires(fields: "price")
             |}
             |""".stripMargin
        val taxSchema       =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  price: Int! @external
             |  tax: Int! @requires(fields: "price")
             |}
             |""".stripMargin
        val productResponse =
          """{"data":{"product":{"_caliban_gateway_requirement_price":10,"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product"}}}"""

        for {
          products <- stub(productResponse)
          shipping <- stub("""{"data":{"_entities":[{"shipping":20}]}}""")
          tax      <- stub("""{"data":{"_entities":[{"tax":3}]}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("shipping", shipping.endpoint, shippingSchema),
                          Subgraph.federation("tax", tax.endpoint, taxSchema)
                        )
                        .interpreter
          response <- gateway.execute("{ product { shipping tax } }")
          sentA    <- products.requests.get
          sentB    <- shipping.requests.get
          sentC    <- tax.requests.get
          query     = sentA.headOption.flatMap(_.query).getOrElse("")
          alias     = "_caliban_gateway_requirement_price:price"
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "shipping")).contains(IntNumber(20)),
          field(response.data, "product").flatMap(field(_, "tax")).contains(IntNumber(3)),
          query.sliding(alias.length).count(_ == alias) == 1,
          !query.contains("_caliban_gateway_requirement_price_2"),
          sentB.size == 1,
          sentC.size == 1
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
        val priceResponse  =
          """{"data":{"_entities":[{"_caliban_gateway_requirement_price":100}]}}"""
        val ratingResponse =
          """{"data":{"_entities":[{"expensive":true,"_caliban_gateway_requirement_expensive":true}]}}"""

        for {
          roots      <- stub(productRoot)
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
                          .interpreter
          response   <- gateway.execute("{ product { expensive label } }")
          priceSent  <- prices.requests.get
          ratingSent <- ratings.requests.get
          labelSent  <- labels.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          field(response.data, "product").flatMap(field(_, "label")).contains(StringValue("premium")),
          priceSent.size == 1,
          ratingSent.size == 1,
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
                          .interpreter
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
                        .interpreter
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
    ),
    suite("shared keys and merged fetches")(
      test("shares one injected key alias between sibling entity fetches without fetching the typename") {
        val shippingSchema =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |type Product @key(fields: "id") { id: ID! @external shipping: Int! }
             |""".stripMargin
        val taxSchema      =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |type Product @key(fields: "id") { id: ID! @external tax: Int! }
             |""".stripMargin

        for {
          roots    <- stub(productRoot)
          shipping <- stub("""{"data":{"_entities":[{"shipping":20}]}}""")
          tax      <- stub("""{"data":{"_entities":[{"tax":3}]}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("roots", roots.endpoint, productRoots),
                          Subgraph.federation("shipping", shipping.endpoint, shippingSchema),
                          Subgraph.federation("tax", tax.endpoint, taxSchema)
                        )
                        .interpreter
          response <- gateway.execute("{ product { shipping tax } }")
          rootSent <- roots.requests.get
          query     = rootSent.headOption.flatMap(_.query).getOrElse("")
          key       = "_caliban_gateway_key:id"
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "shipping")).contains(IntNumber(20)),
          field(response.data, "product").flatMap(field(_, "tax")).contains(IntNumber(3)),
          query.sliding(key.length).count(_ == key) == 1,
          !query.contains("_caliban_gateway_key_2"),
          !query.contains("_caliban_gateway_typename")
        )
      },
      test("keeps the typename when conditional branches share an entity path") {
        val rootsSchema   =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { nodes: [Node!]! }
             |union Node = A | B
             |type A { child: Product }
             |type B { child: User }
             |type Product @key(fields: "id") { id: ID! }
             |type User @key(fields: "id") { id: ID! }
             |""".stripMargin
        val detailsSchema =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |type Product @key(fields: "id") { id: ID! @external name: String }
             |type User @key(fields: "id") { id: ID! @external name: String }
             |""".stripMargin
        val rootResponse  =
          """{"data":{"nodes":[
            |{"_caliban_gateway_runtime_typename":"A","_caliban_gateway_child":{"_caliban_gateway_key":"p","_caliban_gateway_typename":"Product"}},
            |{"_caliban_gateway_runtime_typename":"B","_caliban_gateway_child_1":{"_caliban_gateway_key":"u","_caliban_gateway_typename":"User"}}
            |]}}""".stripMargin.replace("\n", "")

        for {
          roots    <- stub(rootResponse)
          details  <-
            stubByRequest { request =>
              val entities = request.variables.flatMap(_.get("representations")).toList.flatMap {
                case ListValue(values) => values
                case _                 => Nil
              }
              val names    = entities.collect { case InputObjectValue(fields) =>
                (fields.get("__typename"), fields.get("id")) match {
                  case (Some(StringValue(typename)), Some(StringValue(id))) =>
                    s"""{"_caliban_gateway_entity_key":"$id","_caliban_gateway_entity_typename":"$typename","name":"$typename-$id"}"""
                  case _                                                    => "null"
                }
              }
              s"""{"data":{"_entities":[${names.mkString(",")}]}}"""
            }
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("roots", roots.endpoint, rootsSchema),
                          Subgraph.federation("details", details.endpoint, detailsSchema)
                        )
                        .interpreter
          response <- gateway.execute("{ nodes { ... on A { child { name } } ... on B { child { name } } } }")
          rootSent <- roots.requests.get
          rendered  = response.data.toString
        } yield assertTrue(
          response.errors.isEmpty,
          rootSent.exists(_.query.exists(_.contains("_caliban_gateway_typename:__typename"))),
          rendered.contains("Product-p"),
          rendered.contains("User-u"),
          !rendered.contains("Product-u"),
          !rendered.contains("User-p")
        )
      },
      test("reuses a client alias whose selection matches an injected key") {
        val shippingSchema =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |type Product @key(fields: "id") { id: ID! @external shipping: Int! }
             |""".stripMargin

        for {
          roots    <- stub(productRoot)
          shipping <- stub("""{"data":{"_entities":[{"shipping":20}]}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("roots", roots.endpoint, productRoots),
                          Subgraph.federation("shipping", shipping.endpoint, shippingSchema)
                        )
                        .interpreter
          response <- gateway.execute("{ product { _caliban_gateway_key: id shipping } }")
          rootSent <- roots.requests.get
          query     = rootSent.headOption.flatMap(_.query).getOrElse("")
          key       = "_caliban_gateway_key:id"
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "_caliban_gateway_key")).contains(StringValue("p1")),
          field(response.data, "product").flatMap(field(_, "shipping")).contains(IntNumber(20)),
          query.sliding(key.length).count(_ == key) == 1,
          !query.contains("_caliban_gateway_key_2")
        )
      },
      test("merges a requirement fetch with the field fetch of the same subgraph into one call") {
        val priceSchema    =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |type Product @key(fields: "id") { id: ID! @external price: Int! }
             |""".stripMargin
        val ratingSchema   = productRatings("Int!", "Boolean!")
        val priceResponse  =
          """{"data":{"_entities":[{"price":100,"_caliban_gateway_requirement_price":100}]}}"""
        val ratingResponse = """{"data":{"_entities":[{"expensive":true}]}}"""

        for {
          roots      <- stub(productRoot)
          prices     <- stub(priceResponse)
          ratings    <- stub(ratingResponse)
          gateway    <- Gateway
                          .compose(
                            Subgraph.federation("roots", roots.endpoint, productRoots),
                            Subgraph.federation("prices", prices.endpoint, priceSchema),
                            Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                          )
                          .interpreter
          plan       <- gateway.explain(GraphQLRequest(query = Some("{ product { price expensive } }")))
          response   <- gateway.execute("{ product { price expensive } }")
          priceSent  <- prices.requests.get
          ratingSent <- ratings.requests.get
          priceQuery  = priceSent.headOption.flatMap(_.query).getOrElse("")
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "price")).contains(IntNumber(100)),
          field(response.data, "product").flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          priceSent.size == 1,
          priceQuery.contains("_caliban_gateway_requirement_price:price"),
          ratingSent.size == 1,
          ratingSent.headOption.flatMap(_.variables).exists(_.toString.contains("100")),
          plan.linesIterator.count(_.startsWith("fetch prices")) == 1
        )
      },
      test("keeps a prerequisite separate when its provider declares it non-null but composition widens it") {
        val priceSchema    =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@shareable")}
             |type Product @key(fields: "id") { id: ID! @external displayPrice: Int price: Int! @shareable }
             |""".stripMargin
        val catalogSchema  =
          s"""
             |${federationSchemaPreamble("@key", "@shareable")}
             |type Product @key(fields: "id") { id: ID! price: Int @shareable }
             |""".stripMargin
        val ratingSchema   = productRatings("Int!", "Boolean")
        val priceResponse  =
          """{"data":{"_entities":[null]},"errors":[{"message":"price unavailable","path":["_entities",0,"_caliban_gateway_requirement_price"]}]}"""
        val ratingResponse = """{"data":{"_entities":[{"expensive":true}]}}"""

        for {
          roots         <- stub(productRoot)
          prices        <- stubByRequest(request =>
                             if (request.query.exists(_.contains("_caliban_gateway_requirement_price"))) priceResponse
                             else """{"data":{"_entities":[{"displayPrice":10}]}}"""
                           )
          catalog       <- stub(priceResponse)
          ratings       <- stub(ratingResponse)
          gateway       <- Gateway
                             .compose(
                               Subgraph.federation("roots", roots.endpoint, productRoots),
                               Subgraph.federation("prices", prices.endpoint, priceSchema),
                               Subgraph.federation("catalog", catalog.endpoint, catalogSchema),
                               Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                             )
                             .interpreter
          introspection <- gateway.execute("""{ __type(name: "Product") { fields { name type { kind } } } }""")
          response      <- gateway.execute("{ product { displayPrice expensive } }")
          priceSent     <- prices.requests.get
          catalogSent   <- catalog.requests.get
          ratingSent    <- ratings.requests.get
          product        = field(response.data, "product")
          priceKind      = listValues(field(introspection.data, "__type").flatMap(field(_, "fields")))
                             .find(field(_, "name").contains(StringValue("price")))
                             .flatMap(field(_, "type"))
                             .flatMap(field(_, "kind"))
        } yield assertTrue(
          priceKind.contains(EnumValue("SCALAR")),
          product.flatMap(field(_, "displayPrice")).contains(IntNumber(10)),
          product.flatMap(field(_, "expensive")).contains(NullValue),
          response.errors.nonEmpty,
          priceSent.size + catalogSent.size == 2,
          ratingSent.isEmpty
        )
      },
      test("keeps a non-null client field separate from a nullable prerequisite it would otherwise erase") {
        val priceSchema     =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@shareable")}
             |type Product @key(fields: "id") { id: ID! @external displayPrice: Int! @shareable price: Int }
             |""".stripMargin
        val catalogSchema   =
          s"""
             |${federationSchemaPreamble("@key", "@shareable")}
             |type Product @key(fields: "id") { id: ID! displayPrice: Int @shareable }
             |""".stripMargin
        val ratingSchema    = productRatings("Int", "Boolean")
        val displayResponse =
          """{"data":{"_entities":[null]},"errors":[{"message":"displayPrice unavailable","path":["_entities",0,"displayPrice"]}]}"""
        val priceResponse   = """{"data":{"_entities":[{"_caliban_gateway_requirement_price":100}]}}"""
        val ratingResponse  = """{"data":{"_entities":[{"expensive":true}]}}"""

        for {
          roots         <- stub(productRoot)
          prices        <- stubByRequest(request =>
                             if (request.query.exists(_.contains("displayPrice"))) displayResponse else priceResponse
                           )
          catalog       <- stub(displayResponse)
          ratings       <- stub(ratingResponse)
          gateway       <- Gateway
                             .compose(
                               Subgraph.federation("roots", roots.endpoint, productRoots),
                               Subgraph.federation("prices", prices.endpoint, priceSchema),
                               Subgraph.federation("catalog", catalog.endpoint, catalogSchema),
                               Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                             )
                             .interpreter
          introspection <- gateway.execute("""{ __type(name: "Product") { fields { name type { kind } } } }""")
          response      <- gateway.execute("{ product { displayPrice expensive } }")
          priceSent     <- prices.requests.get
          catalogSent   <- catalog.requests.get
          ratingSent    <- ratings.requests.get
          product        = field(response.data, "product")
          displayKind    = listValues(field(introspection.data, "__type").flatMap(field(_, "fields")))
                             .find(field(_, "name").contains(StringValue("displayPrice")))
                             .flatMap(field(_, "type"))
                             .flatMap(field(_, "kind"))
        } yield assertTrue(
          displayKind.contains(EnumValue("SCALAR")),
          product.flatMap(field(_, "displayPrice")).contains(NullValue),
          product.flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          response.errors.nonEmpty,
          priceSent.size + catalogSent.size == 2,
          ratingSent.size == 1
        )
      },
      test("keeps a lookup that covers every implementation over a cheaper single-implementation merge") {
        val priceSchema    =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |interface Node @key(fields: "id") { id: ID! @external displayPrice: Int price: Int }
             |type Product implements Node @key(fields: "id") { id: ID! @external displayPrice: Int price: Int }
             |type Service implements Node @key(fields: "id") { id: ID! @external displayPrice: Int price: Int }
             |""".stripMargin
        val ratingSchema   =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |interface Node @key(fields: "id") {
             |  id: ID! @external
             |  price: Int @external
             |  expensive: Boolean @requires(fields: "price")
             |}
             |type Product implements Node @key(fields: "id") {
             |  id: ID! @external
             |  price: Int @external
             |  expensive: Boolean @requires(fields: "price")
             |}
             |type Service implements Node @key(fields: "id") {
             |  id: ID! @external
             |  price: Int @external
             |  expensive: Boolean @requires(fields: "price")
             |}
             |""".stripMargin
        val ratingResponse = """{"data":{"_entities":[{"expensive":true}]}}"""

        for {
          roots      <- stub(nodeRoot("Product", "p1"))
          prices     <- stubByRequest(request =>
                          if (request.query.exists(_.contains("_caliban_gateway_requirement_price")))
                            """{"data":{"_entities":[{"_caliban_gateway_requirement_price":100}]}}"""
                          else """{"data":{"_entities":[{"displayPrice":10}]}}"""
                        )
          ratings    <- stub(ratingResponse)
          gateway    <- Gateway
                          .compose(
                            Subgraph.federation("roots", roots.endpoint, nodeRoots),
                            Subgraph.federation("prices", prices.endpoint, priceSchema),
                            Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                          )
                          .interpreter
          plan       <- gateway.explain(GraphQLRequest(query = Some("{ node { displayPrice expensive } }")))
          response   <- gateway.execute("{ node { displayPrice expensive } }")
          priceSent  <- prices.requests.get
          ratingSent <- ratings.requests.get

          node    = field(response.data, "node")
          covered = (implementation: String) =>
                      plan.linesIterator.exists(line =>
                        line.startsWith("fetch prices") && line.contains("displayPrice") &&
                          (line.contains("via Node(") || line.contains(s"via $implementation("))
                      )
        } yield assertTrue(
          covered("Product"),
          covered("Service"),
          response.errors.isEmpty,
          node.flatMap(field(_, "displayPrice")).contains(IntNumber(10)),
          node.flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          priceSent.nonEmpty,
          priceSent.forall(_.variables.exists(_.toString.contains("Product"))),
          ratingSent.size == 1
        )
      },
      test("serves an implementation without its own key through the interface lookup") {
        val priceSchema    =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@shareable")}
             |interface Node @key(fields: "id") { id: ID! @external displayPrice: Int price: Int }
             |type Product implements Node @key(fields: "id") { id: ID! @external displayPrice: Int price: Int }
             |type Service implements Node { id: ID! @shareable displayPrice: Int price: Int }
             |""".stripMargin
        val ratingSchema   =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |interface Node @key(fields: "id") {
             |  id: ID! @external
             |  price: Int @external
             |  expensive: Boolean @requires(fields: "price")
             |}
             |type Product implements Node @key(fields: "id") {
             |  id: ID! @external
             |  price: Int @external
             |  expensive: Boolean @requires(fields: "price")
             |}
             |type Service implements Node @key(fields: "id") { id: ID! @external price: Int @external expensive: Boolean }
             |""".stripMargin
        val ratingResponse = """{"data":{"_entities":[{"expensive":true}]}}"""

        for {
          roots      <- stub(nodeRoot("Service", "s1"))
          prices     <- stubByRequest(request =>
                          if (request.query.exists(_.contains("_caliban_gateway_requirement_price")))
                            """{"data":{"_entities":[{"_caliban_gateway_requirement_price":100}]}}"""
                          else """{"data":{"_entities":[{"displayPrice":10}]}}"""
                        )
          ratings    <- stub(ratingResponse)
          gateway    <- Gateway
                          .compose(
                            Subgraph.federation("roots", roots.endpoint, nodeRoots),
                            Subgraph.federation("prices", prices.endpoint, priceSchema),
                            Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                          )
                          .interpreter
          plan       <- gateway.explain(GraphQLRequest(query = Some("{ node { displayPrice expensive } }")))
          response   <- gateway.execute("{ node { displayPrice expensive } }")
          priceSent  <- prices.requests.get
          ratingSent <- ratings.requests.get
          rootSent   <- roots.requests.get
          node        = field(response.data, "node")
        } yield assertTrue(
          plan.linesIterator.exists(line =>
            line.startsWith("fetch prices") && line.contains("via Node(") && line.contains("displayPrice")
          ),
          rootSent.exists(_.query.exists(_.contains("_caliban_gateway_typename:__typename"))),
          response.errors.isEmpty,
          node.flatMap(field(_, "displayPrice")).contains(IntNumber(10)),
          node.flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          priceSent.nonEmpty,
          priceSent.forall(_.variables.exists(_.toString.contains("Service"))),
          ratingSent.size == 1
        )
      },
      test("keeps a fetch from depending on itself when siblings share a key alias") {
        val usersSchema    =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { users: [NodeWithName!]! }
             |interface NodeWithName @key(fields: "id") { id: ID! name: String }
             |type User implements NodeWithName @key(fields: "id") { id: ID! name: String age: Int }
             |""".stripMargin
        val accountsSchema =
          s"""
             |${federationSchemaPreamble("@key", "@interfaceObject", "@external", "@requires")}
             |type Query { anotherUsers: [NodeWithName] }
             |type NodeWithName @key(fields: "id") @interfaceObject {
             |  id: ID!
             |  name: String @external
             |  username: String @requires(fields: "name")
             |}
             |""".stripMargin
        val rootResponse   =
          """{"data":{"anotherUsers":[{"id":"u1","_caliban_gateway_key":"u1","_caliban_gateway_key_2":"u1","_caliban_gateway_key_3":"u1","_caliban_gateway_typename":"NodeWithName","_caliban_gateway_typename_2":"NodeWithName","_caliban_gateway_typename_3":"NodeWithName"}]}}"""
        val usersEntity    =
          """{"data":{"_entities":[{"name":"u1-name","_caliban_gateway_requirement_name":"u1-name","_caliban_gateway_runtime_typename":"User","_caliban_gateway_runtime_typename_2":"User"}]}}"""
        val accountsEntity =
          """{"data":{"_entities":[{"username":"u1-username","_caliban_gateway_key":"u1","_caliban_gateway_typename":"NodeWithName"}]}}"""

        for {
          users    <- stub(usersEntity)
          accounts <- stubByRequest(request =>
                        if (request.query.exists(_.contains("_entities"))) accountsEntity else rootResponse
                      )
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("users", users.endpoint, usersSchema),
                          Subgraph.federation("accounts", accounts.endpoint, accountsSchema)
                        )
                        .interpreter
          response <- gateway.execute("{ anotherUsers { ... on User { username } id name } }")
          user      = listValues(field(response.data, "anotherUsers")).headOption
        } yield assertTrue(
          response.errors.isEmpty,
          user.flatMap(field(_, "username")).contains(StringValue("u1-username")),
          user.flatMap(field(_, "id")).contains(StringValue("u1")),
          user.flatMap(field(_, "name")).contains(StringValue("u1-name"))
        )
      },
      test("merges a nullable prerequisite with client fields of the same subgraph") {
        val priceSchema    =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |type Product @key(fields: "id") { id: ID! @external displayPrice: Int price: Int }
             |""".stripMargin
        val ratingSchema   = productRatings("Int", "Boolean")
        val priceResponse  =
          """{"data":{"_entities":[{"displayPrice":10,"_caliban_gateway_requirement_price":100}]}}"""
        val ratingResponse = """{"data":{"_entities":[{"expensive":true}]}}"""

        for {
          roots      <- stub(productRoot)
          prices     <- stub(priceResponse)
          ratings    <- stub(ratingResponse)
          gateway    <- Gateway
                          .compose(
                            Subgraph.federation("roots", roots.endpoint, productRoots),
                            Subgraph.federation("prices", prices.endpoint, priceSchema),
                            Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                          )
                          .interpreter
          response   <- gateway.execute("{ product { displayPrice expensive } }")
          priceSent  <- prices.requests.get
          ratingSent <- ratings.requests.get
          product     = field(response.data, "product")
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "displayPrice")).contains(IntNumber(10)),
          product.flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          priceSent.size == 1,
          ratingSent.size == 1
        )
      },
      test("merges identical prerequisite fetches of different consumers into one call") {
        val priceSchema    =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |type Product @key(fields: "id") { id: ID! @external price: Int! }
             |""".stripMargin
        val shippingSchema =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  price: Int! @external
             |  shipping: Int! @requires(fields: "price")
             |}
             |""".stripMargin
        val taxSchema      =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  price: Int! @external
             |  tax: Int! @requires(fields: "price")
             |}
             |""".stripMargin
        val priceResponse  = """{"data":{"_entities":[{"_caliban_gateway_requirement_price":100}]}}"""

        for {
          roots        <- stub(productRoot)
          prices       <- stub(priceResponse)
          shipping     <- stub("""{"data":{"_entities":[{"shipping":20}]}}""")
          tax          <- stub("""{"data":{"_entities":[{"tax":3}]}}""")
          gateway      <- Gateway
                            .compose(
                              Subgraph.federation("roots", roots.endpoint, productRoots),
                              Subgraph.federation("prices", prices.endpoint, priceSchema),
                              Subgraph.federation("shipping", shipping.endpoint, shippingSchema),
                              Subgraph.federation("tax", tax.endpoint, taxSchema)
                            )
                            .interpreter
          response     <- gateway.execute("{ product { shipping tax } }")
          priceSent    <- prices.requests.get
          shippingSent <- shipping.requests.get
          taxSent      <- tax.requests.get
          product       = field(response.data, "product")
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "shipping")).contains(IntNumber(20)),
          product.flatMap(field(_, "tax")).contains(IntNumber(3)),
          priceSent.size == 1,
          shippingSent.size == 1,
          taxSent.size == 1
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

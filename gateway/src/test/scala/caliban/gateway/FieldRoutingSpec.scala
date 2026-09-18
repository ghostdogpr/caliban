package caliban.gateway

import caliban.InputValue.{ ListValue, ObjectValue => InputObjectValue }
import caliban.Value.IntValue.IntNumber
import caliban.Value.{ BooleanValue, EnumValue, NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.{ GraphQLRequest, InputValue, ResponseValue }
import zio.{ Duration, Scope, ZIO }
import zio.test._

object FieldRoutingSpec extends ZIOSpecDefault {

  private val nodeWithNameAccounts =
    s"""
       |${federationSchemaPreamble("@key", "@interfaceObject")}
       |type NodeWithName @key(fields: "id") @interfaceObject { id: ID! username: String }
       |""".stripMargin

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

  private val productShippingSchema =
    s"""
       |${federationSchemaPreamble("@key", "@external")}
       |type Product @key(fields: "id") { id: ID! @external shipping: Int! }
       |""".stripMargin

  private def productRequiringPrice(field: String, price: String = "Int!"): String =
    s"""
       |${federationSchemaPreamble("@key", "@external", "@requires")}
       |type Product @key(fields: "id") {
       |  id: ID! @external
       |  price: $price @external
       |  $field @requires(fields: "price")
       |}
       |""".stripMargin

  private val pricesByRequest =
    stubByRequest(request =>
      if (request.query.exists(_.contains("_caliban_gateway_requirement_price")))
        """{"data":{"_entities":[{"_caliban_gateway_requirement_price":100}]}}"""
      else """{"data":{"_entities":[{"displayPrice":10}]}}"""
    )

  private def fieldKind(introspection: ResponseValue, name: String): Option[ResponseValue] =
    listValues(field(introspection, "__type").flatMap(field(_, "fields")))
      .find(field(_, "name").contains(StringValue(name)))
      .flatMap(field(_, "type"))
      .flatMap(field(_, "kind"))

  private def representations(request: GraphQLRequest): List[InputValue] =
    request.variables.flatMap(_.get("representations")).toList.flatMap {
      case ListValue(values) => values
      case _                 => Nil
    }

  def spec = suite("FieldRoutingSpec")(
    suite("requirements and provided fields")(
      test("rejects invalid requirement and provision field sets with source diagnostics") {
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
                                  .compose(Subgraph.federation("inventory", unreachableEndpoint, malformedRequires))
                                  .interpreter
                                  .exit
          provides           <- Gateway
                                  .compose(Subgraph.federation("reviews", unreachableEndpoint, invalidProvides))
                                  .interpreter
                                  .exit
          unknown            <- ZIO.foreach(unknownRequires)(schema =>
                                  Gateway.compose(Subgraph.federation("inventory", unreachableEndpoint, schema)).interpreter.exit
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
          products      <- stub(productResponse)
          inventory     <- stub(inventoryResponse)
          runtime       <- Gateway
                             .compose(
                               Subgraph.federation("products", products.endpoint, productsSchema),
                               Subgraph.federation("inventory", inventory.endpoint, inventorySchema)
                             )
                             .interpreter
          response      <- runtime.execute("{ product { shippingEstimate } }")
          productsSent  <- products.requests.get
          inventorySent <- inventory.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "shippingEstimate")).contains(IntNumber(220)),
          field(response.data, "product").forall(fieldNames(_) == List("shippingEstimate")),
          productsSent.headOption
            .flatMap(_.query)
            .exists(query => query.contains("price(multiplier:2)") && query.contains("weight")),
          inventorySent.headOption
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
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("shipping", shipping.endpoint, shippingSchema)
                        )
                        .interpreter
          response <- runtime.execute("{ product { shipping } }")
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
          products      <- stub(productResponse)
          inventory     <- stub(inventoryResponse)
          runtime       <- Gateway
                             .compose(
                               Subgraph
                                 .federation("products", products.endpoint, productsSchema)
                                 .transform(SchemaTransformation.renameType("PhysicalDetails", "PhysicalItem")),
                               Subgraph
                                 .federation("inventory", inventory.endpoint, inventorySchema)
                                 .transform(SchemaTransformation.renameType("PhysicalDetails", "PhysicalItem"))
                             )
                             .interpreter
          response      <- runtime.execute("{ product { shippingEstimate } }")
          productsSent  <- products.requests.get
          inventorySent <- inventory.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "shippingEstimate")).contains(IntNumber(40)),
          productsSent.headOption
            .flatMap(_.query)
            .exists(query => query.contains("...on PhysicalDetails{dimensions code}") && query.contains("__typename")),
          inventorySent.headOption
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
          products     <- stub(productsResponse)
          catalog      <- stub(catalogResponse)
          runtime      <- Gateway
                            .compose(
                              Subgraph.federation("products", products.endpoint, productsSchema),
                              Subgraph.federation("catalog", catalog.endpoint, catalogSchema)
                            )
                            .interpreter
          response     <- runtime.execute("{ featured { name } regular { name } }")
          productsSent <- products.requests.get
          catalogSent  <- catalog.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "featured").flatMap(field(_, "name")).contains(StringValue("Provided")),
          field(response.data, "regular").flatMap(field(_, "name")).contains(StringValue("Routed")),
          productsSent.headOption
            .flatMap(_.query)
            .exists(query => query.contains("featured{name}") && !query.contains("regular{name}")),
          catalogSent.size == 1
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
        val shippingSchema   = productRequiringPrice("shipping: Int!")
        val productsResponse =
          """{"data":{"featured":{"_caliban_gateway_requirement_price":10,"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},"regular":{"_caliban_gateway_key":"r1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"r1","_caliban_gateway_typename_2":"Product"}}}"""

        for {
          products            <- stub(productsResponse)
          pricing             <- stub("""{"data":{"_entities":[{"_caliban_gateway_requirement_price":20}]}}""")
          shipping            <- stubByRequest { request =>
                                   val entities = representations(request).map {
                                     case InputObjectValue(fields) if fields.get("price").contains(IntNumber(10)) =>
                                       """{"shipping":100}"""
                                     case _                                                                       =>
                                       """{"shipping":200}"""
                                   }
                                   s"""{"data":{"_entities":[${entities.mkString(",")}]}}"""
                                 }
          runtime             <- Gateway
                                   .compose(
                                     Subgraph.federation("products", products.endpoint, productsSchema),
                                     Subgraph.federation("pricing", pricing.endpoint, pricingSchema),
                                     Subgraph.federation("shipping", shipping.endpoint, shippingSchema)
                                   )
                                   .interpreter
          response            <- runtime.execute("{ featured { shipping } regular { shipping } }")
          productsSent        <- products.requests.get
          pricingSent         <- pricing.requests.get
          shippingSent        <- shipping.requests.get
          priceRepresentations = pricingSent.toList.flatMap(representations)
          shippingPrices       = shippingSent.toList
                                   .flatMap(representations)
                                   .collect { case InputObjectValue(fields) => fields.get("price") }
                                   .flatten
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "featured").flatMap(field(_, "shipping")).contains(IntNumber(100)),
          field(response.data, "regular").flatMap(field(_, "shipping")).contains(IntNumber(200)),
          productsSent.headOption
            .flatMap(_.query)
            .exists(query =>
              query.contains("featured{_caliban_gateway_requirement_price:price") &&
                !query.contains("regular{_caliban_gateway_requirement_price:price")
            ),
          priceRepresentations.size == 1,
          priceRepresentations.collect { case InputObjectValue(fields) => fields.get("id") }.flatten == List(
            StringValue("r1")
          ),
          shippingSent.size == 1,
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
          products      <- stub(productResponse)
          inventory     <- stubByRequest(request =>
                             if (request.query.exists(_.contains("shippingEstimateDouble")))
                               """{"data":{"_entities":[{"shippingEstimateDouble":40}]}}"""
                             else """{"data":{"_entities":[{"shippingEstimate":20}]}}"""
                           )
          runtime       <- Gateway
                             .compose(
                               Subgraph.federation("products", products.endpoint, productsSchema),
                               Subgraph.federation("inventory", inventory.endpoint, inventorySchema)
                             )
                             .interpreter
          response      <- runtime.execute("{ product { shippingEstimate shippingEstimateDouble } }")
          productsSent  <- products.requests.get
          inventorySent <- inventory.requests.get
          values         = field(response.data, "product")
          prices         = inventorySent
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
          productsSent.headOption
            .flatMap(_.query)
            .exists(query => query.contains("price(multiplier:1)") && query.contains("price(multiplier:2)")),
          inventorySent.size == 2,
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
        val shippingSchema  = productRequiringPrice("shipping: Int!")
        val taxSchema       = productRequiringPrice("tax: Int!")
        val productResponse =
          """{"data":{"product":{"_caliban_gateway_requirement_price":10,"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product"}}}"""

        for {
          products     <- stub(productResponse)
          shipping     <- stub("""{"data":{"_entities":[{"shipping":20}]}}""")
          tax          <- stub("""{"data":{"_entities":[{"tax":3}]}}""")
          runtime      <- Gateway
                            .compose(
                              Subgraph.federation("products", products.endpoint, productsSchema),
                              Subgraph.federation("shipping", shipping.endpoint, shippingSchema),
                              Subgraph.federation("tax", tax.endpoint, taxSchema)
                            )
                            .interpreter
          response     <- runtime.execute("{ product { shipping tax } }")
          productsSent <- products.requests.get
          shippingSent <- shipping.requests.get
          taxSent      <- tax.requests.get
          query         = productsSent.headOption.flatMap(_.query).getOrElse("")
          alias         = "_caliban_gateway_requirement_price:price"
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "shipping")).contains(IntNumber(20)),
          field(response.data, "product").flatMap(field(_, "tax")).contains(IntNumber(3)),
          query.sliding(alias.length).count(_ == alias) == 1,
          !query.contains("_caliban_gateway_requirement_price_2"),
          shippingSent.size == 1,
          taxSent.size == 1
        )
      },
      test("orders recursive requirements before their dependents") {
        val ratingSchema   = productRequiringPrice("expensive: Boolean!")
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
          roots       <- stub(productRootResponse)
          prices      <- stub(priceResponse)
          ratings     <- stub(ratingResponse)
          labels      <- stub("""{"data":{"_entities":[{"label":"premium"}]}}""")
          runtime     <- Gateway
                           .compose(
                             Subgraph.federation("roots", roots.endpoint, productRootSchema),
                             Subgraph.federation("prices", prices.endpoint, productPriceSchema),
                             Subgraph.federation("ratings", ratings.endpoint, ratingSchema),
                             Subgraph.federation("labels", labels.endpoint, labelSchema)
                           )
                           .interpreter
          response    <- runtime.execute("{ product { expensive label } }")
          pricesSent  <- prices.requests.get
          ratingsSent <- ratings.requests.get
          labelsSent  <- labels.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          field(response.data, "product").flatMap(field(_, "label")).contains(StringValue("premium")),
          pricesSent.size == 1,
          ratingsSent.size == 1,
          ratingsSent.headOption.flatMap(_.variables).exists(_.toString.contains("100")),
          labelsSent.headOption.flatMap(_.variables).exists(_.toString.contains("true"))
        )
      },
      test("blocks dependent requirement routes while preserving independent root data") {
        val rootsSchema  =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { status: String! product: Product }
             |type Product @key(fields: "id") { id: ID! }
             |""".stripMargin
        val ratingSchema = productRequiringPrice("expensive: Boolean!")
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
          roots       <- stub(rootResponse)
          prices      <- stub("""{"data":{"_entities":[null]}}""")
          ratings     <- stub("""{"data":{"_entities":[{"_caliban_gateway_requirement_expensive":true}]}}""")
          labels      <- stub("""{"data":{"_entities":[{"label":"premium"}]}}""")
          runtime     <- Gateway
                           .compose(
                             Subgraph.federation("roots", roots.endpoint, rootsSchema),
                             Subgraph.federation("prices", prices.endpoint, productPriceSchema),
                             Subgraph.federation("ratings", ratings.endpoint, ratingSchema),
                             Subgraph.federation("labels", labels.endpoint, labelSchema)
                           )
                           .interpreter
          response    <- runtime.execute("{ status product { label } }")
          pricesSent  <- prices.requests.get
          ratingsSent <- ratings.requests.get
          labelsSent  <- labels.requests.get
        } yield assertTrue(
          field(response.data, "status").contains(StringValue("ok")),
          field(response.data, "product").contains(NullValue),
          response.errors.nonEmpty,
          pricesSent.size == 1,
          ratingsSent.isEmpty,
          labelsSent.isEmpty
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
          products     <- stubByRequest(request =>
                            if (request.query.exists(_.contains("_entities")))
                              """{"data":{"_entities":[{"label":"premium"}]}}"""
                            else rootResponse
                          )
          pricing      <- stub("""{"data":{"_entities":[{"_caliban_gateway_requirement_price":100}]}}""")
          runtime      <- Gateway
                            .compose(
                              Subgraph.federation("products", products.endpoint, productsSchema),
                              Subgraph.federation("pricing", pricing.endpoint, pricingSchema)
                            )
                            .interpreter
          response     <- runtime.execute("{ product { label } }")
          productsSent <- products.requests.get
          pricingSent  <- pricing.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "label")).contains(StringValue("premium")),
          productsSent.size == 2,
          productsSent.headOption.flatMap(_.query).forall(!_.contains("label")),
          pricingSent.size == 1,
          productsSent.lastOption.flatMap(_.variables).exists(_.toString.contains("100"))
        )
      }
    ),
    suite("shared keys and merged fetches")(
      test("shares one injected key alias between sibling entity fetches without fetching the typename") {
        val taxSchema =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |type Product @key(fields: "id") { id: ID! @external tax: Int! }
             |""".stripMargin

        for {
          roots    <- stub(productRootResponse)
          shipping <- stub("""{"data":{"_entities":[{"shipping":20}]}}""")
          tax      <- stub("""{"data":{"_entities":[{"tax":3}]}}""")
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("roots", roots.endpoint, productRootSchema),
                          Subgraph.federation("shipping", shipping.endpoint, productShippingSchema),
                          Subgraph.federation("tax", tax.endpoint, taxSchema)
                        )
                        .interpreter
          response <- runtime.execute("{ product { shipping tax } }")
          sent     <- roots.requests.get
          query     = sent.headOption.flatMap(_.query).getOrElse("")
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
              val names = representations(request).collect { case InputObjectValue(fields) =>
                (fields.get("__typename"), fields.get("id")) match {
                  case (Some(StringValue(typename)), Some(StringValue(id))) =>
                    s"""{"_caliban_gateway_entity_key":"$id","_caliban_gateway_entity_typename":"$typename","name":"$typename-$id"}"""
                  case _                                                    => "null"
                }
              }
              s"""{"data":{"_entities":[${names.mkString(",")}]}}"""
            }
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("roots", roots.endpoint, rootsSchema),
                          Subgraph.federation("details", details.endpoint, detailsSchema)
                        )
                        .interpreter
          response <- runtime.execute("{ nodes { ... on A { child { name } } ... on B { child { name } } } }")
          sent     <- roots.requests.get
          rendered  = response.data.toString
        } yield assertTrue(
          response.errors.isEmpty,
          sent.exists(_.query.exists(_.contains("_caliban_gateway_typename:__typename"))),
          rendered.contains("Product-p"),
          rendered.contains("User-u"),
          !rendered.contains("Product-u"),
          !rendered.contains("User-p")
        )
      },
      test("reuses a client alias whose selection matches an injected key") {
        for {
          roots    <- stub(productRootResponse)
          shipping <- stub("""{"data":{"_entities":[{"shipping":20}]}}""")
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("roots", roots.endpoint, productRootSchema),
                          Subgraph.federation("shipping", shipping.endpoint, productShippingSchema)
                        )
                        .interpreter
          response <- runtime.execute("{ product { _caliban_gateway_key: id shipping } }")
          sent     <- roots.requests.get
          query     = sent.headOption.flatMap(_.query).getOrElse("")
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
        val ratingSchema   = productRequiringPrice("expensive: Boolean!")
        val priceResponse  =
          """{"data":{"_entities":[{"price":100,"_caliban_gateway_requirement_price":100}]}}"""
        val ratingResponse = """{"data":{"_entities":[{"expensive":true}]}}"""

        for {
          roots       <- stub(productRootResponse)
          prices      <- stub(priceResponse)
          ratings     <- stub(ratingResponse)
          runtime     <- Gateway
                           .compose(
                             Subgraph.federation("roots", roots.endpoint, productRootSchema),
                             Subgraph.federation("prices", prices.endpoint, productPriceSchema),
                             Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                           )
                           .interpreter
          plan        <- runtime.explain(GraphQLRequest(query = Some("{ product { price expensive } }")))
          response    <- runtime.execute("{ product { price expensive } }")
          pricesSent  <- prices.requests.get
          ratingsSent <- ratings.requests.get
          priceQuery   = pricesSent.headOption.flatMap(_.query).getOrElse("")
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "product").flatMap(field(_, "price")).contains(IntNumber(100)),
          field(response.data, "product").flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          pricesSent.size == 1,
          priceQuery.contains("_caliban_gateway_requirement_price:price"),
          ratingsSent.size == 1,
          ratingsSent.headOption.flatMap(_.variables).exists(_.toString.contains("100")),
          plan.linesIterator.count(_.startsWith("fetch prices")) == 1
        )
      },
      test("keeps a prerequisite separate when its source subgraph declares it non-null but composition widens it") {
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
        val ratingSchema   = productRequiringPrice("expensive: Boolean")
        val priceResponse  =
          """{"data":{"_entities":[null]},"errors":[{"message":"price unavailable","path":["_entities",0,"_caliban_gateway_requirement_price"]}]}"""
        val ratingResponse = """{"data":{"_entities":[{"expensive":true}]}}"""

        for {
          roots         <- stub(productRootResponse)
          prices        <- stubByRequest(request =>
                             if (request.query.exists(_.contains("_caliban_gateway_requirement_price"))) priceResponse
                             else """{"data":{"_entities":[{"displayPrice":10}]}}"""
                           )
          catalog       <- stub(priceResponse)
          ratings       <- stub(ratingResponse)
          runtime       <- Gateway
                             .compose(
                               Subgraph.federation("roots", roots.endpoint, productRootSchema),
                               Subgraph.federation("prices", prices.endpoint, priceSchema),
                               Subgraph.federation("catalog", catalog.endpoint, catalogSchema),
                               Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                             )
                             .interpreter
          introspection <- runtime.execute("""{ __type(name: "Product") { fields { name type { kind } } } }""")
          response      <- runtime.execute("{ product { displayPrice expensive } }")
          pricesSent    <- prices.requests.get
          catalogSent   <- catalog.requests.get
          ratingsSent   <- ratings.requests.get
          product        = field(response.data, "product")
          priceKind      = fieldKind(introspection.data, "price")
        } yield assertTrue(
          priceKind.contains(EnumValue("SCALAR")),
          product.flatMap(field(_, "displayPrice")).contains(IntNumber(10)),
          product.flatMap(field(_, "expensive")).contains(NullValue),
          response.errors.nonEmpty,
          pricesSent.size + catalogSent.size == 2,
          ratingsSent.isEmpty
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
        val ratingSchema    = productRequiringPrice("expensive: Boolean", "Int")
        val displayResponse =
          """{"data":{"_entities":[null]},"errors":[{"message":"displayPrice unavailable","path":["_entities",0,"displayPrice"]}]}"""
        val priceResponse   = """{"data":{"_entities":[{"_caliban_gateway_requirement_price":100}]}}"""
        val ratingResponse  = """{"data":{"_entities":[{"expensive":true}]}}"""

        for {
          roots         <- stub(productRootResponse)
          prices        <- stubByRequest(request =>
                             if (request.query.exists(_.contains("displayPrice"))) displayResponse else priceResponse
                           )
          catalog       <- stub(displayResponse)
          ratings       <- stub(ratingResponse)
          runtime       <- Gateway
                             .compose(
                               Subgraph.federation("roots", roots.endpoint, productRootSchema),
                               Subgraph.federation("prices", prices.endpoint, priceSchema),
                               Subgraph.federation("catalog", catalog.endpoint, catalogSchema),
                               Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                             )
                             .interpreter
          introspection <- runtime.execute("""{ __type(name: "Product") { fields { name type { kind } } } }""")
          response      <- runtime.execute("{ product { displayPrice expensive } }")
          pricesSent    <- prices.requests.get
          catalogSent   <- catalog.requests.get
          ratingsSent   <- ratings.requests.get
          product        = field(response.data, "product")
          displayKind    = fieldKind(introspection.data, "displayPrice")
        } yield assertTrue(
          displayKind.contains(EnumValue("SCALAR")),
          product.flatMap(field(_, "displayPrice")).contains(NullValue),
          product.flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          response.errors.nonEmpty,
          pricesSent.size + catalogSent.size == 2,
          ratingsSent.size == 1
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
          roots       <- stub(nodeRoot("Product", "p1"))
          prices      <- pricesByRequest
          ratings     <- stub(ratingResponse)
          runtime     <- Gateway
                           .compose(
                             Subgraph.federation("roots", roots.endpoint, nodeRoots),
                             Subgraph.federation("prices", prices.endpoint, priceSchema),
                             Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                           )
                           .interpreter
          plan        <- runtime.explain(GraphQLRequest(query = Some("{ node { displayPrice expensive } }")))
          response    <- runtime.execute("{ node { displayPrice expensive } }")
          pricesSent  <- prices.requests.get
          ratingsSent <- ratings.requests.get

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
          pricesSent.nonEmpty,
          pricesSent.forall(_.variables.exists(_.toString.contains("Product"))),
          ratingsSent.size == 1
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
          roots       <- stub(nodeRoot("Service", "s1"))
          prices      <- pricesByRequest
          ratings     <- stub(ratingResponse)
          runtime     <- Gateway
                           .compose(
                             Subgraph.federation("roots", roots.endpoint, nodeRoots),
                             Subgraph.federation("prices", prices.endpoint, priceSchema),
                             Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                           )
                           // Allow for CI contention while checking interface routing.
                           .withConfig(_.withPlanningTimeout(Duration.fromSeconds(30)))
                           .interpreter
          plan        <- runtime.explain(GraphQLRequest(query = Some("{ node { displayPrice expensive } }")))
          response    <- runtime.execute("{ node { displayPrice expensive } }")
          pricesSent  <- prices.requests.get
          ratingsSent <- ratings.requests.get
          rootsSent   <- roots.requests.get
          node         = field(response.data, "node")
        } yield assertTrue(
          plan.linesIterator.exists(line =>
            line.startsWith("fetch prices") && line.contains("via Node(") && line.contains("displayPrice")
          ),
          rootsSent.exists(_.query.exists(_.contains("_caliban_gateway_typename:__typename"))),
          response.errors.isEmpty,
          node.flatMap(field(_, "displayPrice")).contains(IntNumber(10)),
          node.flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          pricesSent.nonEmpty,
          pricesSent.forall(_.variables.exists(_.toString.contains("Service"))),
          ratingsSent.size == 1
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
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("users", users.endpoint, usersSchema),
                          Subgraph.federation("accounts", accounts.endpoint, accountsSchema)
                        )
                        .interpreter
          response <- runtime.execute("{ anotherUsers { ... on User { username } id name } }")
          user      = listValues(field(response.data, "anotherUsers")).headOption
        } yield assertTrue(
          response.errors.isEmpty,
          user.flatMap(field(_, "username")).contains(StringValue("u1-username")),
          user.flatMap(field(_, "id")).contains(StringValue("u1")),
          user.flatMap(field(_, "name")).contains(StringValue("u1-name"))
        )
      },
      test("resolves an interface object field selected through a concrete type") {
        val usersSchema =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { user: User }
             |interface NodeWithName @key(fields: "id") { id: ID! name: String }
             |type User implements NodeWithName @key(fields: "id") { id: ID! name: String }
             |""".stripMargin

        for {
          users    <- stub("""{"data":{"user":{"_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"}}}""")
          accounts <- stub("""{"data":{"_entities":[{"username":"u1-username"}]}}""")
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("users", users.endpoint, usersSchema),
                          Subgraph.federation("accounts", accounts.endpoint, nodeWithNameAccounts)
                        )
                        .interpreter
          response <- runtime.execute("{ user { username } }")
          sent     <- accounts.requests.get
          user      = field(response.data, "user")
        } yield assertTrue(
          response.errors.isEmpty,
          user.flatMap(field(_, "username")).contains(StringValue("u1-username")),
          sent.size == 1,
          sent.forall(_.query.exists(_.contains("...on NodeWithName{username"))),
          sent.forall(_.variables.exists(_.toString.contains("NodeWithName")))
        )
      },
      test("satisfies a requirement on an interface object field through a concrete type") {
        val usersSchema =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |type Query { user: User }
             |interface NodeWithName @key(fields: "id") { id: ID! name: String }
             |type User implements NodeWithName @key(fields: "id") {
             |  id: ID!
             |  name: String
             |  username: String @external
             |  display: String @requires(fields: "username")
             |}
             |""".stripMargin

        for {
          users    <- stubByRequest(request =>
                        if (request.query.exists(_.contains("_entities")))
                          """{"data":{"_entities":[{"display":"u1-display"}]}}"""
                        else """{"data":{"user":{"_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"}}}"""
                      )
          accounts <- stub("""{"data":{"_entities":[{"_caliban_gateway_requirement_username":"u1-username"}]}}""")
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("users", users.endpoint, usersSchema),
                          Subgraph.federation("accounts", accounts.endpoint, nodeWithNameAccounts)
                        )
                        .interpreter
          response <- runtime.execute("{ user { display } }")
          sent     <- users.requests.get
          user      = field(response.data, "user")
        } yield assertTrue(
          response.errors.isEmpty,
          user.flatMap(field(_, "display")).contains(StringValue("u1-display")),
          sent.size == 2,
          sent.exists(_.variables.exists(_.toString.contains("u1-username")))
        )
      },
      test("resolves an interface object field selected through a union member") {
        val usersSchema    =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { search: [SearchResult!]! }
             |union SearchResult = User | Team
             |interface NodeWithName @key(fields: "id") { id: ID! name: String }
             |type User implements NodeWithName @key(fields: "id") { id: ID! name: String }
             |type Team { id: ID! }
             |""".stripMargin
        val searchResponse =
          """{"data":{"search":[{"__typename":"User","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"},{"__typename":"Team","id":"t1","_caliban_gateway_typename":"Team"}]}}"""

        for {
          users    <- stub(searchResponse)
          accounts <- stub("""{"data":{"_entities":[{"username":"u1-username"}]}}""")
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("users", users.endpoint, usersSchema),
                          Subgraph.federation("accounts", accounts.endpoint, nodeWithNameAccounts)
                        )
                        .interpreter
          response <- runtime.execute("{ search { ... on User { username } ... on Team { id } } }")
          sent     <- accounts.requests.get
          results   = listValues(field(response.data, "search"))
        } yield assertTrue(
          response.errors.isEmpty,
          results.headOption.flatMap(field(_, "username")).contains(StringValue("u1-username")),
          results.lift(1).flatMap(field(_, "id")).contains(StringValue("t1")),
          sent.size == 1,
          sent.forall(_.query.exists(_.contains("...on NodeWithName{username"))),
          sent.forall(
            _.variables.exists(variables => variables.toString.contains("u1") && !variables.toString.contains("t1"))
          )
        )
      },
      test("splits union members between an interface object and a concrete entity of the same subgraph") {
        val usersSchema    =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { search: [SearchResult!]! }
             |union SearchResult = User | Team
             |interface NodeWithName @key(fields: "id") { id: ID! name: String }
             |type User implements NodeWithName @key(fields: "id") { id: ID! name: String }
             |type Team @key(fields: "id") { id: ID! }
             |""".stripMargin
        val accountsSchema = s"""$nodeWithNameAccounts\ntype Team @key(fields: "id") { id: ID! motto: String }"""
        val searchResponse =
          """{"data":{"search":[{"__typename":"User","_caliban_gateway_key_2":"u1","_caliban_gateway_typename":"User"},{"__typename":"Team","_caliban_gateway_key":"t1","_caliban_gateway_typename":"Team"}]}}"""

        for {
          users    <- stub(searchResponse)
          accounts <- stubByRequest { request =>
                        val query = request.query.getOrElse("")
                        val user  = """{"username":"u1-username"}"""
                        val team  = """{"motto":"go"}"""
                        if (query.contains("username") && query.contains("motto"))
                          s"""{"data":{"_entities":[$user,$team]}}"""
                        else if (query.contains("username")) s"""{"data":{"_entities":[$user]}}"""
                        else s"""{"data":{"_entities":[$team]}}"""
                      }
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("users", users.endpoint, usersSchema),
                          Subgraph.federation("accounts", accounts.endpoint, accountsSchema)
                        )
                        .interpreter
          response <- runtime.execute("{ search { ... on User { username } ... on Team { motto } } }")
          sent     <- accounts.requests.get
          queries   = sent.flatMap(_.query)
          results   = listValues(field(response.data, "search"))
        } yield assertTrue(
          response.errors.isEmpty,
          results.headOption.flatMap(field(_, "username")).contains(StringValue("u1-username")),
          results.lift(1).flatMap(field(_, "motto")).contains(StringValue("go")),
          queries.exists(_.contains("...on NodeWithName{username")),
          queries.exists(_.contains("...on Team{motto")),
          queries.forall(query => !query.contains("...on NodeWithName{motto") && !query.contains("...on Team{username"))
        )
      },
      test("keeps the fields of two interface objects of the same subgraph apart") {
        val usersSchema    =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { user: User }
             |interface NodeWithName @key(fields: "id") { id: ID! name: String }
             |interface Named @key(fields: "id") { id: ID! }
             |type User implements NodeWithName & Named @key(fields: "id") { id: ID! name: String }
             |""".stripMargin
        val accountsSchema =
          s"""$nodeWithNameAccounts\ntype Named @key(fields: "id") @interfaceObject { id: ID! nickname: String }"""

        for {
          users    <- stub("""{"data":{"user":{"_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"}}}""")
          accounts <- stubByRequest { request =>
                        if (request.query.exists(_.contains("username")))
                          """{"data":{"_entities":[{"username":"u1-username"}]}}"""
                        else """{"data":{"_entities":[{"nickname":"nick"}]}}"""
                      }
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("users", users.endpoint, usersSchema),
                          Subgraph.federation("accounts", accounts.endpoint, accountsSchema)
                        )
                        .interpreter
          response <- runtime.execute("{ user { username nickname } }")
          sent     <- accounts.requests.get
          queries   = sent.flatMap(_.query)
          user      = field(response.data, "user")
        } yield assertTrue(
          response.errors.isEmpty,
          user.flatMap(field(_, "username")).contains(StringValue("u1-username")),
          user.flatMap(field(_, "nickname")).contains(StringValue("nick")),
          queries.exists(_.contains("...on NodeWithName{username}")),
          queries.exists(_.contains("...on Named{nickname}")),
          queries.size == 2
        )
      },
      test("resolves an interface object field for every member of a union that implements it") {
        val usersSchema    =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { search: [SearchResult!]! }
             |union SearchResult = User | Admin
             |interface NodeWithName @key(fields: "id") { id: ID! name: String }
             |type User implements NodeWithName @key(fields: "id") { id: ID! name: String }
             |type Admin implements NodeWithName @key(fields: "id") { id: ID! name: String }
             |""".stripMargin
        val searchResponse =
          """{"data":{"search":[{"__typename":"User","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"},{"__typename":"Admin","_caliban_gateway_key":"a1","_caliban_gateway_typename":"Admin"}]}}"""

        for {
          users    <- stub(searchResponse)
          accounts <- stub("""{"data":{"_entities":[{"username":"u1-username"},{"username":"a1-username"}]}}""")
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("users", users.endpoint, usersSchema),
                          Subgraph.federation("accounts", accounts.endpoint, nodeWithNameAccounts)
                        )
                        .interpreter
          response <- runtime.execute("{ search { ... on User { username } ... on Admin { username } } }")
          sent     <- accounts.requests.get
          results   = listValues(field(response.data, "search"))
        } yield assertTrue(
          response.errors.isEmpty,
          results.headOption.flatMap(field(_, "username")).contains(StringValue("u1-username")),
          results.lift(1).flatMap(field(_, "username")).contains(StringValue("a1-username")),
          sent.size == 1,
          sent.forall(_.query.exists(_.contains("...on NodeWithName{username}")))
        )
      },
      test("resolves an interface object field through an unrelated interface parent") {
        val usersSchema   =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { nodes: [Node!]! }
             |interface Node { id: ID! }
             |interface NodeWithName @key(fields: "id") { id: ID! name: String }
             |type User implements Node & NodeWithName @key(fields: "id") { id: ID! name: String }
             |type Admin implements Node & NodeWithName @key(fields: "id") { id: ID! name: String }
             |""".stripMargin
        val nodesResponse =
          """{"data":{"nodes":[{"__typename":"User","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"},{"__typename":"Admin","_caliban_gateway_key":"a1","_caliban_gateway_typename":"Admin"}]}}"""

        for {
          users        <- stub(nodesResponse)
          accounts     <- stub("""{"data":{"_entities":[{"username":"u1-username"},{"username":"a1-username"}]}}""")
          runtime      <- Gateway
                            .compose(
                              Subgraph.federation("users", users.endpoint, usersSchema),
                              Subgraph.federation("accounts", accounts.endpoint, nodeWithNameAccounts)
                            )
                            .interpreter
          response     <- runtime.execute("{ nodes { ... on User { username } ... on Admin { username } } }")
          accountsSent <- accounts.requests.get
          usersSent    <- users.requests.get
          results       = listValues(field(response.data, "nodes"))
        } yield assertTrue(
          response.errors.isEmpty,
          results.headOption.flatMap(field(_, "username")).contains(StringValue("u1-username")),
          results.lift(1).flatMap(field(_, "username")).contains(StringValue("a1-username")),
          usersSent.size == 1,
          accountsSent.size == 1,
          accountsSent.forall(_.query.exists(_.contains("...on NodeWithName{username}")))
        )
      },
      test("merges a nullable prerequisite with client fields of the same subgraph") {
        val priceSchema    =
          s"""
             |${federationSchemaPreamble("@key", "@external")}
             |type Product @key(fields: "id") { id: ID! @external displayPrice: Int price: Int }
             |""".stripMargin
        val ratingSchema   = productRequiringPrice("expensive: Boolean", "Int")
        val priceResponse  =
          """{"data":{"_entities":[{"displayPrice":10,"_caliban_gateway_requirement_price":100}]}}"""
        val ratingResponse = """{"data":{"_entities":[{"expensive":true}]}}"""

        for {
          roots       <- stub(productRootResponse)
          prices      <- stub(priceResponse)
          ratings     <- stub(ratingResponse)
          runtime     <- Gateway
                           .compose(
                             Subgraph.federation("roots", roots.endpoint, productRootSchema),
                             Subgraph.federation("prices", prices.endpoint, priceSchema),
                             Subgraph.federation("ratings", ratings.endpoint, ratingSchema)
                           )
                           .interpreter
          response    <- runtime.execute("{ product { displayPrice expensive } }")
          pricesSent  <- prices.requests.get
          ratingsSent <- ratings.requests.get
          product      = field(response.data, "product")
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "displayPrice")).contains(IntNumber(10)),
          product.flatMap(field(_, "expensive")).contains(BooleanValue(true)),
          pricesSent.size == 1,
          ratingsSent.size == 1
        )
      },
      test("merges identical prerequisite fetches of different consumers into one call") {
        val shippingSchema = productRequiringPrice("shipping: Int!")
        val taxSchema      = productRequiringPrice("tax: Int!")
        val priceResponse  = """{"data":{"_entities":[{"_caliban_gateway_requirement_price":100}]}}"""

        for {
          roots        <- stub(productRootResponse)
          prices       <- stub(priceResponse)
          shipping     <- stub("""{"data":{"_entities":[{"shipping":20}]}}""")
          tax          <- stub("""{"data":{"_entities":[{"tax":3}]}}""")
          runtime      <- Gateway
                            .compose(
                              Subgraph.federation("roots", roots.endpoint, productRootSchema),
                              Subgraph.federation("prices", prices.endpoint, productPriceSchema),
                              Subgraph.federation("shipping", shipping.endpoint, shippingSchema),
                              Subgraph.federation("tax", tax.endpoint, taxSchema)
                            )
                            .interpreter
          response     <- runtime.execute("{ product { shipping tax } }")
          pricesSent   <- prices.requests.get
          shippingSent <- shipping.requests.get
          taxSent      <- tax.requests.get
          product       = field(response.data, "product")
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "shipping")).contains(IntNumber(20)),
          product.flatMap(field(_, "tax")).contains(IntNumber(3)),
          pricesSent.size == 1,
          shippingSent.size == 1,
          taxSent.size == 1
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

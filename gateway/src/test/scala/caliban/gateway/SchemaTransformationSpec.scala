package caliban.gateway

import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ EnumValue, IntValue, NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.SchemaCoordinateMapping
import caliban.parsing.Parser
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
import caliban.tools.RemoteSchema
import caliban.{ graphQL, CalibanError, PathValue, RootResolver }
import sttp.model.Uri
import zio._
import zio.test._

object SchemaTransformationSpec extends ZIOSpecDefault {

  private object LocalApi extends GenericSchema[Any] {
    import auto._

    sealed trait State
    object State {
      case object ACTIVE extends State
    }

    final case class EchoArgs(input: String)
    final case class EchoResult(value: String, state: State)
    final case class Query(echo: EchoArgs => EchoResult)

    implicit val stateSchema: Schema[Any, State]           = gen
    implicit val echoArgsBuilder: ArgBuilder[EchoArgs]     = ArgBuilder.gen
    implicit val echoResultSchema: Schema[Any, EchoResult] = gen
    implicit val querySchema: Schema[Any, Query]           = gen

    val api = graphQL(RootResolver(Query(args => EchoResult(args.input, State.ACTIVE))))
  }

  private val remoteSchema =
    """
      |type Query {
      |  product(id: ID!, filter: Filter): Product
      |  hiddenRoot: String
      |}
      |type Product {
      |  id: ID!
      |  name: String!
      |  secret: String
      |  status: Status!
      |}
      |type Internal { value: String }
      |input Filter { term: String hidden: String }
      |enum Status { ACTIVE LEGACY }
      |""".stripMargin

  private val remoteTransformations = List(
    SchemaTransformation.renameType("Product", "Item"),
    SchemaTransformation.renameField("Query", "product", "item"),
    SchemaTransformation.renameField("Product", "name", "title"),
    SchemaTransformation.renameArgument("Query", "product", "id", "sku"),
    SchemaTransformation.renameInputField("Filter", "term", "query"),
    SchemaTransformation.renameEnumValue("Status", "ACTIVE", "AVAILABLE"),
    SchemaTransformation.hideType("Internal"),
    SchemaTransformation.hideField("Query", "hiddenRoot"),
    SchemaTransformation.hideField("Product", "secret"),
    SchemaTransformation.hideInputField("Filter", "hidden"),
    SchemaTransformation.hideEnumValue("Status", "LEGACY")
  )

  def spec = suite("SchemaTransformationSpec")(
    test("does not forward field-definition directives after a field rename") {
      val schema =
        "directive @sql(fields: String!) on FIELD_DEFINITION type Query { product: Product @sql(fields: \"name\") } type Product { name: String }"

      for {
        remote   <- stub("""{"data":{"item":{"name":"Table"}}}""")
        gateway  <- Gateway
                      .compose(
                        Subgraph
                          .graphql("products", remote.endpoint, schema)
                          .transform(SchemaTransformation.renameField("Query", "product", "item"))
                      )
                      .build
        result   <- gateway.execute("{ item { name } }")
        requests <- remote.requests.get
      } yield assertTrue(
        result.errors.isEmpty,
        requests.headOption.flatMap(_.query).exists(query => query.contains("item:product") && !query.contains("@sql"))
      )
    },
    test("renames and hides remote schema coordinates while translating execution") {
      val response = """{"data":{"item":{"id":"p1","title":"Table","status":"ACTIVE"}}}"""

      for {
        remote     <- stub(response)
        gateway    <- Gateway
                        .compose(
                          Subgraph
                            .graphql("products", remote.endpoint, remoteSchema)
                            .transform(remoteTransformations: _*)
                        )
                        .build
        result     <- gateway.execute(
                        """{
                      |  item(sku: "p1", filter: { query: "wood" }) { id title status }
                      |  itemType: __type(name: "Item") { fields { name } }
                      |  sourceType: __type(name: "Product") { name }
                      |  hiddenType: __type(name: "Internal") { name }
                      |  filterType: __type(name: "Filter") { inputFields { name } }
                      |  stateType: __type(name: "Status") { enumValues { name } }
                      |}""".stripMargin
                      )
        requests   <- remote.requests.get
        itemFields  = field(result.data, "itemType")
                        .flatMap(field(_, "fields"))
                        .collect { case ListValue(values) => values.flatMap(field(_, "name")) }
        inputFields = field(result.data, "filterType")
                        .flatMap(field(_, "inputFields"))
                        .collect { case ListValue(values) => values.flatMap(field(_, "name")) }
        enumValues  = field(result.data, "stateType")
                        .flatMap(field(_, "enumValues"))
                        .collect { case ListValue(values) => values.flatMap(field(_, "name")) }
      } yield assertTrue(
        result.errors.isEmpty,
        field(result.data, "item").flatMap(field(_, "title")).contains(StringValue("Table")),
        field(result.data, "item").flatMap(field(_, "status")).contains(StringValue("AVAILABLE")),
        itemFields.contains(List(StringValue("id"), StringValue("status"), StringValue("title"))),
        inputFields.contains(List(StringValue("query"))),
        enumValues.contains(List(StringValue("AVAILABLE"))),
        field(result.data, "sourceType").contains(NullValue),
        field(result.data, "hiddenType").contains(NullValue),
        requests.size == 1,
        requests.head.query.exists(query =>
          query.contains("item:product(id:\"p1\",filter:{term:\"wood\"})") &&
            query.contains("title:name") && !query.contains("__type")
        )
      )
    },
    test("reverses enum and input-field renames for singleton list coercion") {
      val schema =
        "type Query { search(statuses: [Status!]!, filters: [Filter!]!): String } input Filter { term: String! } enum Status { ACTIVE }"

      for {
        remote   <- stub("""{"data":{"search":"ok"}}""")
        gateway  <- Gateway
                      .compose(
                        Subgraph
                          .graphql("search", remote.endpoint, schema)
                          .transform(
                            SchemaTransformation.renameEnumValue("Status", "ACTIVE", "AVAILABLE"),
                            SchemaTransformation.renameInputField("Filter", "term", "query")
                          )
                      )
                      .build
        result   <- gateway.execute("{ search(statuses: AVAILABLE, filters: { query: \"wood\" }) }")
        requests <- remote.requests.get
      } yield assertTrue(
        result.errors.isEmpty,
        field(result.data, "search").contains(StringValue("ok")),
        requests.headOption
          .flatMap(_.query)
          .exists(
            _.contains("search(statuses:ACTIVE,filters:{term:\"wood\"})")
          )
      )
    },
    test("uses the same coordinate translation for local subgraphs") {
      val transformations = List(
        SchemaTransformation.renameType("EchoResult", "Reply"),
        SchemaTransformation.renameField("Query", "echo", "say"),
        SchemaTransformation.renameArgument("Query", "echo", "input", "message"),
        SchemaTransformation.renameField("EchoResult", "value", "text"),
        SchemaTransformation.renameEnumValue("State", "ACTIVE", "READY")
      )

      for {
        gateway <- Gateway
                     .compose(Subgraph.local("echo", LocalApi.api).transform(transformations: _*))
                     .build
        result  <- gateway.execute("{ say(message: \"hello\") { text state } }")
      } yield assertTrue(
        result.errors.isEmpty,
        field(result.data, "say").exists {
          case ObjectValue(values) =>
            values.contains("text" -> StringValue("hello")) && values.contains("state" -> EnumValue("READY"))
          case _                   => false
        }
      )
    },
    test("keeps source errors on transformed client coordinates") {
      val response =
        """{"data":{"item":{"title":null}},"errors":[{"message":"unavailable","path":["item","title"]}]}"""

      for {
        remote  <- stub(response)
        gateway <- Gateway
                     .compose(
                       Subgraph
                         .graphql("products", remote.endpoint, remoteSchema)
                         .transform(
                           SchemaTransformation.renameField("Query", "product", "item"),
                           SchemaTransformation.renameField("Product", "name", "title")
                         )
                     )
                     .build
        result  <- gateway.execute("{ item(id: \"p1\") { title } }")
      } yield assertTrue(
        result.errors.collectFirst { case error: CalibanError.ExecutionError => error.path }.contains(
          List(PathValue.Key("item"), PathValue.Key("title"))
        )
      )
    },
    test("reverse-maps transformed fields on a custom source root") {
      val schema =
        "schema { query: RootQuery } type RootQuery { product(id: ID!): Product } type Product { name: String! }"

      for {
        remote   <- stub("""{"data":{"item":{"name":"Table"}}}""")
        gateway  <- Gateway
                      .compose(
                        Subgraph
                          .graphql("products", remote.endpoint, schema)
                          .transform(
                            SchemaTransformation.renameField("RootQuery", "product", "item"),
                            SchemaTransformation.renameArgument("RootQuery", "product", "id", "sku")
                          )
                      )
                      .build
        result   <- gateway.execute("{ item(sku: \"p1\") { name } }")
        requests <- remote.requests.get
      } yield assertTrue(
        result.errors.isEmpty,
        field(result.data, "item").flatMap(field(_, "name")).contains(StringValue("Table")),
        requests.headOption.flatMap(_.query).exists(_.contains("item:product(id:\"p1\")"))
      )
    },
    test("rewrites only typed directive arguments and Federation field sets") {
      val schema          =
        "directive @sql(fields: String!) on FIELD_DEFINITION directive @flag(status: Status!) on FIELD_DEFINITION type Query { product: Product @sql(fields: \"name\") @flag(status: ACTIVE) } type Product { name: String } enum Status { ACTIVE }"
      val transformations = List(
        SchemaTransformation.renameField("Product", "name", "title"),
        SchemaTransformation.renameEnumValue("Status", "ACTIVE", "READY")
      )

      for {
        document   <- ZIO.fromEither(Parser.parseQuery(schema))
        rootType   <- ZIO.fromEither(RemoteSchema.toRootType(document))
        mapping    <- ZIO.fromEither(
                        SchemaCoordinateMapping.compile(
                          "products",
                          rootType,
                          document,
                          federation = false,
                          transformations = transformations
                        )
                      )
        transformed = mapping.transform(document)
        directives  = transformed.objectTypeDefinitions
                        .find(_.name == "Query")
                        .flatMap(_.fields.find(_.name == "product"))
                        .toList
                        .flatMap(_.directives)
        sqlFields   = directives.find(_.name == "sql").flatMap(_.arguments.get("fields"))
        flagStatus  = directives.find(_.name == "flag").flatMap(_.arguments.get("status"))
      } yield assertTrue(
        sqlFields.contains(StringValue("name")),
        flagStatus.contains(EnumValue("READY"))
      )
    },
    test("transforms enum defaults declared by custom directives") {
      val schema =
        "directive @flag(status: Status = ACTIVE) on FIELD_DEFINITION type Query { product: Product } type Product { name: String @flag(status: ACTIVE) } enum Status { ACTIVE }"

      for {
        remote   <- stub("""{"data":{"product":{"name":"Table"}}}""")
        gateway  <- Gateway
                      .compose(
                        Subgraph
                          .graphql("directives", remote.endpoint, schema)
                          .transform(SchemaTransformation.renameEnumValue("Status", "ACTIVE", "AVAILABLE"))
                      )
                      .build
        result   <- gateway.execute("{ __schema { directives { name args { name defaultValue } } } }")
        value    <- gateway.execute("{ product { name } }")
        requests <- remote.requests.get
        default   = field(result.data, "__schema")
                      .flatMap(field(_, "directives"))
                      .collect { case ListValue(values) => values }
                      .flatMap(_.find(field(_, "name").contains(StringValue("flag"))))
                      .flatMap(field(_, "args"))
                      .collect { case ListValue(values) => values }
                      .flatMap(_.find(field(_, "name").contains(StringValue("status"))))
                      .flatMap(field(_, "defaultValue"))
      } yield assertTrue(
        result.errors.isEmpty,
        value.errors.isEmpty,
        default.contains(StringValue("AVAILABLE")),
        requests.headOption.flatMap(_.query).exists(!_.contains("@flag"))
      )
    },
    test("uses an aliased provides directive's output type for field-set transformations") {
      val directives    = federationDirectives.replace("directive @provides", "directive @supplies")
      val catalogSchema =
        s"""
           |schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.3"
           |  import: ["@key", "@external", { name: "@provides", as: "@supplies" }]
           |) { query: Query }
           |$directives
           |type Query { product: Product }
           |type Product @key(fields: "id") {
           |  id: ID!
           |  price: Int!
           |  details: Details @supplies(fields: "price")
           |}
           |type Details { price: Int! @external }
           |""".stripMargin
      val detailsSchema =
        s"""
           |${federationSchemaPreambleWithQueryRoot()}
           |type Query { status: String }
           |type Details { price: Int! }
           |""".stripMargin
      val endpoint      = unreachableEndpoint

      Gateway
        .compose(
          Subgraph
            .federation("catalog", endpoint, catalogSchema)
            .transform(
              SchemaTransformation.renameField("Product", "price", "productCost"),
              SchemaTransformation.renameField("Details", "price", "detailCost")
            ),
          Subgraph
            .federation("details", endpoint, detailsSchema)
            .transform(SchemaTransformation.renameField("Details", "price", "detailCost"))
        )
        .build
        .as(assertTrue(true))
    },
    test("keeps Federation keys and requirements aligned with transformed coordinates") {
      val productsSchema  =
        s"""
           |schema { query: Queries }
           |${federationSchemaPreamble("@key", "@external", "@provides")}
           |type Queries { product(id: ID!): Product @provides(fields: "price") }
           |type Product @key(fields: "id") { id: ID! price: Int! @external }
           |""".stripMargin
      val pricesSchema    =
        s"""
           |${federationSchemaPreamble("@key")}
           |type Product @key(fields: "id") { id: ID! price: Int! }
           |""".stripMargin
      val shippingSchema  =
        s"""
           |${federationSchemaPreamble("@key", "@external", "@requires")}
           |type Product @key(fields: "id") {
           |  id: ID! @external
           |  price: Int! @external
           |  shippingEstimate: Int! @requires(fields: "price")
           |}
           |""".stripMargin
      val transformations = List(
        SchemaTransformation.renameType("Product", "Item"),
        SchemaTransformation.renameField("Product", "id", "sku"),
        SchemaTransformation.renameField("Product", "price", "cost")
      )

      for {
        products      <-
          stub(
            """{"data":{"product":{"sku":"p1","cost":100,"_caliban_gateway_requirement_cost":100,"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
          )
        shipping      <- stub("""{"data":{"_entities":[{"shipping":12}]}}""")
        prices        <- stub("""{"data":{"_entities":[{"price":100}]}}""")
        gateway       <- Gateway
                           .compose(
                             Subgraph
                               .federation("products", products.endpoint, productsSchema)
                               .transform(transformations: _*),
                             Subgraph
                               .federation("shipping", shipping.endpoint, shippingSchema)
                               .transform(
                                 (transformations :+ SchemaTransformation
                                   .renameField("Product", "shippingEstimate", "shipping")): _*
                               ),
                             Subgraph
                               .federation("prices", prices.endpoint, pricesSchema)
                               .transform(transformations: _*)
                           )
                           .build
        response      <- gateway.execute("{ product(id: \"p1\") { sku cost shipping } }")
        requests      <- shipping.requests.get
        priceRequests <- prices.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        priceRequests.isEmpty,
        field(response.data, "product").flatMap(field(_, "shipping")).contains(IntValue(12)),
        requests.headOption
          .flatMap(_.variables)
          .exists(
            _.get("representations").contains(
              InputListValue(
                List(
                  InputObjectValue(
                    Map(
                      "__typename" -> StringValue("Product"),
                      "id"         -> StringValue("p1"),
                      "price"      -> IntValue(100)
                    )
                  )
                )
              )
            )
          ),
        requests.headOption
          .flatMap(_.query)
          .exists(query => query.contains("...on Product") && query.contains("shipping:shippingEstimate"))
      )
    },
    test("translates ordinary lookup metadata, arguments, correlation fields, and results") {
      val productsSchema    =
        "type Query { products: [Product!]! } type Product { id: ID! name: String! }"
      val reviewsSchema     =
        """
          |input ProductRefInput { productId: ID! }
          |type Query { productsByRefs(refs: [ProductRefInput!]!): [Product!]! }
          |type Product { id: ID! reviews: [Review!]! }
          |type Review { body: String! }
          |""".stripMargin
      val lookup            = Lookup.list(
        "Product",
        List("id"),
        "productsByRefs",
        Lookup.Correlation.byKey(Map("id" -> "id")),
        "refs" -> Lookup.Argument.batch(
          Lookup.Argument.obj("productId" -> Lookup.Argument.key("id"))
        )
      )
      val productTransforms = List(
        SchemaTransformation.renameType("Product", "Item"),
        SchemaTransformation.renameField("Product", "id", "sku")
      )
      val reviewTransforms  = productTransforms ::: List(
        SchemaTransformation.renameType("ProductRefInput", "ItemRef"),
        SchemaTransformation.renameInputField("ProductRefInput", "productId", "sku"),
        SchemaTransformation.renameField("Query", "productsByRefs", "itemsByRefs"),
        SchemaTransformation.renameArgument("Query", "productsByRefs", "refs", "keys"),
        SchemaTransformation.renameField("Product", "reviews", "feedback")
      )

      for {
        products <- stub(
                      """{"data":{"products":[{"name":"Table","_caliban_gateway_key":"p1"}]}}"""
                    )
        reviews  <-
          stub(
            """{"data":{"_caliban_gateway_lookup":[{"_caliban_gateway_lookup_key":"p1","feedback":[{"body":"Solid"}]}]}}"""
          )
        gateway  <- Gateway
                      .compose(
                        Subgraph
                          .graphql("products", products.endpoint, productsSchema)
                          .transform(productTransforms: _*),
                        Subgraph
                          .graphql("reviews", reviews.endpoint, reviewsSchema)
                          .withLookup(lookup)
                          .transform(reviewTransforms: _*)
                      )
                      .build
        response <- gateway.execute("{ products { name feedback { body } } }")
        requests <- reviews.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "products").collect { case ListValue(value :: Nil) => value }
          .flatMap(field(_, "feedback"))
          .collect { case ListValue(value :: Nil) => value }
          .flatMap(field(_, "body"))
          .contains(StringValue("Solid")),
        requests.headOption
          .flatMap(_.query)
          .exists(query =>
            query.contains("_caliban_gateway_lookup:productsByRefs(refs:[{productId:\"p1\"}])") &&
              query.contains("feedback:reviews")
          )
      )
    },
    test("attributes transformations that leave composed types structurally invalid") {
      val schema   =
        "type Query { empty: Empty state: Status search(input: Only): String } type Empty { value: String } input Only { value: String } enum Status { ACTIVE }"
      val endpoint = unreachableEndpoint

      Gateway
        .compose(
          Subgraph
            .graphql("products", endpoint, schema)
            .transform(
              SchemaTransformation.hideField("Empty", "value"),
              SchemaTransformation.hideInputField("Only", "value"),
              SchemaTransformation.hideEnumValue("Status", "ACTIVE")
            )
        )
        .build
        .exit
        .map { exit =>
          val diagnostics = buildDiagnostics(exit)
          assertTrue(
            diagnostics.forall(_.startsWith("[products]")),
            diagnostics.exists(_.contains("object 'Empty' with no visible fields")),
            diagnostics.exists(_.contains("input object 'Only' with no visible fields")),
            diagnostics.exists(_.contains("enum 'Status' with no visible values"))
          )
        }
    },
    test("rejects transformed Federation transport coordinates") {
      val schema   =
        s"""
           |${federationSchemaPreambleWithQueryRoot("@key")}
           |type Query { product: Product }
           |type Product @key(fields: "id") { id: ID! }
           |""".stripMargin
      val endpoint = unreachableEndpoint

      Gateway
        .compose(
          Subgraph
            .federation("products", endpoint, schema)
            .transform(
              SchemaTransformation.renameType("_Any", "Representation"),
              SchemaTransformation.renameType("Product", "_Entity"),
              SchemaTransformation.renameField("Query", "_entities", "entities"),
              SchemaTransformation.renameField("Query", "product", "_service"),
              SchemaTransformation.renameArgument("Query", "_entities", "representations", "values")
            )
        )
        .build
        .exit
        .map { exit =>
          val diagnostics = buildDiagnostics(exit)
          assertTrue(
            diagnostics.forall(_.startsWith("[products]")),
            diagnostics.exists(_.contains("Federation transport type '_Any' cannot be transformed")),
            diagnostics.exists(_.contains("Federation transport field 'Query._entities' cannot be transformed")),
            diagnostics.exists(_.contains("cannot be transformed to reserved Federation transport type '_Entity'")),
            diagnostics.exists(_.contains("cannot be transformed to reserved Federation transport field '_service'"))
          )
        }
    },
    test("allows transport-like coordinate names in ordinary GraphQL schemas") {
      val schema =
        "type Query { _entities: String _service: _Any } type _Any { value: String }"

      for {
        remote  <- stub("""{"data":{"entities":"ok","service":{"value":"value"}}}""")
        gateway <- Gateway
                     .compose(
                       Subgraph
                         .graphql("ordinary", remote.endpoint, schema)
                         .transform(
                           SchemaTransformation.renameField("Query", "_entities", "entities"),
                           SchemaTransformation.renameField("Query", "_service", "service"),
                           SchemaTransformation.renameType("_Any", "AnyValue")
                         )
                     )
                     .build
        result  <- gateway.execute("{ entities service { value } }")
      } yield assertTrue(result.errors.isEmpty)
    },
    test("rejects hidden enum values referenced by directives or defaults") {
      val schema   =
        "directive @flag(status: Status = ACTIVE) on FIELD_DEFINITION type Query { value(state: Status = ACTIVE): String @flag(status: ACTIVE) } enum Status { ACTIVE OTHER }"
      val endpoint = unreachableEndpoint

      Gateway
        .compose(
          Subgraph
            .graphql("products", endpoint, schema)
            .transform(SchemaTransformation.hideEnumValue("Status", "ACTIVE"))
        )
        .build
        .exit
        .map { exit =>
          val diagnostics = buildDiagnostics(exit)
          assertTrue(
            diagnostics.exists(
              _.contains("Hidden enum value 'Status.ACTIVE' is referenced by a directive or default value")
            )
          )
        }
    },
    test("rejects hidden input fields referenced by directives or defaults") {
      val schema   =
        "directive @flag(filter: Filter = { hidden: \"directive-default\" }) on FIELD_DEFINITION input Filter { visible: String hidden: String } type Query { value(filter: Filter = { hidden: \"field-default\" }): String @flag(filter: { hidden: \"applied\" }) }"
      val endpoint = unreachableEndpoint

      Gateway
        .compose(
          Subgraph
            .graphql("products", endpoint, schema)
            .transform(SchemaTransformation.hideInputField("Filter", "hidden"))
        )
        .build
        .exit
        .map { exit =>
          val diagnostics = buildDiagnostics(exit)
          assertTrue(
            diagnostics.exists(
              _.contains("Hidden input field 'Filter.hidden' is referenced by a directive or default value")
            )
          )
        }
    },
    test("rejects invalid and colliding transformations with source diagnostics") {
      val endpoint = unreachableEndpoint
      val subgraph = Subgraph
        .graphql("products", endpoint, remoteSchema)
        .transform(
          SchemaTransformation.renameField("Product", "name", "id"),
          SchemaTransformation.hideField("Product", "name"),
          SchemaTransformation.renameType("Product", "Query"),
          SchemaTransformation.renameType("Query", "EntryPoint"),
          SchemaTransformation.renameArgument("Query", "product", "missing", "value"),
          SchemaTransformation.hideArgument("Query", "product", "id"),
          SchemaTransformation.hideType("Query"),
          SchemaTransformation.renameEnumValue("Status", "ACTIVE", "a b"),
          SchemaTransformation.renameEnumValue("Status", "LEGACY", "__LEGACY")
        )

      Gateway
        .compose(subgraph)
        .build
        .exit
        .map { exit =>
          val diagnostics = buildDiagnostics(exit)
          assertTrue(
            diagnostics.forall(_.startsWith("[products]")),
            diagnostics.exists(_.contains("Field 'Product.name' is transformed to existing field 'id'")),
            diagnostics.exists(_.contains("Coordinate 'Product.name' has conflicting transformations")),
            diagnostics.exists(_.contains("Type 'Product' is transformed to existing type 'Query'")),
            diagnostics.exists(_.contains("Operation root type 'Query' cannot be renamed")),
            diagnostics.exists(_.contains("Argument 'Query.product(missing:)' does not exist")),
            diagnostics.exists(_.contains("Required argument 'Query.product(id:)' cannot be hidden")),
            diagnostics.exists(_.contains("Operation root type 'Query' cannot be hidden")),
            diagnostics.exists(_.contains("invalid GraphQL name 'a b'")),
            diagnostics.exists(_.contains("reserved GraphQL name '__LEGACY'"))
          )
        }
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

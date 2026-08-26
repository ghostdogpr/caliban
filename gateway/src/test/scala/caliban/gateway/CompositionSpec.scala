package caliban.gateway

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ FloatValue, IntValue, NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.{ SchemaComposition, SchemaContribution, SchemaCoordinateMapping }
import caliban.introspection.adt.{ __Directive, __DirectiveLocation }
import caliban.parsing.{ Parser, SourceMapper }
import caliban.parsing.adt.{ Directive, Document }
import caliban.schema.{ RootType, Types }
import caliban.tools.RemoteSchema
import sttp.model.Uri
import zio._
import zio.test._

object CompositionSpec extends ZIOSpecDefault {

  private final case class CompositionInput(
    name: String,
    schema: String,
    transformations: List[SchemaTransformation] = Nil
  )

  private val endpoint = unreachableEndpoint

  private def schema(body: String, imports: String*): String =
    federationSchemaPreambleWithQueryRoot(imports: _*) + body

  private val directiveDefinitions =
    """
      |directive @link(url: String!, as: String, import: [link__Import]) repeatable on SCHEMA
      |directive @compose(name: String!) repeatable on SCHEMA
      |directive @label(name: String!) repeatable on OBJECT | INTERFACE | UNION | SCALAR | ENUM | INPUT_OBJECT | FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION | ENUM_VALUE
      |directive @shareable repeatable on OBJECT | FIELD_DEFINITION
      |directive @inaccessible on OBJECT | FIELD_DEFINITION | INPUT_OBJECT | INPUT_FIELD_DEFINITION | ENUM_VALUE
      |scalar link__Import
      |""".stripMargin

  private def directiveSchema(body: String, audit: String, compose: String = "@compose(name: \"@audit\")"): String =
    s"""
       |schema
       |  @link(
       |    url: "https://specs.apollo.dev/federation/v2.3"
       |    import: [
       |      { name: "@composeDirective", as: "@compose" }
       |      { name: "@tag", as: "@label" }
       |      "@shareable"
       |      "@inaccessible"
       |    ]
       |  )
       |  @link(url: "https://example.com/audit/v1.0", import: ["@audit"])
       |  $compose
       |{ query: Query }
       |$directiveDefinitions
       |$audit
       |$body
       |""".stripMargin

  private def compose(inputs: CompositionInput*) = {
    val contributions = inputs.toList.foldRight(Right(Nil): Either[List[String], List[SchemaContribution]]) {
      case (input, result) =>
        for {
          tail         <- result
          document     <- Parser.parseQuery(input.schema).left.map(error => List(s"[${input.name}] ${error.getMessage}"))
          sourceRoot   <- RemoteSchema
                            .toRootType(document, promoteOrphans = true)
                            .left
                            .map(error => List(s"[${input.name}] ${error.getMessage}"))
          subgraph      = Subgraph.federation(input.name, endpoint, document).transform(input.transformations: _*)
          contribution <- Gateway.prepareContribution(
                            subgraph,
                            sourceRoot,
                            document,
                            document,
                            federation = true
                          )
        } yield contribution :: tail
    }
    contributions.flatMap(SchemaComposition.compose)
  }

  private def directives(value: Option[List[Directive]]): List[(String, Map[String, caliban.InputValue])] =
    value.getOrElse(Nil).map(directive => directive.name -> directive.arguments)

  def spec = suite("CompositionSpec")(
    suite("ownership")(
      test("routes a shareable root field deterministically") {
        val valueSchema = schema("type Query { value: String @shareable }", "@shareable")

        for {
          alpha         <- stub("""{"data":{"value":"alpha"}}""")
          beta          <- stub("""{"data":{"value":"beta"}}""")
          gateway       <- Gateway
                             .compose(
                               Subgraph.federation("beta", beta.endpoint, valueSchema),
                               Subgraph.federation("alpha", alpha.endpoint, valueSchema)
                             )
                             .interpreter
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
          "@shareable"
        )
        val priceSchema = schema(
          "type Query { product: Product! @shareable } type Product { id: ID! @shareable price: Int! }",
          "@shareable"
        )
        val stockSchema = schema(
          "type Query { product: Product! @shareable } type Product { stock: Int! }",
          "@shareable"
        )

        for {
          names         <- stub("""{"data":{"product":{"id":"p1","name":"Table"}}}""")
          prices        <- stub("""{"data":{"product":{"id":"p1","price":10}}}""")
          stock         <- stub("""{"data":{"product":{"stock":5}}}""")
          gateway       <- Gateway
                             .compose(
                               Subgraph.federation("names", names.endpoint, namesSchema),
                               Subgraph.federation("prices", prices.endpoint, priceSchema),
                               Subgraph.federation("stock", stock.endpoint, stockSchema)
                             )
                             .interpreter
          response      <- gateway.execute("{ product { id name price } }")
          stockRequests <- stock.requests.get
          product        = field(response.data, "product")
        } yield assertTrue(
          response.errors.isEmpty,
          product.flatMap(field(_, "id")).contains(StringValue("p1")),
          product.flatMap(field(_, "name")).contains(StringValue("Table")),
          product.flatMap(field(_, "price")).contains(IntValue(10)),
          stockRequests.isEmpty
        )
      },
      test("retains an entity transition below a multiply provided root") {
        val namesSchema   = schema(
          "type Query { product: Product! @shareable } type Product @key(fields: \"id\") { id: ID! name: String! details: Details! } type Details { value: String! }",
          "@key",
          "@shareable"
        )
        val pricesSchema  = schema(
          "type Query { product: Product! @shareable } type Product @key(fields: \"id\") { id: ID! price: Int! }",
          "@key",
          "@shareable"
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
                             .interpreter
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
        val original  = schema("type Query { feed: String }", "@override")
        val replacing = schema("type Query { feed: String @override(from: \"products\") }", "@override")

        for {
          products        <- stub("""{"data":{"feed":"old"}}""")
          inventory       <- stub("""{"data":{"feed":"new"}}""")
          gateway         <- Gateway
                               .compose(
                                 Subgraph.federation("products", products.endpoint, original),
                                 Subgraph.federation("inventory", inventory.endpoint, replacing)
                               )
                               .interpreter
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
        val valueSchema = schema("type Query { value: String }", "@shareable")
        val result      = Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, valueSchema),
            Subgraph.federation("beta", endpoint, valueSchema)
          )
          .interpreter
          .exit

        result.map(exit => assertTrue(buildDiagnostics(exit).exists(_.contains("shareable"))))
      },
      test("requires each Federation 2 provider to make a key field shareable") {
        val keyed = schema(
          "type Query { alpha: Product } type Product @key(fields: \"id\") { id: ID! }",
          "@key"
        )
        val plain = schema("type Query { beta: Product } type Product { id: ID! }")

        Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, keyed),
            Subgraph.federation("beta", endpoint, plain)
          )
          .interpreter
          .exit
          .map(exit => assertTrue(buildDiagnostics(exit).exists(_.contains("shareable"))))
      },
      test("does not treat an unimported custom directive as Federation shareability") {
        val valueSchema = schema("type Query { value: String @shareable }")

        Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, valueSchema),
            Subgraph.federation("beta", endpoint, valueSchema)
          )
          .interpreter
          .exit
          .map(exit => assertTrue(buildDiagnostics(exit).exists(_.contains("shareable"))))
      },
      test("allows an override to name a source that is no longer present") {
        val base     = schema("type Query { value: String @shareable }", "@shareable")
        val migrated = schema(
          "type Query { value: String @shareable @override(from: \"removed\") }",
          "@shareable",
          "@override"
        )

        Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, base),
            Subgraph.federation("beta", endpoint, migrated)
          )
          .interpreter
          .exit
          .map(exit => assertTrue(exit.isSuccess))
      },
      test("allows an override of an external-only declaration") {
        val external   = schema(
          "type Query { alpha: Product } type Product { code: String @external }",
          "@external"
        )
        val overriding = schema(
          "type Query { beta: Product } type Product { code: String @override(from: \"alpha\") }",
          "@override"
        )

        Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, external),
            Subgraph.federation("beta", endpoint, overriding)
          )
          .interpreter
          .exit
          .map(exit => assertTrue(exit.isSuccess))
      },
      test("does not expose a field declared only as external") {
        val externalOnly = schema(
          "type Query { product: Product } type Product { id: ID! code: String @external }",
          "@external"
        )

        for {
          source       <- stub("""{"data":{"product":{"id":"p1"}}}""")
          gateway      <- Gateway.compose(Subgraph.federation("products", source.endpoint, externalOnly)).interpreter
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
                        .interpreter
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
      test("rejects malformed and unknown Federation keys with source diagnostics") {
        val malformed = schema(
          "type Query { product: Product } type Product @key(fields: \"id(\") { id: ID! }",
          "@key"
        )
        val unknown   = schema(
          "type Query { product: Product } type Product @key(fields: \"missing\") { id: ID! }",
          "@key"
        )

        Gateway
          .compose(
            Subgraph.federation("malformed", endpoint, malformed),
            Subgraph.federation("unknown", endpoint, unknown)
          )
          .interpreter
          .exit
          .map { exit =>
            val diagnostics = buildDiagnostics(exit)
            assertTrue(
              diagnostics.count(_.contains("Invalid @key field set")) == 2,
              diagnostics.exists(message =>
                message.startsWith("[malformed]") && message.contains("could not be parsed")
              ),
              diagnostics.exists(message => message.startsWith("[unknown]") && message.contains("does not exist"))
            )
          }
      },
      test("rejects incompatible external and overridden declarations") {
        val idOwner    = schema("type Query { alpha: Product } type Product { id: ID }", "@external")
        val idExternal = schema(
          "type Query { beta: Product } type Product { id: Int @external }",
          "@external"
        )
        val feedOwner  = schema("type Query { feed: String }", "@override")
        val feedNext   = schema("type Query { feed: Int @override(from: \"feed-owner\") }", "@override")

        Gateway
          .compose(
            Subgraph.federation("id-owner", endpoint, idOwner),
            Subgraph.federation("id-external", endpoint, idExternal),
            Subgraph.federation("feed-owner", endpoint, feedOwner),
            Subgraph.federation("feed-next", endpoint, feedNext)
          )
          .interpreter
          .exit
          .map { exit =>
            val diagnostics = buildDiagnostics(exit)
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
        val original  = schema("type Query { value: String }", "@override")
        val replacing = schema(
          "type Query { value: String @override(from: \"alpha\") }",
          "@override"
        )

        Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, original),
            Subgraph.federation("beta", endpoint, replacing),
            Subgraph.federation("gamma", endpoint, replacing)
          )
          .interpreter
          .exit
          .map { exit =>
            val diagnostics = buildDiagnostics(exit)
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
          "@inaccessible"
        )

        for {
          source       <- stub(
                            """{"data":{"product":{"id":"p1","state":"ACTIVE"}}}""",
                            """{"data":{"hiddenState":"HIDDEN"}}"""
                          )
          gateway      <- Gateway.compose(Subgraph.federation("products", source.endpoint, hiddenSchema)).interpreter
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
          "@shareable",
          "@inaccessible"
        )
        val hidden  = schema(
          "type Query { search(term: String @inaccessible): String @shareable }",
          "@shareable",
          "@inaccessible"
        )

        for {
          alpha    <- stub("""{"data":{"search":"ok"}}""")
          beta     <- stub("""{"data":{"search":"ok"}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("alpha", alpha.endpoint, visible),
                          Subgraph.federation("beta", beta.endpoint, hidden)
                        )
                        .interpreter
          response <- gateway.execute("{ search(term: \"secret\") }")
          requests <- alpha.requests.get.zip(beta.requests.get)
        } yield assertTrue(response.errors.nonEmpty, requests._1.isEmpty, requests._2.isEmpty)
      },
      test("rejects visible input coordinates that reference inaccessible types") {
        val argumentSchema = schema(
          "type Query { search(secret: Secret): String } input Secret @inaccessible { value: String }",
          "@inaccessible"
        )
        val inputSchema    = schema(
          "type Query { find(filter: Filter): String } input Filter { secret: Secret } input Secret @inaccessible { value: String }",
          "@inaccessible"
        )

        Gateway
          .compose(
            Subgraph.federation("arguments", endpoint, argumentSchema),
            Subgraph.federation("inputs", endpoint, inputSchema)
          )
          .interpreter
          .exit
          .map { exit =>
            val diagnostics = buildDiagnostics(exit)
            assertTrue(
              diagnostics.exists(_.startsWith("[arguments]")),
              diagnostics.exists(_.startsWith("[inputs]"))
            )
          }
      },
      test("allows inaccessible owners to reference inaccessible types") {
        val hiddenSchema = schema(
          "type Query { visible: String hidden(secret: Secret): Hidden @inaccessible } type Hidden @inaccessible { value: String } input Secret @inaccessible { value: String }",
          "@inaccessible"
        )

        Gateway
          .compose(Subgraph.federation("hidden", endpoint, hiddenSchema))
          .interpreter
          .exit
          .map(exit => assertTrue(exit.isSuccess))
      },
      test("applies inaccessible type visibility across every contribution") {
        val alpha = schema(
          "type Query { health: String @shareable box: Box @inaccessible @shareable } type Box @inaccessible { secret: Secret @shareable } type Secret @inaccessible { value: String @shareable }",
          "@inaccessible",
          "@shareable"
        )
        val beta  = schema(
          "type Query { health: String @shareable box: Box @inaccessible @shareable } type Box { secret: Secret @shareable } type Secret @inaccessible { value: String @shareable }",
          "@inaccessible",
          "@shareable"
        )

        Gateway
          .compose(
            Subgraph.federation("alpha", endpoint, alpha),
            Subgraph.federation("beta", endpoint, beta)
          )
          .interpreter
          .exit
          .map(exit => assertTrue(exit.isSuccess))
      },
      test("rejects required inaccessible arguments and input fields without defaults") {
        val argumentSchema = schema(
          "type Query { search(term: String!, tenant: String! @inaccessible): String }",
          "@inaccessible"
        )
        val inputSchema    = schema(
          "type Query { search(filter: Filter!): String } input Filter { term: String! tenant: String! @inaccessible }",
          "@inaccessible"
        )
        val defaultSchema  = schema(
          "type Query { search(tenant: String! = \"public\" @inaccessible): String }",
          "@inaccessible"
        )

        for {
          argument <- Gateway.compose(Subgraph.federation("argument", endpoint, argumentSchema)).interpreter.exit
          input    <- Gateway.compose(Subgraph.federation("input", endpoint, inputSchema)).interpreter.exit
          default  <- Gateway.compose(Subgraph.federation("default", endpoint, defaultSchema)).interpreter.exit
        } yield assertTrue(
          argument.isFailure,
          input.isFailure,
          default.isSuccess,
          buildDiagnostics(argument).exists(_.toLowerCase.contains("required @inaccessible")),
          buildDiagnostics(input).exists(_.toLowerCase.contains("required @inaccessible"))
        )
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
          .interpreter
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
          "@inaccessible"
        )

        for {
          source   <- stub("""{"data":{"product":{"id":"p1"}}}""")
          gateway  <- Gateway.compose(Subgraph.federation("products", source.endpoint, hiddenSchema)).interpreter
          response <-
            gateway.execute(
              "{ visible: __type(name: \"Product\") { interfaces { name } } hidden: __type(name: \"HiddenInterface\") { name } }"
            )
        } yield assertTrue(
          field(response.data, "visible").flatMap(field(_, "interfaces")).contains(ListValue(Nil)),
          field(response.data, "hidden").contains(NullValue)
        )
      },
      test("does not expose inaccessible union members") {
        val hiddenSchema = schema(
          """
            |type Query { result: Search }
            |union Search = Product | HiddenResult
            |type Product { id: ID! }
            |type HiddenResult @inaccessible { reason: String! }
            |""".stripMargin,
          "@inaccessible"
        )

        for {
          source   <- stub("""{"data":{"result":{"__typename":"Product","id":"p1"}}}""")
          gateway  <- Gateway.compose(Subgraph.federation("search", source.endpoint, hiddenSchema)).interpreter
          response <-
            gateway.execute(
              "{ search: __type(name: \"Search\") { possibleTypes { name } } hidden: __type(name: \"HiddenResult\") { name } }"
            )
        } yield assertTrue(
          field(response.data, "search")
            .flatMap(field(_, "possibleTypes"))
            .contains(ListValue(List(caliban.ResponseValue.ObjectValue(List("name" -> StringValue("Product")))))),
          field(response.data, "hidden").contains(NullValue)
        )
      },
      test("uses globally composed visibility for fields and input fields") {
        val hidden  = schema(
          """
            |type Query { alpha(filter: Filter): Product }
            |type Product { id: ID! @shareable secret: HiddenOutput @shareable @inaccessible }
            |type HiddenOutput @inaccessible { value: String @shareable }
            |input Filter { term: String secret: HiddenInput @inaccessible }
            |input HiddenInput @inaccessible { value: String }
            |""".stripMargin,
          "@inaccessible",
          "@shareable"
        )
        val visible = schema(
          """
            |type Query { beta(filter: Filter): Product }
            |type Product { id: ID! @shareable secret: HiddenOutput @shareable }
            |type HiddenOutput { value: String @shareable }
            |input Filter { term: String secret: HiddenInput }
            |input HiddenInput { value: String }
            |""".stripMargin,
          "@inaccessible",
          "@shareable"
        )

        for {
          alpha        <- stub("""{"data":{"alpha":{"id":"p1"}}}""")
          beta         <- stub("""{"data":{"beta":{"id":"p1"}}}""")
          gateway      <- Gateway
                            .compose(
                              Subgraph.federation("alpha", alpha.endpoint, hidden),
                              Subgraph.federation("beta", beta.endpoint, visible)
                            )
                            .interpreter
          response     <-
            gateway.execute(
              "{ product: __type(name: \"Product\") { fields { name } } filter: __type(name: \"Filter\") { inputFields { name } } output: __type(name: \"HiddenOutput\") { name } input: __type(name: \"HiddenInput\") { name } }"
            )
          productFields = field(response.data, "product")
                            .flatMap(field(_, "fields"))
                            .collect { case ListValue(values) => values.flatMap(field(_, "name")) }
          inputFields   = field(response.data, "filter")
                            .flatMap(field(_, "inputFields"))
                            .collect { case ListValue(values) => values.flatMap(field(_, "name")) }
        } yield assertTrue(
          response.errors.isEmpty,
          productFields.contains(List(StringValue("id"))),
          inputFields.contains(List(StringValue("term"))),
          field(response.data, "output").contains(NullValue),
          field(response.data, "input").contains(NullValue)
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
                       .interpreter
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
          alpha    <- stub(
                        """{"data":{"alpha":{"_caliban_gateway_runtime_typename":"Product","id":"p1"}}}"""
                      )
          beta     <- stub(
                        """{"data":{"beta":{"_caliban_gateway_runtime_typename":"Review","body":"good"}}}"""
                      )
          gateway  <- Gateway
                        .compose(
                          Subgraph.graphql("alpha", alpha.endpoint, alphaSchema),
                          Subgraph.graphql("beta", beta.endpoint, betaSchema)
                        )
                        .interpreter
          response <- gateway.execute(
                        "{ alpha { ... on Product { id } } beta { ... on Review { body } } }"
                      )
        } yield assertTrue(response.errors.isEmpty)
      },
      test("drops types reachable only from subscription roots") {
        val result = compose(
          CompositionInput(
            "alpha",
            "type Query { alpha: String } type Subscription { alphaEvents: Event } type Event { value: String }"
          ),
          CompositionInput(
            "beta",
            "type Query { beta: String } type Subscription { betaEvents: Event } type Event { value: Int }"
          )
        )

        assertTrue(result.isRight, result.toOption.forall(!_.rootType.types.contains("Event")))
      },
      test("resolves interface and union references to composed types") {
        val alphaSchema =
          "type Query { search: Search product: Product } interface Node { id: ID! } union Search = Product type Product implements Node { id: ID! }"
        val betaSchema  =
          "type Query { productByName: Product } interface Node { name: String! } type Product implements Node { name: String! }"

        for {
          alpha          <- stub("""{"data":{"search":null}}""")
          beta           <- stub("""{"data":{"productByName":null}}""")
          gateway        <- Gateway
                              .compose(
                                Subgraph.graphql("alpha", alpha.endpoint, alphaSchema),
                                Subgraph.graphql("beta", beta.endpoint, betaSchema)
                              )
                              .interpreter
          response       <-
            gateway.execute(
              "{ product: __type(name: \"Product\") { interfaces { fields { name } } } search: __type(name: \"Search\") { possibleTypes { fields { name } } } }"
            )
          product         = field(response.data, "product")
          search          = field(response.data, "search")
          interfaceFields = product
                              .flatMap(field(_, "interfaces"))
                              .collect { case ListValue(interface :: Nil) => interface }
                              .flatMap(field(_, "fields"))
                              .collect { case ListValue(values) =>
                                values.flatMap(field(_, "name")).collect { case StringValue(name) => name }.toSet
                              }
          possibleFields  = search
                              .flatMap(field(_, "possibleTypes"))
                              .collect { case ListValue(possible :: Nil) => possible }
                              .flatMap(field(_, "fields"))
                              .collect { case ListValue(values) =>
                                values.flatMap(field(_, "name")).collect { case StringValue(name) => name }.toSet
                              }
        } yield assertTrue(
          response.errors.isEmpty,
          interfaceFields.contains(Set("id", "name")),
          possibleFields.contains(Set("id", "name"))
        )
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
          .interpreter
          .exit
          .map(exit => assertTrue(buildDiagnostics(exit).exists(_.contains("Input/output enum"))))
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
          .interpreter
          .exit
          .map { exit =>
            val diagnostics = buildDiagnostics(exit)
            assertTrue(
              diagnostics.exists(_.startsWith("[query.value]")),
              diagnostics.exists(_.startsWith("[type Filter.limit]"))
            )
          }
      }
    ),
    suite("directive metadata")(
      test("retains metadata when one source type backs multiple operation roots") {
        val root     = Types.makeObject(
          Some("Root"),
          None,
          Types.makeField("value", None, Nil, () => Types.string) :: Nil,
          Directive("mark") :: Nil
        )
        val rootType = RootType(
          root,
          Some(root),
          None,
          additionalDirectives = __Directive(
            "mark",
            None,
            Set(__DirectiveLocation.OBJECT),
            _ => Nil,
            isRepeatable = false
          ) :: Nil
        )
        val document = Document(Nil, SourceMapper.empty)
        val result   = SchemaCoordinateMapping
          .compile("local", rootType, document, federation = false, Nil)
          .flatMap(mapping =>
            SchemaComposition.compose(
              SchemaContribution("local", rootType, document, federation = false, Nil, mapping) :: Nil
            )
          )

        assertTrue(
          result.isRight,
          result.exists(graph => directives(graph.rootType.queryType.directives).exists(_._1 == "mark")),
          result.exists(graph =>
            graph.rootType.mutationType.exists(tpe => directives(tpe.directives).exists(_._1 == "mark"))
          )
        )
      },
      test("retains tag metadata across visible type-system coordinates") {
        val result = compose(
          CompositionInput(
            "types",
            directiveSchema(
              """
                |type Query {
                |  node: Node
                |  result: Result
                |  code: Code
                |  state: State
                |  search(filter: Filter): String
                |}
                |interface Node @label(name: "interface") { id: ID! }
                |type Product implements Node { id: ID! }
                |union Result @label(name: "union") = Product
                |scalar Code @label(name: "scalar")
                |enum State @label(name: "enum") { READY @label(name: "value") }
                |input Filter @label(name: "input") { term: String @label(name: "input-field") }
                |""".stripMargin,
              "directive @audit(level: String!) on FIELD_DEFINITION"
            )
          )
        )
        val types  = result.toOption.fold(Map.empty[String, caliban.introspection.adt.__Type])(_.rootType.types)
        val labels = List("Node", "Result", "Code", "State", "Filter").map(name =>
          directives(types.get(name).flatMap(_.directives)).exists(_._1 == "label")
        )

        assertTrue(
          result.isRight,
          labels.forall(_ == true),
          directives(types.get("State").flatMap(_.allEnumValues.headOption).flatMap(_.directives))
            .exists(_._1 == "label"),
          directives(types.get("Filter").flatMap(_.allInputFields.headOption).flatMap(_.directives))
            .exists(_._1 == "label")
        )
      },
      test("resolves namespace-qualified tag and composeDirective metadata") {
        val sdl    =
          """
            |schema
            |  @link(url: "https://specs.apollo.dev/federation/v2.3")
            |  @link(url: "https://example.com/audit/v1.0")
            |  @federation__composeDirective(name: "@audit__trace")
            |{ query: Query }
            |directive @link(url: String!, as: String, import: [link__Import]) repeatable on SCHEMA
            |directive @federation__composeDirective(name: String!) repeatable on SCHEMA
            |directive @federation__tag(name: String!) repeatable on FIELD_DEFINITION
            |directive @audit__trace(level: String!) on FIELD_DEFINITION
            |scalar link__Import
            |type Query { value: String @federation__tag(name: "public") @audit__trace(level: "metadata") }
            |""".stripMargin
        val result = compose(CompositionInput("namespaced", sdl))
        val graph  = result.toOption
        val field  = graph.flatMap(_.rootType.queryType.allFields.find(_.name == "value"))

        assertTrue(
          result.isRight,
          graph.toList.flatMap(_.rootType.additionalDirectives).map(_.name).sorted ==
            List("audit__trace", "federation__tag"),
          directives(field.flatMap(_.directives)).map(_._1).sorted == List("audit__trace", "federation__tag")
        )
      },
      test("retains aliased tag and selected custom metadata on transformed visible coordinates") {
        val sdl      = directiveSchema(
          """
            |type Query @label(name: "root") {
            |  product(filter: Filter @audit(level: "argument")): Product
            |}
            |type Product @audit(level: "type") {
            |  value: String @label(name: "public") @audit(level: "field")
            |  hidden: String @inaccessible @label(name: "hidden")
            |}
            |input Filter { term: String @audit(level: "input") }
            |""".stripMargin,
          "directive @audit(level: String!) repeatable on OBJECT | FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION"
        )
        val result   = compose(
          CompositionInput(
            "products",
            sdl,
            List(
              SchemaTransformation.renameField("Product", "value", "display"),
              SchemaTransformation.hideField("Product", "hidden")
            )
          )
        )
        val composed = result.toOption
        val query    = composed.map(_.rootType.queryType)
        val product  = composed.flatMap(_.rootType.types.get("Product"))
        val display  = product.flatMap(_.allFields.find(_.name == "display"))
        val filter   = composed.flatMap(_.rootType.types.get("Filter"))

        assertTrue(
          result.isRight,
          composed.toList.flatMap(_.rootType.additionalDirectives).map(_.name).sorted == List("audit", "label"),
          directives(query.flatMap(_.directives)).exists(_._1 == "label"),
          directives(product.flatMap(_.directives)).exists(_._1 == "audit"),
          directives(display.flatMap(_.directives)).map(_._1).sorted == List("audit", "label"),
          product.forall(!_.allFields.exists(_.name == "hidden")),
          directives(
            query.flatMap(_.allFields.find(_.name == "product")).flatMap(_.allArgs.headOption).flatMap(_.directives)
          ).exists(_._1 == "audit"),
          directives(filter.flatMap(_.allInputFields.headOption).flatMap(_.directives)).exists(_._1 == "audit")
        )
      },
      test("rejects retained metadata that references non-visible input coordinates") {
        val hiddenField = compose(
          CompositionInput(
            "hidden-field",
            directiveSchema(
              """
                |type Query { value: String @audit(options: { secret: "value" }) }
                |input Options { visible: String secret: String @inaccessible }
                |""".stripMargin,
              "directive @audit(options: [Options]) on FIELD_DEFINITION"
            )
          )
        )
        val hiddenType  = compose(
          CompositionInput(
            "hidden-type",
            directiveSchema(
              """
                |type Query { value: String @audit(options: { value: "secret" }) }
                |input SecretOptions @inaccessible { value: String }
                |""".stripMargin,
              "directive @audit(options: SecretOptions) on FIELD_DEFINITION"
            )
          )
        )
        val hiddenEnum  = compose(
          CompositionInput(
            "hidden-enum",
            directiveSchema(
              """
                |type Query { value: String @audit(state: BLOCKED) }
                |enum State { READY BLOCKED @inaccessible }
                |""".stripMargin,
              "directive @audit(state: State) on FIELD_DEFINITION"
            )
          )
        )

        assertTrue(
          hiddenField.left.exists(_.exists(_.contains("Options.secret"))),
          hiddenType.left.exists(_.exists(_.contains("SecretOptions"))),
          hiddenEnum.left.exists(_.exists(_.contains("State.BLOCKED")))
        )
      },
      test("retains an otherwise-unused ID referenced by a directive definition") {
        val result = compose(
          CompositionInput(
            "id-metadata",
            directiveSchema(
              "type Query { value: String @audit(id: \"1\") }",
              "directive @audit(id: ID) on FIELD_DEFINITION"
            )
          )
        )

        assertTrue(
          result.isRight,
          result.exists(_.rootType.types.contains("ID")),
          result.exists(
            _.rootType.additionalDirectives
              .find(_.name == "audit")
              .exists(
                _.allArgs.exists(argument => argument.name == "id" && argument._type.innerType.name.contains("ID"))
              )
          )
        )
      },
      test("merges repeatable applications from every contribution") {
        def shared(source: String)                                   = directiveSchema(
          s"""type Query { value: String @shareable @label(name: "$source") @audit(level: "$source") }""",
          "directive @audit(level: String!) repeatable on FIELD_DEFINITION"
        )
        val result                                                   = compose(CompositionInput("alpha", shared("alpha")), CompositionInput("beta", shared("beta")))
        val applied: List[(String, Map[String, caliban.InputValue])] = result.toOption.toList.flatMap(
          _.rootType.queryType.allFields
            .find(_.name == "value")
            .toList
            .flatMap(field => directives(field.directives))
        )

        assertTrue(
          result.isRight,
          applied.collect { case ("label", arguments) => arguments("name") }.toSet ==
            Set[caliban.InputValue](StringValue("alpha"), StringValue("beta")),
          applied.collect { case ("audit", arguments) => arguments("level") }.toSet ==
            Set[caliban.InputValue](StringValue("alpha"), StringValue("beta"))
        )
      },
      test("selects linked directives independently for each source") {
        def featureSchema(sourceDirective: String, selected: Boolean, feature: String = "audit") = {
          val selection = if (selected) s"""@compose(name: "@$sourceDirective")""" else ""
          s"""
             |schema
             |  @link(
             |    url: "https://specs.apollo.dev/federation/v2.3"
             |    import: [{ name: "@composeDirective", as: "@compose" }, "@shareable"]
             |  )
             |  @link(
             |    url: "https://example.com/$feature/v1.0"
             |    import: [{ name: "@audit", as: "@$sourceDirective" }]
             |  )
             |  $selection
             |{ query: Query }
             |directive @link(url: String!, as: String, import: [link__Import]) repeatable on SCHEMA
             |directive @compose(name: String!) repeatable on SCHEMA
             |directive @shareable repeatable on FIELD_DEFINITION
             |directive @$sourceDirective(label: String!) repeatable on FIELD_DEFINITION
             |scalar link__Import
             |type Query { value: String @shareable @$sourceDirective(label: "$sourceDirective") }
             |""".stripMargin
        }

        val merged    = compose(
          CompositionInput("alpha", featureSchema("audit", selected = true)),
          CompositionInput("beta", featureSchema("review", selected = false))
        )
        val field     = merged.toOption.flatMap(_.rootType.queryType.allFields.find(_.name == "value"))
        val collision = compose(
          CompositionInput("alpha", featureSchema("audit", selected = true, feature = "first")),
          CompositionInput("beta", featureSchema("audit", selected = true, feature = "second"))
        )

        assertTrue(
          merged.isRight,
          merged.toOption.toList.flatMap(_.rootType.additionalDirectives.map(_.name)) == List("audit"),
          directives(field.flatMap(_.directives)).collect { case ("audit", arguments) => arguments("label") } ==
            List[caliban.InputValue](StringValue("audit")),
          collision.left.exists(
            _.exists(message => message.contains("first") && message.contains("second") && message.contains("audit"))
          )
        )
      },
      test("preserves authored multiplicity for repeatable directives") {
        val result       = compose(
          CompositionInput(
            "alpha",
            directiveSchema(
              "type Query { value: String @shareable @audit(label: \"same\") }",
              "directive @audit(label: String!) repeatable on FIELD_DEFINITION"
            )
          ),
          CompositionInput(
            "beta",
            directiveSchema(
              "type Query { value: String @shareable @audit(label: \"same\") @audit(label: \"same\") }",
              "directive @audit(label: String!) repeatable on FIELD_DEFINITION"
            )
          )
        )
        val applications = result.toOption.toList.flatMap(
          _.rootType.queryType.allFields
            .find(_.name == "value")
            .toList
            .flatMap(field => directives(field.directives))
            .filter(_._1 == "audit")
        )

        assertTrue(result.isRight, applications.size == 2)
      },
      test("retains composed type directives on interface objects") {
        val sdl    =
          s"""
             |${federationSchemaPreamble("@key", "@interfaceObject", "@tag")}
             |directive @tag(name: String!) repeatable on OBJECT | INTERFACE | FIELD_DEFINITION
             |type Query { node: Node }
             |type Node @key(fields: "id") @interfaceObject @tag(name: "metadata") { id: ID! }
             |""".stripMargin
        val result = compose(CompositionInput("nodes", sdl))
        val node   = result.toOption.flatMap(_.rootType.types.get("Node"))

        assertTrue(
          result.isRight,
          node.exists(_.kind == caliban.introspection.adt.__TypeKind.INTERFACE),
          directives(node.flatMap(_.directives)).exists { case (name, arguments) =>
            name == "tag" && arguments.get("name").contains(StringValue("metadata"))
          }
        )
      },
      test("validates and compares applications with GraphQL input semantics") {
        def defaultSchema(value: String, selected: Boolean) = directiveSchema(
          s"type Query { value: String @shareable @audit$value }",
          "directive @audit(enabled: Boolean = false) on FIELD_DEFINITION",
          compose = if (selected) "@compose(name: \"@audit\")" else ""
        )
        val equivalent                                      = compose(
          CompositionInput("alpha", defaultSchema("", selected = true)),
          CompositionInput("beta", defaultSchema("(enabled: false)", selected = false))
        )
        val invalid                                         = compose(
          CompositionInput(
            "invalid",
            directiveSchema(
              "type Query { value: String @audit(unknown: \"value\") }",
              "directive @audit(required: String!) on OBJECT"
            )
          )
        )

        assertTrue(
          equivalent.isRight,
          invalid.left.exists(_.exists(_.contains("FIELD_DEFINITION"))),
          invalid.left.exists(_.exists(_.contains("unknown"))),
          invalid.left.exists(_.exists(_.contains("required")))
        )
      },
      test("canonicalizes built-in scalar directive arguments") {
        def scalarSchema(score: String, id: String, selected: Boolean) = directiveSchema(
          s"type Query { value: String @shareable @audit(score: $score, id: $id) }",
          "directive @audit(score: Float!, id: ID!) on FIELD_DEFINITION",
          compose = if (selected) "@compose(name: \"@audit\")" else ""
        )
        val result                                                     = compose(
          CompositionInput("alpha", scalarSchema("1", "1", selected = true)),
          CompositionInput("beta", scalarSchema("1.0", "\"1\"", selected = false))
        )
        val audit                                                      = result.toOption.flatMap(
          _.rootType.queryType.allFields
            .find(_.name == "value")
            .flatMap(_.directives)
            .flatMap(_.find(_.name == "audit"))
        )

        assertTrue(
          result.isRight,
          audit.flatMap(_.arguments.get("score")).contains(FloatValue(BigDecimal(1))),
          audit.flatMap(_.arguments.get("id")).contains(StringValue("1"))
        )
      },
      test("canonicalizes structured directive-definition defaults") {
        def structured(default: String, selected: Boolean) = directiveSchema(
          "input Options { enabled: Boolean = false tags: [String!] = [\"x\"] }\n" +
            "type Query { value: String @shareable @audit }",
          s"directive @audit(options: Options = $default) on FIELD_DEFINITION",
          compose = if (selected) "@compose(name: \"@audit\")" else ""
        )
        val result                                         = compose(
          CompositionInput(
            "alpha",
            structured("{ enabled: false, tags: [\"x\"] }", selected = true)
          ),
          CompositionInput("beta", structured("{ tags: \"x\" }", selected = false))
        )

        assertTrue(result.isRight)
      },
      test("rejects unknown fields before canonicalizing directive input objects") {
        val result = compose(
          CompositionInput(
            "unknown-input-field",
            directiveSchema(
              "input Options { nested: Nested }\n" +
                "input Nested { known: String }\n" +
                "type Query { value: String @audit(options: { nested: { known: \"x\", typo: \"y\" } }) }",
              "directive @audit(options: Options!) on FIELD_DEFINITION"
            )
          )
        )

        assertTrue(
          result.left.exists(
            _.exists(message => message.contains("unknown-input-field") && message.contains("typo"))
          )
        )
      },
      test("retains selected schema-coordinate applications as composed metadata") {
        val result = compose(
          CompositionInput(
            "schema-metadata",
            directiveSchema(
              "type Query { value: String }",
              "directive @audit(label: String!) repeatable on SCHEMA",
              compose = "@compose(name: \"@audit\") @audit(label: \"schema\")"
            )
          )
        )

        assertTrue(
          result.isRight,
          result.exists(graph =>
            directives(Some(graph.schemaDirectives)).exists { case (name, arguments) =>
              name == "audit" && arguments.get("label").contains(StringValue("schema"))
            }
          )
        )
      },
      test("requires Federation 2.1 for composeDirective") {
        val sdl    = directiveSchema(
          "type Query { value: String @audit(level: \"value\") }",
          "directive @audit(level: String!) on FIELD_DEFINITION"
        ).replace("federation/v2.3", "federation/v2.0")
        val result = compose(CompositionInput("old-federation", sdl))

        assertTrue(
          result.left.exists(
            _.exists(message => message.contains("old-federation") && message.contains("v2.1"))
          )
        )
      },
      test("rejects incompatible definitions and non-repeatable applications") {
        def custom(argumentType: String, value: String) = directiveSchema(
          s"type Query { value: String @shareable @audit(level: $value) }",
          s"directive @audit(level: $argumentType!) on FIELD_DEFINITION"
        )
        val definitionsResult                           = compose(
          CompositionInput("alpha", custom("String", "\"alpha\"")),
          CompositionInput("beta", custom("Int", "1"))
        )
        val applicationsResult                          = compose(
          CompositionInput("alpha", custom("String", "\"alpha\"")),
          CompositionInput("beta", custom("String", "\"beta\""))
        )
        val definitionErrors                            = definitionsResult.left.getOrElse(Nil)
        val applicationErrors                           = applicationsResult.left.getOrElse(Nil)

        assertTrue(
          definitionErrors.exists(message =>
            message.contains("@audit") && message.contains("'alpha'") && message.contains("'beta'")
          ),
          applicationErrors.exists(message =>
            message.contains("Query.value") && message.contains("'alpha'") && message.contains("'beta'")
          )
        )
      },
      test("reports non-repeatable duplicates from every source") {
        def duplicated(source: String) = directiveSchema(
          s"""type Query { value: String @shareable @audit(level: "$source") @audit(level: "$source") }""",
          "directive @audit(level: String!) on FIELD_DEFINITION"
        )
        val errors                     = compose(
          CompositionInput("alpha", duplicated("alpha")),
          CompositionInput("beta", duplicated("beta"))
        ).left.getOrElse(Nil)

        assertTrue(
          errors.count(_.contains("Non-repeatable directive '@audit' is applied more than once")) == 2,
          errors.exists(_.startsWith("[alpha]")),
          errors.exists(_.startsWith("[beta]"))
        )
      },
      test("validates compose declarations and exposes only retained definitions through introspection") {
        val invalid           = compose(
          CompositionInput(
            "invalid",
            directiveSchema(
              "type Query { value: String }",
              "directive @audit(level: String!) on FIELD_DEFINITION",
              compose = "@compose(name: \"audit\") @compose(name: \"@missing\")"
            )
          )
        )
        val valid             = directiveSchema(
          "type Query { value: String @audit(level: \"ok\") }",
          "directive @audit(level: String!) on FIELD_DEFINITION\ndirective @unused on FIELD_DEFINITION"
        )
        val schemaApplication = compose(
          CompositionInput(
            "schema-application",
            directiveSchema(
              "type Query { value: String }",
              "directive @audit(level: String!) on SCHEMA",
              compose = "@compose(name: \"@audit\") @audit(level: \"schema\")"
            )
          )
        )

        for {
          remote   <- stub("""{"data":{"value":"ok"}}""")
          runtime  <- Gateway.compose(Subgraph.federation("valid", remote.endpoint, valid)).interpreter
          response <- runtime.execute("{ __schema { directives { name isRepeatable locations } } }")
          names     = listValues(field(response.data, "__schema").flatMap(field(_, "directives"))).flatMap {
                        case ObjectValue(fields) => fields.collectFirst { case ("name", StringValue(name)) => name }
                        case _                   => None
                      }
        } yield assertTrue(
          invalid.left.exists(_.exists(_.contains("must start with '@'"))),
          invalid.left.exists(_.exists(_.contains("@missing"))),
          schemaApplication.exists(_.schemaDirectives.exists(_.name == "audit")),
          response.errors.isEmpty,
          names.contains("audit"),
          names.contains("label"),
          !names.contains("unused"),
          !names.contains("compose")
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

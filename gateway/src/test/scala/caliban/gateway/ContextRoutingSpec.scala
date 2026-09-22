package caliban.gateway

import caliban.Value.IntValue.IntNumber
import caliban.gateway.GatewayTestSupport._
import zio.{ Scope, ZIO }
import zio.test._

object ContextRoutingSpec extends ZIOSpecDefault {

  private val contextDirectives =
    """
      |directive @context(name: String!) repeatable on OBJECT | INTERFACE | UNION
      |directive @fromContext(field: String!) on ARGUMENT_DEFINITION
      |""".stripMargin

  private def preamble(imports: String*): String =
    federationSchemaPreamble(imports: _*)
      .replace("federation/v2.3", "federation/v2.8") + contextDirectives

  private val userContextSchema =
    s"""
       |${preamble("@key", "@context", "@fromContext")}
       |type Query { user: User }
       |type User @key(fields: "id") @context(name: "userContext") {
       |  id: ID!
       |  currency: String!
       |  amount(currency: String @fromContext(field: "$$userContext { currency }")): Int!
       |}
       |""".stripMargin

  def spec = suite("ContextRoutingSpec")(
    test("injects per-entity context arguments into downstream fetches") {
      val usersSchema        =
        s"""
           |${preamble("@key", "@shareable")}
           |type Query { users: [User!]! }
           |type User @key(fields: "id") {
           |  id: ID!
           |  currency: String!
           |}
           |""".stripMargin
      val transactionsSchema =
        s"""
           |${preamble("@key", "@external", "@context", "@fromContext")}
           |type User @key(fields: "id") @context(name: "userContext") {
           |  id: ID! @external
           |  currency: String! @external
           |  transactions: [Transaction!]!
           |}
           |type Transaction @key(fields: "id") {
           |  id: ID!
           |  amount(currency: String @fromContext(field: "$$userContext ... on User { currency }")): Int!
           |}
           |""".stripMargin

      for {
        users            <-
          stub(
            """{"data":{"users":[{"_caliban_gateway_requirement_currency":"USD","_caliban_gateway_requirement_currency_User":"USD","_caliban_gateway_context_typename":"User","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"},{"_caliban_gateway_requirement_currency":"EUR","_caliban_gateway_requirement_currency_User":"EUR","_caliban_gateway_context_typename":"User","_caliban_gateway_key":"u2","_caliban_gateway_typename":"User"}]}}"""
          )
        transactions     <-
          stubByRequest { request =>
            val query = request.query.getOrElse("")
            if (query.contains("currency:\"")) {
              val amount = if (query.contains("currency:\"EUR\"")) 200 else 100
              s"""{"data":{"_entities":[{"amount":$amount}]}}"""
            } else
              """{"data":{"_entities":[{"transactions":[{"_caliban_gateway_key":"t1","_caliban_gateway_typename":"Transaction"}]},{"transactions":[{"_caliban_gateway_key":"t2","_caliban_gateway_typename":"Transaction"}]}]}}"""
          }
        runtime          <- Gateway
                              .compose(
                                Subgraph.federation("users", users.endpoint, usersSchema),
                                Subgraph.federation("transactions", transactions.endpoint, transactionsSchema)
                              )
                              .interpreter
        response         <- runtime.execute("{ users { transactions { amount } } }")
        usersSent        <- users.requests.get
        transactionsSent <- transactions.requests.get
        amounts           = listValues(field(response.data, "users")).flatMap { user =>
                              listValues(field(user, "transactions")).flatMap(field(_, "amount"))
                            }
      } yield assertTrue(
        response.errors.isEmpty,
        amounts == List(IntNumber(100), IntNumber(200)),
        usersSent.headOption.flatMap(_.query).exists(_.contains("_caliban_gateway_requirement_currency")),
        transactionsSent.size == 3,
        transactionsSent.flatMap(_.query).exists(_.contains("amount(currency:\"USD\")")),
        transactionsSent.flatMap(_.query).exists(_.contains("amount(currency:\"EUR\")"))
      )
    },
    test("hides context-supplied arguments from the composed API") {
      for {
        remote  <- stub("""{"data":{"user":null}}""")
        runtime <- Gateway.compose(Subgraph.federation("users", remote.endpoint, userContextSchema)).interpreter
        result  <- runtime.execute("{ __type(name: \"User\") { fields { name args { name } } } }")
        fields   = field(result.data, "__type").toList.flatMap(value => listValues(field(value, "fields")))
        amount   = fields.find(value => field(value, "name").contains(caliban.Value.StringValue("amount")))
      } yield assertTrue(
        result.errors.isEmpty,
        amount.exists(value => listValues(field(value, "args")).isEmpty)
      )
    },
    test("splits a same-subgraph context field into an entity fetch") {
      for {
        remote   <- stubByRequest(request =>
                      if (request.query.exists(_.contains("_entities")))
                        """{"data":{"_entities":[{"amount":42}]}}"""
                      else
                        """{"data":{"user":{"_caliban_gateway_requirement_currency":"USD","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"}}}"""
                    )
        runtime  <- Gateway.compose(Subgraph.federation("users", remote.endpoint, userContextSchema)).interpreter
        response <- runtime.execute("{ user { amount } }")
        sent     <- remote.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "user").flatMap(field(_, "amount")).contains(IntNumber(42)),
        sent.size == 2,
        sent.lastOption.flatMap(_.query).exists(_.contains("amount(currency:\"USD\")"))
      )
    },
    test("preserves unconditioned context arguments alongside type-conditioned arguments") {
      val schema =
        s"""
           |${preamble("@key", "@context", "@fromContext")}
           |type Query { user: User }
           |type User @key(fields: "id") @context(name: "userContext") {
           |  id: ID!
           |  currency: String!
           |  region: String!
           |  amount(
           |    currency: String @fromContext(field: "$$userContext { currency }")
           |    region: String @fromContext(field: "$$userContext ... on User { region }")
           |  ): Int!
           |}
           |""".stripMargin

      for {
        remote   <- stubByRequest(request =>
                      if (request.query.exists(_.contains("_entities")))
                        """{"data":{"_entities":[{"amount":42}]}}"""
                      else
                        """{"data":{"user":{"_caliban_gateway_requirement_currency":"USD","_caliban_gateway_requirement_region_User":"US","_caliban_gateway_context_typename":"User","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"}}}"""
                    )
        runtime  <- Gateway.compose(Subgraph.federation("users", remote.endpoint, schema)).interpreter
        response <- runtime.execute("{ user { amount } }")
        sent     <- remote.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "user").flatMap(field(_, "amount")).contains(IntNumber(42)),
        sent.size == 2,
        sent.lastOption
          .flatMap(_.query)
          .exists(query => query.contains("currency:\"USD\"") && query.contains("region:\"US\""))
      )
    },
    test("fetches sibling root context selections before an entity field") {
      val schema =
        s"""
           |${preamble("@key", "@context", "@fromContext")}
           |type Query @context(name: "topLevelQuery") {
           |  me: User!
           |  product: Product
           |  other: String
           |}
           |type User @key(fields: "id") {
           |  id: ID!
           |  locale: String!
           |}
           |type Product @key(fields: "id") {
           |  id: ID!
           |  price(locale: String @fromContext(field: "$$topLevelQuery { me { locale } }")): Int!
           |}
           |""".stripMargin

      for {
        remote   <- stubByRequest(request =>
                      if (request.query.exists(_.contains("_entities")))
                        """{"data":{"_entities":[{"price":7}]}}"""
                      else
                        """{"data":{"product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"},"_caliban_gateway_requirement_me_locale":{"locale":"en"}}}"""
                    )
        runtime  <- Gateway.compose(Subgraph.federation("products", remote.endpoint, schema)).interpreter
        response <- runtime.execute("{ product { price } }")
        plan     <- runtime.explain("{ product { price } other }")
        sent     <- remote.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "product").flatMap(field(_, "price")).contains(IntNumber(7)),
        sent.headOption.flatMap(_.query).exists(query => query.contains("me") && query.contains("locale")),
        sent.lastOption.flatMap(_.query).exists(_.contains("price(locale:\"en\")")),
        plan.linesIterator.exists(_ == "fetch products at $.other fields []")
      )
    },
    test("plans an injected root __typename context selection") {
      val schema =
        s"""
           |${preamble("@key", "@context", "@fromContext")}
           |type Query @context(name: "topLevelQuery") {
           |  product: Product
           |}
           |type Product @key(fields: "id") {
           |  id: ID!
           |  label(parentType: String @fromContext(field: "$$topLevelQuery { __typename }")): String
           |}
           |""".stripMargin

      for {
        remote  <- stub("{}")
        runtime <- Gateway.compose(Subgraph.federation("products", remote.endpoint, schema)).interpreter
        plan    <- runtime.explain("{ product { label } }").exit
      } yield assertTrue(plan.isSuccess)
    },
    test("preserves list wrappers in nested context selections") {
      val schema =
        s"""
           |${preamble("@key", "@context", "@fromContext")}
           |type Query { user: User }
           |type User @context(name: "userContext") {
           |  groups: [Group!]!
           |  child: Child!
           |}
           |type Group { code: String! }
           |type Child @key(fields: "id") {
           |  id: ID!
           |  result(codes: [String!] @fromContext(field: "$$userContext { groups { code } }")): String!
           |}
           |""".stripMargin

      Gateway
        .compose(Subgraph.federation("contexts", unreachableEndpoint, schema))
        .interpreter
        .exit
        .map(exit => assertTrue(exit.isSuccess))
    },
    test("waits for an entity fetch that produces a context value") {
      val pricingSchema  =
        s"""
           |${preamble("@key", "@external", "@context", "@fromContext")}
           |type Query { product: Product }
           |type Product @key(fields: "id") @context(name: "productContext") {
           |  id: ID!
           |  currency: String! @external
           |  price(currency: String @fromContext(field: "$$productContext { currency }")): Int!
           |}
           |""".stripMargin
      val productsSchema =
        s"""
           |${preamble("@key")}
           |type Query { noop: Boolean }
           |type Product @key(fields: "id") {
           |  id: ID!
           |  currency: String!
           |}
           |""".stripMargin

      for {
        pricing      <- stubByRequest(request =>
                          if (request.query.exists(_.contains("price(currency:\"USD\")")))
                            """{"data":{"_entities":[{"price":9}]}}"""
                          else
                            """{"data":{"product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product"}}}"""
                        )
        products     <- stub("""{"data":{"_entities":[{"_caliban_gateway_requirement_currency":"USD"}]}}""")
        runtime      <- Gateway
                          .compose(
                            Subgraph.federation("pricing", pricing.endpoint, pricingSchema),
                            Subgraph.federation("products", products.endpoint, productsSchema)
                          )
                          .interpreter
        response     <- runtime.execute("{ product { price } }")
        pricingSent  <- pricing.requests.get
        productsSent <- products.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "product").flatMap(field(_, "price")).contains(IntNumber(9)),
        pricingSent.size == 2,
        productsSent.size == 1,
        pricingSent.lastOption.flatMap(_.query).exists(_.contains("price(currency:\"USD\")"))
      )
    },
    test("preserves custom-scalar values at direct and nested selector leaves") {
      val schema =
        s"""
           |${preamble("@key", "@context", "@fromContext")}
           |scalar JSON
           |type Query { user: User }
           |type User @context(name: "userContext") {
           |  metadata: JSON!
           |  profile: Profile!
           |  child: Child!
           |}
           |type Profile { metadata: JSON! }
           |type Child @key(fields: "id") {
           |  id: ID!
           |  result(
           |    direct: JSON @fromContext(field: "$$userContext { metadata }")
           |    nested: JSON @fromContext(field: "$$userContext { profile { metadata } }")
           |  ): String!
           |}
           |""".stripMargin

      for {
        remote   <- stubByRequest(request =>
                      if (request.query.exists(_.contains("_entities")))
                        """{"data":{"_entities":[{"result":"ok"}]}}"""
                      else
                        """{"data":{"user":{"_caliban_gateway_requirement_metadata":{"locale":"en"},"_caliban_gateway_requirement_profile_metadata":{"metadata":{"locale":"fr"}},"child":{"_caliban_gateway_key":"c1","_caliban_gateway_typename":"Child"}}}}"""
                    )
        runtime  <- Gateway.compose(Subgraph.federation("contexts", remote.endpoint, schema)).interpreter
        response <- runtime.execute("{ user { child { result } } }")
        sent     <- remote.requests.get
        entity    = sent.lastOption.flatMap(_.query).getOrElse("")
      } yield assertTrue(
        response.errors.isEmpty,
        entity.contains("direct:{locale:\"en\"}"),
        entity.contains("nested:{locale:\"fr\"}")
      )
    },
    test("rejects a required non-contextual argument in another subgraph") {
      val contextual =
        s"""
           |${preamble("@key", "@shareable", "@context", "@fromContext")}
           |type Query { product: Product }
           |type Product @key(fields: "id") @context(name: "productContext") {
           |  id: ID!
           |  locale: String!
           |  price(locale: String @fromContext(field: "$$productContext { locale }")): Int! @shareable
           |}
           |""".stripMargin
      val required   =
        s"""
           |${preamble("@key", "@shareable")}
           |type Query { product: Product }
           |type Product @key(fields: "id") {
           |  id: ID!
           |  price(locale: String!): Int! @shareable
           |}
           |""".stripMargin

      compositionDiagnostics(
        Gateway.compose(
          Subgraph.federation("contextual", unreachableEndpoint, contextual),
          Subgraph.federation("required", unreachableEndpoint, required)
        )
      ).map(diagnostics => assertTrue(diagnostics.exists(_.contains("must be nullable or define a default value"))))
    },
    test("ignores non-contextual inaccessible arguments during compatibility checks") {
      val withoutArgument =
        s"""
           |${preamble("@shareable", "@inaccessible")}
           |type Query { search: String @shareable }
           |""".stripMargin
      val hiddenArgument  =
        s"""
           |${preamble("@shareable", "@inaccessible")}
           |type Query { search(tenant: String @inaccessible): String @shareable }
           |""".stripMargin

      Gateway
        .compose(
          Subgraph.federation("without", unreachableEndpoint, withoutArgument),
          Subgraph.federation("hidden", unreachableEndpoint, hiddenArgument)
        )
        .interpreter
        .exit
        .map(exit => assertTrue(exit.isSuccess))
    },
    test("rejects contexts on mutation and subscription root types") {
      def schema(operation: String): String =
        s"""
           |${preamble("@context")}
           |type Query { noop: Boolean }
           |type $operation @context(name: "rootContext") { value: String }
           |""".stripMargin

      for {
        mutation     <- compositionDiagnostics(
                          Gateway.compose(Subgraph.federation("mutation", unreachableEndpoint, schema("Mutation")))
                        )
        subscription <-
          compositionDiagnostics(
            Gateway.compose(Subgraph.federation("subscription", unreachableEndpoint, schema("Subscription")))
          )
      } yield assertTrue(
        mutation.exists(_.contains("not supported on the Mutation root type")),
        subscription.exists(_.contains("not supported on the Subscription root type"))
      )
    },
    test("accepts type-conditioned selectors across context locations") {
      val blockString = "\"\"\""
      val schema      =
        s"""
           |${preamble("@key", "@context", "@fromContext")}
           |type Query { foo: Foo bar: Bar }
           |type Foo @context(name: "sharedContext") {
           |  value: String!
           |  child: Child!
           |}
           |type Bar @context(name: "sharedContext") {
           |  otherValue: String!
           |  child: Child!
           |}
           |type Child @key(fields: "id") {
           |  id: ID!
           |  result(value: String @fromContext(field: $blockString
           |    , # before context
           |    $$, # before name
           |    sharedContext ... on Foo { value } ... on Bar { otherValue }
           |  $blockString)): String!
           |}
           |""".stripMargin

      Gateway
        .compose(Subgraph.federation("contexts", unreachableEndpoint, schema))
        .interpreter
        .exit
        .map(exit => assertTrue(exit.isSuccess))
    },
    test("validates the context feature version, names, selectors, and argument nullability") {
      val abstractCondition =
        s"""
           |${preamble("@key", "@context", "@fromContext")}
           |type Query { child: Child }
           |interface Node @context(name: "nodeContext") { value: String! }
           |type User implements Node { value: String! }
           |type Child @key(fields: "id") {
           |  id: ID!
           |  result(value: String @fromContext(field: "$$nodeContext ... on Node { value }")): String!
           |}
           |""".stripMargin
      val unusedCondition   =
        s"""
           |${preamble("@key", "@context", "@fromContext")}
           |type Query { foo: Foo }
           |type Foo @context(name: "fooContext") { value: String! child: Child! }
           |type Bar { other: String! }
           |type Child @key(fields: "id") {
           |  id: ID!
           |  result(value: String @fromContext(field: "$$fooContext ... on Foo { value } ... on Bar { other }")): String!
           |}
           |""".stripMargin
      val nestedCondition   =
        s"""
           |${preamble("@key", "@context", "@fromContext")}
           |type Query { holder: Holder }
           |type Holder @context(name: "holderContext") { node: Node! child: Child! }
           |interface Node { id: ID! }
           |type User implements Node { id: ID! }
           |type Child @key(fields: "id") {
           |  id: ID!
           |  result(value: ID @fromContext(field: "$$holderContext { node { ... on User { id } } }")): String!
           |}
           |""".stripMargin
      val schemas           = List(
        userContextSchema.replace("federation/v2.8", "federation/v2.7"),
        userContextSchema.replace("name: \"userContext\"", "name: \"user_context\""),
        userContextSchema.replace("{ currency }", "{ missing }"),
        userContextSchema.replace("currency: String @fromContext", "currency: String! @fromContext"),
        userContextSchema.replace("amount(currency: String @fromContext", "amount(currency: Int @fromContext"),
        userContextSchema.replace("{ currency }", "{ id currency }"),
        userContextSchema.replace("{ currency }", "{ currency @skip(if: true) }"),
        userContextSchema.replace("currency: String @fromContext", "currency: String = \"USD\" @fromContext"),
        userContextSchema.replace(
          "$userContext { currency }",
          "$userContext ... on User { currency } ... on User { currency }"
        ),
        abstractCondition,
        unusedCondition,
        nestedCondition
      )

      for {
        diagnostics <- ZIO.foreach(schemas.zipWithIndex) { case (schema, index) =>
                         compositionDiagnostics(
                           Gateway.compose(Subgraph.federation(s"invalid-$index", unreachableEndpoint, schema))
                         )
                       }
      } yield assertTrue(
        diagnostics(0).exists(_.contains("not available in the linked feature version")),
        diagnostics(1).exists(_.contains("Invalid Federation @context name")),
        diagnostics(2).exists(message => message.contains("@fromContext") && message.contains("missing")),
        diagnostics(3).exists(_.contains("context arguments must be nullable")),
        diagnostics(4).exists(_.contains("selected value is incompatible")),
        diagnostics(5).exists(_.contains("multiple fields")),
        diagnostics(6).exists(_.contains("directives are not allowed")),
        diagnostics(7).exists(_.contains("must not define a default value")),
        diagnostics(8).exists(_.contains("multiple fields")),
        diagnostics(9).exists(_.contains("concrete object types")),
        diagnostics(10).exists(_.contains("do not match a context location")),
        diagnostics(11).exists(_.contains("inline fragments are only allowed at the top level"))
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

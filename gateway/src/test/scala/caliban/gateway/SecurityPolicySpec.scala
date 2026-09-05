package caliban.gateway

import caliban.Value.{ BooleanValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.OperationPolicy.SecurityDirective.{ Authenticated, RequiresScopes }
import caliban.gateway.OperationPolicy.{ Reject, SecurityRequirement }
import caliban.GraphQLRequest
import zio._
import zio.test._

object SecurityPolicySpec extends ZIOSpecDefault {

  private val linkDefinitions =
    """
      |directive @link(url: String!, as: String, import: [link__Import]) repeatable on SCHEMA
      |scalar link__Import
      |scalar fed__Scope
      |scalar fed__Policy
      |""".stripMargin

  private val securitySchema =
    s"""
       |extend schema @link(
       |  url: "https://specs.apollo.dev/federation/v2.9"
       |  as: "fed"
       |  import: [
       |    { name: "@authenticated", as: "@loggedIn" }
       |    { name: "@requiresScopes", as: "@guarded" }
       |  ]
       |)
       |$linkDefinitions
       |directive @loggedIn on FIELD_DEFINITION | OBJECT
       |directive @guarded(scopes: [[fed__Scope!]!]!) on FIELD_DEFINITION | OBJECT
       |type Query @loggedIn {
       |  node: Node @guarded(scopes: [["read:node"]])
       |}
       |interface Node { id: ID! secret: String }
       |type Public implements Node { id: ID! secret: String }
       |type Private implements Node @guarded(scopes: [["read:private", "tenant:a"], ["admin"]]) {
       |  id: ID!
       |  secret: String @guarded(scopes: [["read:secret"]])
       |}
       |""".stripMargin

  private val request = GraphQLRequest(
    query = Some(
      """
        |query Secure($show: Boolean! = true) {
        |  result: node {
        |    id
        |    ...PrivateFields @include(if: $show)
        |  }
        |}
        |fragment PrivateFields on Private { hidden: secret }
        |""".stripMargin
    ),
    operationName = Some("Secure")
  )

  private val claimsSchema =
    s"""
       |extend schema @link(
       |  url: "https://specs.apollo.dev/federation/v2.9"
       |  import: ["@authenticated", "@requiresScopes"]
       |)
       |$linkDefinitions
       |directive @authenticated on FIELD_DEFINITION | OBJECT
       |directive @requiresScopes(scopes: [[String!]!]!) on FIELD_DEFINITION | OBJECT
       |type Query {
       |  open: String
       |  login: String @authenticated
       |  scoped: String @authenticated @requiresScopes(scopes: [["read", "tenant"], ["admin"]])
       |  other: String @requiresScopes(scopes: [["other"]])
       |  empty: String @requiresScopes(scopes: [])
       |  emptyAlternative: String @requiresScopes(scopes: [[]])
       |  node: Node
       |}
       |interface Node { value: String }
       |type Public implements Node { value: String }
       |type Private implements Node @requiresScopes(scopes: [["read:private"]]) { value: String }
       |""".stripMargin

  private final case class Claims(scope: String)

  private trait RequestClaims {
    def current: Task[Option[Claims]]
  }

  private def claimScopes(claims: Claims): Set[String] =
    claims.scope.split(" ").filter(_.nonEmpty).toSet

  private def assertLookupGuard(guardRoot: Boolean, single: Boolean, renamed: Boolean) = {
    val guard      = "@policy(policies: [[\"owner\"]])"
    val lookupName = if (single) "productById" else "productsByIds"
    val schema     = s"""extend schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@policy"])
                    |$linkDefinitions
                    |directive @policy(policies: [[String!]!]!) on FIELD_DEFINITION
                    |type Query {
                    |  productsByIds(ids: [ID!]!): [Product!]! ${if (guardRoot && !single) guard else ""}
                    |  productById(id: ID!): Product ${if (guardRoot && single) guard else ""}
                    |}
                    |type Product { id: ID! externalId: ID! ${if (guardRoot) "" else guard} review: String }
                    |""".stripMargin
    val lookup     =
      if (single) Lookup.single("Product", List("id"), lookupName, "id" -> Lookup.Argument.key("id"))
      else
        Lookup.list(
          "Product",
          List("id"),
          lookupName,
          Map("externalId" -> "id"),
          "ids"                                                         -> Lookup.Argument.batch(Lookup.Argument.key("id"))
        )
    val joined     = GraphQLRequest(query =
      Some("query Joined($fetch: Boolean! = true) { product { id review @include(if: $fetch) } }")
    )

    for {
      products     <- stub("""{"data":{"product":{"id":"p1","_caliban_gateway_key":"p1"}}}""")
      reviews      <- stub("""{"data":{"_caliban_gateway_lookup":[]}}""")
      source        = Subgraph.graphql("reviews", reviews.endpoint, schema).withLookup(lookup)
      transformed   = if (renamed)
                        source.transform(
                          SchemaTransformation.renameField("Query", lookupName, "lookup"),
                          SchemaTransformation.renameField("Product", "externalId", "correlation")
                        )
                      else source
      runtime      <-
        Gateway
          .compose(
            Subgraph.graphql("products", products.endpoint, "type Query { product: Product } type Product { id: ID! }"),
            transformed
          )
          .withOperationPolicy(OperationPolicy[Any](_ => ZIO.succeed(OperationPolicy.Allow)))
          .interpreter
      root          = if (renamed) "lookup" else lookupName
      arguments     = if (single) "id: \"p1\"" else "ids: [\"p1\"]"
      selection     = if (guardRoot) "review" else if (renamed) "correlation" else "externalId"
      direct       <- runtime.execute(s"{ $root($arguments) { $selection } }")
      first        <- runtime.executeRequest(joined)
      cached       <- runtime.executeRequest(joined)
      explanation  <- runtime.explain(joined).either
      skipped      <- runtime.executeRequest(joined.copy(variables = Some(Map("fetch" -> BooleanValue(false)))))
      productCalls <- products.requests.get
      reviewCalls  <- reviews.requests.get
    } yield assertTrue(
      List(direct, first, cached).forall(_.errors.exists(_.msg.contains("unsupported @policy"))),
      explanation.left.exists(_.msg.contains("unsupported @policy")),
      skipped.errors.isEmpty,
      productCalls.size == 1,
      reviewCalls.isEmpty
    )
  }

  def spec = suite("SecurityPolicySpec")(
    test("enforces authentication and scope alternatives") {
      val cases = List(
        ("{ login }", None, false),
        ("{ login }", Some(""), true),
        ("{ scoped }", None, false),
        ("{ scoped }", Some(""), false),
        ("{ scoped }", Some("read"), false),
        ("{ scoped }", Some("tenant"), false),
        ("{ scoped }", Some("read tenant extra"), true),
        ("{ scoped }", Some("admin"), true),
        ("{ scoped }", Some("Admin"), false),
        ("{ other }", None, false),
        ("{ other }", Some("other"), true),
        ("{ scoped other }", Some("admin"), false),
        ("{ scoped other }", Some("admin other"), true),
        ("{ empty }", None, false),
        ("{ empty }", Some(""), true),
        ("{ emptyAlternative }", None, false),
        ("{ emptyAlternative }", Some(""), true)
      )

      for {
        remote  <-
          stub(
            """{"data":{"open":"ok","login":"ok","scoped":"ok","other":"ok","empty":"ok","emptyAlternative":"ok"}}"""
          )
        claims  <- FiberRef.make(Option.empty[Claims])
        runtime <- Gateway
                     .compose(Subgraph.federation("claims", remote.endpoint, claimsSchema))
                     .withOperationPolicy(OperationPolicy.fromClaims(claims.get)(claimScopes))
                     .interpreter
        results <- ZIO.foreach(cases) { case (query, scope, allowed) =>
                     claims.locally(scope.map(Claims(_)))(runtime.execute(query)).map { result =>
                       assertTrue(
                         result.errors.map(_.msg) == (if (allowed) Nil
                                                      else List("Operation rejected by gateway policy."))
                       )
                     }
                   }
        sent    <- remote.requests.get
      } yield results.reduce(_ && _) && assertTrue(sent.size == cases.count(_._3))
    },
    test("reads request claims once per protected execution including cache hits") {
      val query = "{ login scoped }"

      for {
        remote   <- stub("""{"data":{"login":"ok","scoped":"ok"}}""")
        calls    <- Ref.make(0)
        claims   <- FiberRef.make(Option.empty[Claims])
        service   = new RequestClaims {
                      def current: Task[Option[Claims]] = calls.update(_ + 1) *> claims.get
                    }
        policy    = OperationPolicy.fromClaims(ZIO.serviceWithZIO[RequestClaims](_.current))(claimScopes)
        runtime  <- Gateway
                      .compose(Subgraph.federation("claims", remote.endpoint, claimsSchema))
                      .withOperationPolicy(policy)
                      .interpreter
        allowed  <- claims
                      .locally(Some(Claims("admin")))(runtime.execute(query))
                      .provideLayer(ZLayer.succeed(service))
        rejected <- runtime.execute(query).provideLayer(ZLayer.succeed(service))
        reads    <- calls.get
        sent     <- remote.requests.get
      } yield assertTrue(
        allowed.errors.isEmpty,
        rejected.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        reads == 2,
        sent.size == 1
      )
    },
    test("skips claims for public selections and masks lookup failures on protected selections") {
      val query = GraphQLRequest(
        query = Some("query($show: Boolean!) { open login @include(if: $show) }"),
        variables = Some(Map("show" -> BooleanValue(false)))
      )

      for {
        remote          <- stub("""{"data":{"open":"ok"}}""")
        calls           <- Ref.make(0)
        readClaims       = calls.update(_ + 1) *> ZIO.fail(new RuntimeException("claims-secret"))
        runtime         <- Gateway
                             .compose(Subgraph.federation("claims", remote.endpoint, claimsSchema))
                             .withOperationPolicy(OperationPolicy.fromClaims[Any, Claims](readClaims)(claimScopes))
                             .interpreter
        publicResult    <- runtime.executeRequest(query)
        introspection   <- runtime.execute("{ __schema { queryType { name } } }")
        protectedResult <- runtime.executeRequest(
                             query.copy(variables = Some(Map("show" -> BooleanValue(true))))
                           )
        reads           <- calls.get
        sent            <- remote.requests.get
      } yield assertTrue(
        publicResult.errors.isEmpty,
        introspection.errors.isEmpty,
        protectedResult.errors.map(_.msg) == List("Operation policy failed."),
        reads == 1,
        sent.size == 1
      )
    },
    test("masks scope mapping defects without executing protected operations") {
      val scopes: Claims => Set[String] = _ => throw new RuntimeException("scopes-secret")
      for {
        remote  <- stub("""{"data":{"login":"ok"}}""")
        runtime <- Gateway
                     .compose(Subgraph.federation("claims", remote.endpoint, claimsSchema))
                     .withOperationPolicy(OperationPolicy.fromClaims(ZIO.some(Claims("")))(scopes))
                     .interpreter
        result  <- runtime.execute("{ login }")
        sent    <- remote.requests.get
      } yield assertTrue(result.errors.map(_.msg) == List("Operation policy failed."), sent.isEmpty)
    },
    test("conservatively enforces protected runtime branches before contacting a source") {
      val query = "{ node { value } }"

      for {
        remote       <- stub("""{"data":{"node":{"_caliban_gateway_runtime_typename":"Public","value":"ok"}}}""")
        claims       <- FiberRef.make(Option.empty[Claims])
        runtime      <- Gateway
                          .compose(Subgraph.federation("claims", remote.endpoint, claimsSchema))
                          .withOperationPolicy(OperationPolicy.fromClaims(claims.get)(claimScopes))
                          .interpreter
        rejected     <- claims.locally(Some(Claims("")))(runtime.execute(query))
        publicResult <- runtime.execute("{ node { ... on Public { value } } }")
        allowed      <- claims.locally(Some(Claims("read:private")))(runtime.execute(query))
        sent         <- remote.requests.get
      } yield assertTrue(
        rejected.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        publicResult.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        allowed.errors.isEmpty,
        sent.size == 1
      )
    },
    test("requires a policy for aliased and namespace-qualified Federation security directives") {
      for {
        remote     <- stub("""{"data":{"node":null}}""")
        exit       <- Gateway.compose(Subgraph.federation("secure", remote.endpoint, securitySchema)).interpreter.exit
        diagnostics = buildDiagnostics(exit)
        sent       <- remote.requests.get
      } yield assertTrue(
        diagnostics.exists(message => message.startsWith("[secure]") && message.contains("@authenticated")),
        diagnostics.exists(message => message.startsWith("[secure]") && message.contains("@requiresScopes")),
        sent.isEmpty
      )
    },
    test("rejects policy selections at execution even with an allowing policy, but serves public selections") {
      val expressions = List("[]", "[[]]", "[[\"owner\"]]")
      for {
        remote     <- stub("""{"data":{"open":"ok"}}""")
        claimsRead <- Ref.make(0)
        configured  = List(
                        Option.empty[OperationPolicy[Any]],
                        Some(OperationPolicy[Any](_ => ZIO.succeed(OperationPolicy.Allow))),
                        Some(OperationPolicy.fromClaims(claimsRead.update(_ + 1).as(Some(Claims("admin"))))(claimScopes))
                      )
        results    <-
          ZIO.foreach(expressions) { expression =>
            val schema = s"""extend schema @link(url: "https://specs.apollo.dev/federation/v2.9",
                            | import: [{ name: "@policy", as: "@guarded" }])
                            |$linkDefinitions
                            |directive @guarded(policies: [[String!]!]!) on FIELD_DEFINITION
                            |type Query { open: String value: String @guarded(policies: $expression) }
                            |""".stripMargin
            ZIO
              .foreach(configured) { policy =>
                val gateway = Gateway.compose(Subgraph.federation("secure", remote.endpoint, schema))
                for {
                  runtime  <- policy.fold(gateway)(gateway.withOperationPolicy(_)).interpreter
                  first    <- runtime.execute("{ value }")
                  cached   <- runtime.execute("{ value }")
                  request   =
                    GraphQLRequest(query = Some("query($show: Boolean!) { open alias: value @include(if: $show) }"))
                  skipped  <- runtime.executeRequest(request.copy(variables = Some(Map("show" -> BooleanValue(false)))))
                  included <- runtime.executeRequest(request.copy(variables = Some(Map("show" -> BooleanValue(true)))))
                } yield assertTrue(
                  List(first, cached, included).forall(_.errors.exists(_.msg.contains("unsupported @policy"))),
                  skipped.errors.isEmpty,
                  field(skipped.data, "open").contains(StringValue("ok"))
                )
              }
              .map(_.reduce(_ && _))
          }
        reads      <- claimsRead.get
        sent       <- remote.requests.get
      } yield results.reduce(_ && _) && assertTrue(reads == 0, sent.size == expressions.size * configured.size)
    },
    test("blocks hidden transitive policy dependencies without rejecting composition") {
      val schema =
        s"""
           |${federationSchemaPreambleWithQueryRoot("@policy", "@inaccessible", "@requires", "@key").replace(
            "v2.3",
            "v2.9"
          )}
           |directive @policy(policies: [[String!]!]!) on FIELD_DEFINITION
           |directive @requires(fields: federation__FieldSet!) on FIELD_DEFINITION
           |type Query { product: Product }
           |type Product @key(fields: "id") {
           |  id: ID!
           |  secret: String @policy(policies: [["owner"]]) @inaccessible
           |  bridge: String @requires(fields: "secret")
           |  visible: String @requires(fields: "bridge")
           |  public: String
           |}
           |""".stripMargin
      for {
        remote  <- stub("""{"data":{"product":{"public":"ok"}}}""")
        runtime <- Gateway.compose(Subgraph.federation("secure", remote.endpoint, schema)).interpreter
        denied  <- runtime.execute("{ product { visible } }")
        public  <- runtime.execute("{ product { public } }")
        sent    <- remote.requests.get
      } yield assertTrue(
        denied.errors.exists(_.msg.contains("unsupported @policy")),
        public.errors.isEmpty,
        sent.size == 1
      )
    },
    test("blocks hidden policy dependencies on a custom query root") {
      val schema =
        s"""
           |${federationSchemaPreambleWithQueryRoot("@policy", "@inaccessible", "@requires")
            .replace("v2.3", "v2.9")
            .replace("query: Query", "query: RootQuery")}
           |directive @policy(policies: [[String!]!]!) on FIELD_DEFINITION
           |directive @requires(fields: federation__FieldSet!) on FIELD_DEFINITION
           |type RootQuery {
           |  secret: String @policy(policies: [["owner"]]) @inaccessible
           |  visible: String @requires(fields: "secret")
           |  public: String
           |}
           |""".stripMargin
      for {
        remote  <- stub("""{"data":{"public":"ok"}}""")
        runtime <- Gateway.compose(Subgraph.federation("secure", remote.endpoint, schema)).interpreter
        denied  <- runtime.execute("{ visible }")
        public  <- runtime.execute("{ public }")
        sent    <- remote.requests.get
      } yield assertTrue(
        denied.errors.exists(_.msg.contains("unsupported @policy")),
        public.errors.isEmpty,
        sent.size == 1
      )
    },
    test("blocks policy guarded keys injected for entity lookups") {
      val schema = productsFederationSchema
        .replace("v2.3", "v2.9")
        .replace("import: [\"@key\"]", "import: [\"@key\", \"@policy\"]")
        .replace("id: ID! name:", "id: ID! @policy(policies: [[\"owner\"]]) name:") +
        " directive @policy(policies: [[String!]!]!) on FIELD_DEFINITION"
      for {
        products     <- stub("""{"data":{"product":{"name":"Table"}}}""")
        reviews      <- stub("""{"data":{"_entities":[]}}""")
        runtime      <- Gateway
                          .compose(
                            Subgraph.federation("products", products.endpoint, schema),
                            Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                          )
                          .interpreter
        denied       <- runtime.execute("{ product(id: \"p1\") { reviews { body } } }")
        public       <- runtime.execute("{ product(id: \"p1\") { name } }")
        productCalls <- products.requests.get
        reviewCalls  <- reviews.requests.get
      } yield assertTrue(
        denied.errors.exists(_.msg.contains("unsupported @policy")),
        public.errors.isEmpty,
        productCalls.size == 1,
        reviewCalls.isEmpty
      )
    },
    test("blocks guarded ordinary lookup roots for single and list lookups, including renamed fields") {
      ZIO
        .foreach(List(false, true))(single =>
          ZIO
            .foreach(List(false, true))(renamed => assertLookupGuard(guardRoot = true, single, renamed))
            .map(_.reduce(_ && _))
        )
        .map(_.reduce(_ && _))
    },
    test("blocks guarded correlation fields generated by ordinary lookups, including renamed fields") {
      ZIO
        .foreach(List(false, true))(renamed => assertLookupGuard(guardRoot = false, single = false, renamed))
        .map(_.reduce(_ && _))
    },
    test("recognizes standalone linked security features in Federation 1 schemas") {
      val schema =
        s"""
           |extend schema
           |  @link(url: "https://specs.apollo.dev/authenticated/v0.1", as: "login")
           |  @link(
           |    url: "https://specs.apollo.dev/requiresScopes/v0.1"
           |    import: [{ name: "@requiresScopes", as: "@guarded" }]
           |  )
           |$linkDefinitions
           |directive @login on FIELD_DEFINITION | OBJECT
           |directive @guarded(scopes: [[String!]!]!) on FIELD_DEFINITION | OBJECT
           |type Query @login {
           |  value: String @guarded(scopes: [["read:value"]])
           |}
           |""".stripMargin

      for {
        remote     <- stub("""{"data":{"value":"ok"}}""")
        exit       <- Gateway.compose(Subgraph.federation("federation-one", remote.endpoint, schema)).interpreter.exit
        diagnostics = buildDiagnostics(exit)
        sent       <- remote.requests.get
      } yield assertTrue(
        diagnostics.exists(message => message.startsWith("[federation-one]") && message.contains("@authenticated")),
        diagnostics.exists(message => message.startsWith("[federation-one]") && message.contains("@requiresScopes")),
        sent.isEmpty
      )
    },
    test("retains security applications from every composed subgraph field") {
      val authenticatedSchema =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.8"
           |  as: "fed"
           |  import: ["@shareable", "@authenticated"]
           |)
           |$linkDefinitions
           |directive @shareable repeatable on OBJECT | FIELD_DEFINITION
           |directive @authenticated on FIELD_DEFINITION | OBJECT
           |type Query { value: String @shareable @authenticated }
           |""".stripMargin
      val scopesSchema        =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.8"
           |  as: "fed"
           |  import: ["@shareable", "@requiresScopes"]
           |)
           |$linkDefinitions
           |directive @shareable repeatable on OBJECT | FIELD_DEFINITION
           |directive @requiresScopes(scopes: [[fed__Scope!]!]!) on FIELD_DEFINITION | OBJECT
           |type Query { value: String @shareable @requiresScopes(scopes: [["read:value"]]) }
           |""".stripMargin

      for {
        alpha     <- stub("""{"data":{"value":"alpha"}}""")
        beta      <- stub("""{"data":{"value":"beta"}}""")
        observed  <- Ref.make(List.empty[SecurityRequirement])
        policy     = OperationPolicy[Any](operation => observed.set(operation.securityRequirements).as(Reject()))
        runtime   <- Gateway
                       .compose(
                         Subgraph.federation("alpha", alpha.endpoint, authenticatedSchema),
                         Subgraph.federation("beta", beta.endpoint, scopesSchema)
                       )
                       .withOperationPolicy(policy)
                       .interpreter
        _         <- runtime.execute("{ value }")
        seen      <- observed.get
        alphaSent <- alpha.requests.get
        betaSent  <- beta.requests.get
      } yield assertTrue(
        seen == List(
          SecurityRequirement("Query", Some("value"), List(Authenticated, RequiresScopes(List(List("read:value")))))
        ),
        alphaSent.isEmpty,
        betaSent.isEmpty
      )
    },
    test("exposes effective security requirements before cached operations reach a source") {
      val merged = request.copy(query =
        Some(
          """query Secure($show: Boolean! = true) {
            |  result: node { id }
            |  ...RootFields
            |}
            |fragment RootFields on Query {
            |  result: node { ...PrivateFields @include(if: $show) }
            |}
            |fragment PrivateFields on Private { hidden: secret }
            |""".stripMargin
        )
      )
      for {
        remote        <- stub("""{"data":{"node":null}}""")
        observed      <- Ref.make(Vector.empty[List[SecurityRequirement]])
        policy         = OperationPolicy[Any] { operation =>
                           observed.update(_ :+ operation.securityRequirements).as(Reject())
                         }
        runtime       <- Gateway
                           .compose(Subgraph.federation("secure", remote.endpoint, securitySchema))
                           .withOperationPolicy(policy)
                           .interpreter
        included      <- runtime.executeRequest(request)
        skipped       <- runtime.executeRequest(
                           request.copy(variables = Some(Map("show" -> BooleanValue(false))))
                         )
        introspection <- runtime.execute("{ __schema { queryType { name } } }")
        _             <- runtime.executeRequest(merged)
        _             <- runtime.executeRequest(merged.copy(variables = Some(Map("show" -> BooleanValue(false)))))
        seen          <- observed.get
        sent          <- remote.requests.get
        first          = seen.headOption.getOrElse(Nil)
        second         = seen.drop(1).headOption.getOrElse(Nil)
      } yield assertTrue(
        included.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        skipped.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        introspection.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        first.contains(
          SecurityRequirement("Query", None, List(Authenticated))
        ),
        first.contains(
          SecurityRequirement("Query", Some("node"), List(RequiresScopes(List(List("read:node")))))
        ),
        first.contains(
          SecurityRequirement(
            "Private",
            None,
            List(RequiresScopes(List(List("read:private", "tenant:a"), List("admin"))))
          )
        ),
        first.contains(
          SecurityRequirement("Private", Some("secret"), List(RequiresScopes(List(List("read:secret")))))
        ),
        !second.exists(requirement => requirement.typeName == "Private" && requirement.fieldName.contains("secret")),
        seen.drop(2).headOption.exists(_.isEmpty),
        seen.lift(3).exists(_.toSet == first.toSet),
        seen.lift(4).exists(_.toSet == second.toSet),
        seen.size == 5,
        sent.isEmpty
      )
    },
    test("retains nested protected selections in runtime branches") {
      val schema =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.9"
           |  import: ["@authenticated"]
           |)
           |$linkDefinitions
           |directive @authenticated on FIELD_DEFINITION | OBJECT
           |type Query { node: Node }
           |interface Node { child: Child }
           |type Public implements Node { child: Child }
           |type Private implements Node { child: Child }
           |type Child { secret: String @authenticated }
           |""".stripMargin

      for {
        remote   <- stub("""{"data":{"node":null}}""")
        observed <- Ref.make(List.empty[SecurityRequirement])
        policy    = OperationPolicy[Any](operation => observed.set(operation.securityRequirements).as(Reject()))
        runtime  <- Gateway
                      .compose(Subgraph.federation("nested", remote.endpoint, schema))
                      .withOperationPolicy(policy)
                      .interpreter
        _        <- runtime.execute("{ node { ... on Private { child { secret } } } }")
        seen     <- observed.get
        sent     <- remote.requests.get
      } yield assertTrue(
        seen.contains(
          SecurityRequirement("Child", Some("secret"), List(Authenticated))
        ),
        sent.isEmpty
      )
    },
    test("applies protected intermediate interface fields through a parent interface") {
      val schema =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.9"
           |  import: ["@authenticated"]
           |)
           |$linkDefinitions
           |directive @authenticated on FIELD_DEFINITION | OBJECT | INTERFACE
           |type Query { node: Node }
           |interface Node { value: String }
           |interface ChildNode implements Node { value: String @authenticated }
           |type Child implements Node & ChildNode { value: String }
           |type Other implements Node { value: String }
           |""".stripMargin

      for {
        remote   <- stub("""{"data":{"node":null}}""")
        observed <- Ref.make(List.empty[SecurityRequirement])
        policy    = OperationPolicy[Any](operation => observed.set(operation.securityRequirements).as(Reject()))
        runtime  <- Gateway
                      .compose(Subgraph.federation("interfaces", remote.endpoint, schema))
                      .withOperationPolicy(policy)
                      .interpreter
        _        <- runtime.execute("{ node { value } }")
        seen     <- observed.get
        sent     <- remote.requests.get
      } yield assertTrue(
        seen.contains(
          SecurityRequirement("ChildNode", Some("value"), List(Authenticated))
        ),
        sent.isEmpty
      )
    },
    test("rejects protected coordinates removed from the client schema") {
      val schema =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.9"
           |  import: ["@authenticated", "@inaccessible", "@requires"]
           |)
           |$linkDefinitions
           |directive @authenticated on FIELD_DEFINITION | OBJECT
           |directive @inaccessible on FIELD_DEFINITION
           |directive @requires(fields: String!) on FIELD_DEFINITION
           |type Query { product: Product }
           |type Product {
           |  secret: String @authenticated @inaccessible
           |  visible: String @requires(fields: "secret")
           |}
           |""".stripMargin

      for {
        remote     <- stub("""{"data":{"product":null}}""")
        policy      = OperationPolicy[Any](_ => ZIO.succeed(OperationPolicy.Allow))
        exit       <- Gateway
                        .compose(Subgraph.federation("hidden", remote.endpoint, schema))
                        .withOperationPolicy(policy)
                        .interpreter
                        .exit
        diagnostics = buildDiagnostics(exit)
        sent       <- remote.requests.get
      } yield assertTrue(
        diagnostics.exists(message =>
          message.startsWith("[hidden]") && message.contains("@authenticated") && message.contains("Product.secret")
        ),
        sent.isEmpty
      )
    },
    test("rejects public fields missing transitive security requirements") {
      val schema =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.9"
           |  import: ["@authenticated", "@requires"]
           |)
           |$linkDefinitions
           |directive @authenticated on FIELD_DEFINITION | OBJECT
           |directive @requires(fields: String!) on FIELD_DEFINITION
           |type Query { product: Product }
           |type Product {
           |  secret: String @authenticated
           |  shipping: String @requires(fields: "secret")
           |}
           |""".stripMargin

      for {
        remote     <- stub("""{"data":{"product":null}}""")
        policy      = OperationPolicy[Any](_ => ZIO.succeed(OperationPolicy.Allow))
        exit       <- Gateway
                        .compose(Subgraph.federation("transitive", remote.endpoint, schema))
                        .withOperationPolicy(policy)
                        .interpreter
                        .exit
        diagnostics = buildDiagnostics(exit)
        sent       <- remote.requests.get
      } yield assertTrue(
        diagnostics.exists(message =>
          message.startsWith("[transitive]") && message.contains("Product.shipping") &&
            message.contains("Product.secret")
        ),
        sent.isEmpty
      )
    },
    test("rejects public fields missing security required by a context selector") {
      val schema =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.8"
           |  import: ["@key", "@authenticated", "@context", "@fromContext"]
           |)
           |$linkDefinitions
           |directive @authenticated on FIELD_DEFINITION | OBJECT
           |directive @key(fields: String!) repeatable on OBJECT | INTERFACE
           |directive @context(name: String!) repeatable on OBJECT | INTERFACE | UNION
           |directive @fromContext(field: String!) on ARGUMENT_DEFINITION
           |type Query { user: User }
           |type User @context(name: "userContext") {
           |  secret: String @authenticated
           |  transaction: Transaction
           |}
           |type Transaction @key(fields: "id") {
           |  id: ID!
           |  amount(secret: String @fromContext(field: "$$userContext { secret }")): Int!
           |}
           |""".stripMargin

      for {
        remote     <- stub("""{"data":{"user":null}}""")
        policy      = OperationPolicy[Any](_ => ZIO.succeed(OperationPolicy.Allow))
        exit       <- Gateway
                        .compose(Subgraph.federation("context-security", remote.endpoint, schema))
                        .withOperationPolicy(policy)
                        .interpreter
                        .exit
        diagnostics = buildDiagnostics(exit)
        sent       <- remote.requests.get
      } yield assertTrue(
        diagnostics.exists(message =>
          message.startsWith("[context-security]") && message.contains("Transaction.amount") &&
            message.contains("@fromContext") && message.contains("User.secret")
        ),
        sent.isEmpty
      )
    },
    test("treats scopes as authenticated transitive requirements") {
      val schema =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.9"
           |  import: ["@authenticated", "@requiresScopes", "@requires"]
           |)
           |$linkDefinitions
           |directive @authenticated on FIELD_DEFINITION | OBJECT
           |directive @requiresScopes(scopes: [[fed__Scope!]!]!) on FIELD_DEFINITION | OBJECT
           |directive @requires(fields: String!) on FIELD_DEFINITION
           |type Query { product: Product }
           |type Product {
           |  secret: String @authenticated
           |  scopedShipping: String
           |    @requires(fields: "secret")
           |    @requiresScopes(scopes: [["read:shipping"]])
           |}
           |""".stripMargin

      for {
        remote <- stub("""{"data":{"product":null}}""")
        policy  = OperationPolicy[Any](_ => ZIO.succeed(OperationPolicy.Allow))
        exit   <- Gateway
                    .compose(Subgraph.federation("transitive-authentication", remote.endpoint, schema))
                    .withOperationPolicy(policy)
                    .interpreter
                    .exit
        sent   <- remote.requests.get
      } yield assertTrue(exit.isSuccess, sent.isEmpty)
    },
    test("accepts fields carrying sufficient transitive security requirements") {
      val schema =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.9"
           |  import: ["@requiresScopes", "@requires"]
           |)
           |$linkDefinitions
           |directive @requiresScopes(scopes: [[fed__Scope!]!]!) on FIELD_DEFINITION | OBJECT
           |directive @requires(fields: String!) on FIELD_DEFINITION
           |type Query { product: Product }
           |type Product {
           |  secret: String @requiresScopes(scopes: [["read", "tenant"], ["admin"]])
           |  shipping: String
           |    @requires(fields: "secret")
           |    @requiresScopes(scopes: [["tenant", "read"]])
           |}
           |""".stripMargin

      for {
        remote <- stub("""{"data":{"product":null}}""")
        policy  = OperationPolicy[Any](_ => ZIO.succeed(OperationPolicy.Allow))
        exit   <- Gateway
                    .compose(Subgraph.federation("sufficient", remote.endpoint, schema))
                    .withOperationPolicy(policy)
                    .interpreter
                    .exit
        sent   <- remote.requests.get
      } yield assertTrue(exit.isSuccess, sent.isEmpty)
    },
    test("leaves an unrelated unimported directive with the same local name alone") {
      val schema =
        s"""
           |extend schema @link(url: "https://specs.apollo.dev/federation/v2.8", import: [])
           |$linkDefinitions
           |directive @authenticated on FIELD_DEFINITION
           |type Query { value: String @authenticated }
           |""".stripMargin

      for {
        remote   <- stub("""{"data":{"value":"ok"}}""")
        runtime  <- Gateway.compose(Subgraph.federation("ordinary", remote.endpoint, schema)).interpreter
        response <- runtime.execute("{ value }")
        sent     <- remote.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "value").contains(StringValue("ok")),
        sent.size == 1
      )
    },
    test("rejects security directives unavailable in the linked feature version") {
      val schema =
        s"""
           |extend schema @link(url: "https://specs.apollo.dev/federation/v2.5", import: ["@policy"])
           |$linkDefinitions
           |directive @policy(policies: [[fed__Policy!]!]!) on FIELD_DEFINITION
           |type Query { value: String @policy(policies: [["owner"]]) }
           |""".stripMargin

      for {
        remote     <- stub("""{"data":{"value":"ok"}}""")
        exit       <- Gateway.compose(Subgraph.federation("old-federation", remote.endpoint, schema)).interpreter.exit
        diagnostics = buildDiagnostics(exit)
        sent       <- remote.requests.get
      } yield assertTrue(
        diagnostics.exists(message =>
          message.startsWith("[old-federation]") && message.contains("@policy") && message.contains("version")
        ),
        sent.isEmpty
      )
    },
    test("recognizes progressive overrides and validates contexts before source execution") {
      val schema =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.8"
           |  as: "fed"
           |  import: [
           |    { name: "@override", as: "@replace" }
           |    { name: "@fromContext", as: "@inject" }
           |  ]
           |)
           |$linkDefinitions
           |directive @replace(from: String!, label: String) on FIELD_DEFINITION
           |directive @fed__context(name: String!) repeatable on OBJECT | INTERFACE | UNION
           |directive @inject(field: String!) on ARGUMENT_DEFINITION
           |type Query @fed__context(name: "tenant") {
           |  value(input: String @inject(field: "$$tenant { id }")): String
           |  migrated: String @replace(from: "legacy", label: "percent(10)")
           |}
           |""".stripMargin

      for {
        remote     <- stub("""{"data":{"value":"ok","migrated":"ok"}}""")
        exit       <- Gateway.compose(Subgraph.federation("unsupported", remote.endpoint, schema)).interpreter.exit
        diagnostics = buildDiagnostics(exit)
        sent       <- remote.requests.get
      } yield assertTrue(
        !diagnostics.exists(message => message.startsWith("[unsupported]") && message.contains("@override")),
        !diagnostics.exists(message => message.startsWith("[unsupported]") && message.contains("@context")),
        !diagnostics.exists(message =>
          message.startsWith("[unsupported]") && message.contains("@fromContext is not supported")
        ),
        diagnostics.exists(message =>
          message.startsWith("[unsupported]") && message.contains("Invalid Federation @fromContext")
        ),
        sent.isEmpty
      )
    },
    test("rejects recognized Federation directives at unsupported schema locations") {
      val schema =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.8"
           |  as: "fed"
           |  import: [
           |    "@authenticated"
           |    { name: "@fromContext", as: "@inject" }
           |  ]
           |)
           |$linkDefinitions
           |directive @authenticated on ARGUMENT_DEFINITION
           |directive @inject(field: String!) on INPUT_FIELD_DEFINITION
           |type Query {
           |  value(input: String @authenticated, filter: Filter): String
           |}
           |input Filter {
           |  term: String @inject(field: "$$tenant { id }")
           |}
           |""".stripMargin

      for {
        remote     <- stub("""{"data":{"value":"ok"}}""")
        exit       <- Gateway.compose(Subgraph.federation("invalid-locations", remote.endpoint, schema)).interpreter.exit
        diagnostics = buildDiagnostics(exit)
        sent       <- remote.requests.get
      } yield assertTrue(
        diagnostics.exists(message =>
          message.startsWith("[invalid-locations]") && message.contains("@authenticated") &&
            message.contains("Query.value(input:)")
        ),
        diagnostics.exists(message =>
          message.startsWith("[invalid-locations]") && message.contains("@fromContext") &&
            message.contains("Filter.term")
        ),
        sent.isEmpty
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

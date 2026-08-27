package caliban.gateway

import caliban.Value.{ BooleanValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.OperationPolicy.SecurityDirective.{ Authenticated, Policy, RequiresScopes }
import caliban.gateway.OperationPolicy.{ Reject, RuntimeTypeCondition, SecurityRequirement }
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
       |directive @fed__policy(policies: [[fed__Policy!]!]!) on FIELD_DEFINITION | OBJECT
       |type Query @loggedIn {
       |  node: Node @fed__policy(policies: [["owner", "region"], ["admin"]])
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
       |  import: ["@authenticated", "@requiresScopes", "@policy"]
       |)
       |$linkDefinitions
       |directive @authenticated on FIELD_DEFINITION | OBJECT
       |directive @requiresScopes(scopes: [[String!]!]!) on FIELD_DEFINITION | OBJECT
       |directive @policy(policies: [[String!]!]!) on FIELD_DEFINITION | OBJECT
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

  private val namedPolicySchema =
    s"""
       |extend schema @link(
       |  url: "https://specs.apollo.dev/federation/v2.9"
       |  import: ["@policy", "@requiresScopes"]
       |)
       |$linkDefinitions
       |directive @policy(policies: [[String!]!]!) on FIELD_DEFINITION | OBJECT
       |directive @requiresScopes(scopes: [[String!]!]!) on FIELD_DEFINITION | OBJECT
       |type Query {
       |  open: String
       |  named: String @policy(policies: [["owner", "region"], ["admin"]])
       |  extra: String @policy(policies: [["extra"]])
       |  scoped: String @requiresScopes(scopes: [["read"]]) @policy(policies: [["owner"]])
       |  empty: String @policy(policies: [])
       |  emptyAlternative: String @policy(policies: [[]])
       |}
       |""".stripMargin

  private final case class Claims(scope: String, policies: Set[String] = Set.empty)

  private trait RequestClaims {
    def current: Task[Option[Claims]]
  }

  private trait NamedPolicies {
    def allow(claims: Claims, name: String): Task[Boolean]
  }

  private def claimScopes(claims: Claims): Set[String] =
    claims.scope.split(" ").filter(_.nonEmpty).toSet

  def spec = suite("SecurityPolicySpec")(
    test("uses the same authentication and scope semantics with and without a named-policy handler") {
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
        remote   <-
          stub(
            """{"data":{"open":"ok","login":"ok","scoped":"ok","other":"ok","empty":"ok","emptyAlternative":"ok"}}"""
          )
        claims   <- FiberRef.make(Option.empty[Claims])
        policies  = List(
                      OperationPolicy.fromClaims(claims.get)(claimScopes),
                      OperationPolicy.fromClaims(
                        claims.get,
                        (_: Claims, _: String) => ZIO.dieMessage("unused-handler")
                      )(claimScopes)
                    )
        runtimes <- ZIO.foreach(policies) { policy =>
                      Gateway
                        .compose(Subgraph.federation("claims", remote.endpoint, claimsSchema))
                        .withOperationPolicy(policy)
                        .interpreter
                    }
        results  <- ZIO.foreach(cases) { case (query, scope, allowed) =>
                      ZIO
                        .foreach(runtimes) { runtime =>
                          claims.locally(scope.map(Claims(_)))(runtime.execute(query)).map { result =>
                            assertTrue(
                              result.errors.map(_.msg) ==
                                (if (allowed) Nil else List("Operation rejected by gateway policy."))
                            )
                          }
                        }
                        .map(_.reduce(_ && _))
                    }
        sent     <- remote.requests.get
      } yield results.reduce(_ && _) && assertTrue(sent.size == cases.count(_._3) * policies.size)
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
        status   <- runtime.status
        sent     <- remote.requests.get
      } yield assertTrue(
        allowed.errors.isEmpty,
        rejected.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        reads == 2,
        status.operationCache.hits == 1,
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
      val policies                      = List(
        OperationPolicy.fromClaims(ZIO.some(Claims("")))(scopes),
        OperationPolicy.fromClaims(ZIO.some(Claims("")), (_: Claims, _: String) => ZIO.succeed(false))(scopes)
      )

      for {
        remote  <- stub("""{"data":{"login":"ok"}}""")
        results <- ZIO.foreach(policies) { policy =>
                     for {
                       runtime <- Gateway
                                    .compose(Subgraph.federation("claims", remote.endpoint, claimsSchema))
                                    .withOperationPolicy(policy)
                                    .interpreter
                       result  <- runtime.execute("{ login }")
                     } yield assertTrue(result.errors.map(_.msg) == List("Operation policy failed."))
                   }
        sent    <- remote.requests.get
      } yield results.reduce(_ && _) && assertTrue(sent.isEmpty)
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
        diagnostics.exists(message => message.startsWith("[secure]") && message.contains("@policy")),
        sent.isEmpty
      )
    },
    test("requires a named-policy handler at build time without reading request claims") {
      for {
        remote     <- stub("""{"data":{"node":null}}""")
        calls      <- Ref.make(0)
        policy      = OperationPolicy.fromClaims(calls.update(_ + 1).as(Option.empty[Claims]))(claimScopes)
        exit       <- Gateway
                        .compose(Subgraph.federation("secure", remote.endpoint, securitySchema))
                        .withOperationPolicy(policy)
                        .interpreter
                        .exit
        diagnostics = buildDiagnostics(exit)
        reads      <- calls.get
        sent       <- remote.requests.get
      } yield assertTrue(
        exit.causeOption.flatMap(_.failureOption).exists {
          case GatewayBuildError.InvalidConfiguration(_) => true
          case _                                         => false
        },
        diagnostics.size == 1,
        diagnostics.exists(message =>
          message.contains("[secure]") && message.contains("Query.node") && message.contains("named-policy handler")
        ),
        reads == 0,
        sent.isEmpty
      )
    },
    test("combines named-policy alternatives with scopes and authentication") {
      val cases = List(
        ("{ open }", None, true),
        ("{ named }", None, false),
        ("{ named }", Some(Claims("")), false),
        ("{ named }", Some(Claims("", Set("owner"))), false),
        ("{ named }", Some(Claims("", Set("region"))), false),
        ("{ named }", Some(Claims("", Set("owner", "region"))), true),
        ("{ named }", Some(Claims("", Set("admin"))), true),
        ("{ named extra }", Some(Claims("", Set("admin"))), false),
        ("{ named extra }", Some(Claims("", Set("admin", "extra"))), true),
        ("{ scoped }", Some(Claims("read")), false),
        ("{ scoped }", Some(Claims("", Set("owner"))), false),
        ("{ scoped }", Some(Claims("read", Set("owner"))), true),
        ("{ empty }", None, false),
        ("{ empty }", Some(Claims("")), true),
        ("{ emptyAlternative }", None, false),
        ("{ emptyAlternative }", Some(Claims("")), true)
      )

      for {
        remote  <-
          stub(
            """{"data":{"open":"ok","named":"ok","extra":"ok","scoped":"ok","empty":"ok","emptyAlternative":"ok"}}"""
          )
        claims  <- FiberRef.make(Option.empty[Claims])
        policy   = OperationPolicy.fromClaims(
                     claims.get,
                     (claims: Claims, name: String) => ZIO.succeed(claims.policies.contains(name))
                   )(claimScopes)
        runtime <- Gateway
                     .compose(Subgraph.federation("claims", remote.endpoint, namedPolicySchema))
                     .withOperationPolicy(policy)
                     .interpreter
        results <- ZIO.foreach(cases) { case (query, current, allowed) =>
                     claims.locally(current)(runtime.execute(query)).map { result =>
                       assertTrue(
                         result.errors.map(_.msg) ==
                           (if (allowed) Nil else List("Operation rejected by gateway policy."))
                       )
                     }
                   }
        sent    <- remote.requests.get
      } yield results.reduce(_ && _) && assertTrue(sent.size == cases.count(_._3))
    },
    test("authentication-only policies build without a handler and never invoke a supplied handler") {
      val expressions = List("[]", "[[]]", "[[\"owner\"], []]", "[[], [\"owner\"]]")

      for {
        remote      <- stub("""{"data":{"value":"ok"}}""")
        calls       <- Ref.make(0)
        results     <- ZIO.foreach(expressions) { expression =>
                         val schema =
                           s"""
                          |extend schema @link(
                          |  url: "https://specs.apollo.dev/federation/v2.9"
                          |  import: ["@policy"]
                          |)
                          |$linkDefinitions
                          |directive @policy(policies: [[String!]!]!) on FIELD_DEFINITION
                          |type Query { value: String @policy(policies: $expression) }
                          |""".stripMargin

                         for {
                           claims     <- FiberRef.make(Option.empty[Claims])
                           gateway     = Gateway.compose(Subgraph.federation("claims", remote.endpoint, schema))
                           noPolicy   <- gateway.interpreter.exit
                           without     = OperationPolicy.fromClaims(claims.get)(claimScopes)
                           withHandler = OperationPolicy.fromClaims(
                                           claims.get,
                                           (_: Claims, _: String) => calls.update(_ + 1) *> ZIO.dieMessage("unused-handler")
                                         )(claimScopes)
                           decisions  <- ZIO.foreach(List(without, withHandler)) { policy =>
                                           for {
                                             runtime       <- gateway.withOperationPolicy(policy).interpreter
                                             anonymous     <- runtime.execute("{ value }")
                                             authenticated <- claims.locally(Some(Claims("")))(runtime.execute("{ value }"))
                                           } yield assertTrue(
                                             anonymous.errors.map(_.msg) == List("Operation rejected by gateway policy."),
                                             authenticated.errors.isEmpty
                                           )
                                         }
                         } yield decisions.reduce(_ && _) && assertTrue(
                           noPolicy.causeOption.flatMap(_.failureOption).exists {
                             case GatewayBuildError.OperationPolicyRequired(_) => true
                             case _                                            => false
                           }
                         )
                       }
        invocations <- calls.get
        sent        <- remote.requests.get
      } yield results.reduce(_ && _) && assertTrue(invocations == 0, sent.size == expressions.size * 2)
    },
    test("short-circuits named policies using fresh claims and handler environments on cache hits") {
      val query = "{ named }"

      for {
        remote       <- stub("""{"data":{"open":"ok","named":"ok"}}""")
        reads        <- Ref.make(0)
        calls        <- Ref.make(Vector.empty[String])
        claims       <- FiberRef.make(Option.empty[Claims])
        claimService  = new RequestClaims {
                          def current: Task[Option[Claims]] = reads.update(_ + 1) *> claims.get
                        }
        namedService  = new NamedPolicies {
                          def allow(claims: Claims, name: String): Task[Boolean] =
                            calls.update(_ :+ name).as(claims.policies.contains(name))
                        }
        policy        = OperationPolicy.fromClaims(
                          ZIO.serviceWithZIO[RequestClaims](_.current),
                          (claims: Claims, name: String) => ZIO.serviceWithZIO[NamedPolicies](_.allow(claims, name))
                        )(claimScopes)
        runtime      <- Gateway
                          .compose(Subgraph.federation("claims", remote.endpoint, namedPolicySchema))
                          .withOperationPolicy(policy)
                          .interpreter
        layer         = ZLayer.succeed(claimService) ++ ZLayer.succeed(namedService)
        publicResult <- runtime.execute("{ open }").provideLayer(layer)
        anonymous    <- runtime.execute(query).provideLayer(layer)
        skippedCalls <- calls.get
        allowed      <- claims
                          .locally(Some(Claims("", Set("owner", "region"))))(runtime.execute(query))
                          .provideLayer(layer)
        rejected     <- claims.locally(Some(Claims("")))(runtime.execute(query)).provideLayer(layer)
        observed     <- calls.get
        readCount    <- reads.get
        status       <- runtime.status
        sent         <- remote.requests.get
      } yield assertTrue(
        publicResult.errors.isEmpty,
        anonymous.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        skippedCalls.isEmpty,
        allowed.errors.isEmpty,
        rejected.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        observed == Vector("owner", "region", "owner", "admin"),
        readCount == 3,
        status.operationCache.hits == 2,
        sent.size == 2
      )
    },
    test("masks named-policy failures and defects without trying an allowing alternative") {
      val failures = List(
        ZIO.fail(new RuntimeException("policy-handler-secret")),
        ZIO.dieMessage("policy-handler-secret")
      )

      for {
        remote   <- stub("""{"data":{"named":"ok"}}""")
        calls    <- Ref.make(Vector.empty[String])
        results  <- ZIO.foreach(failures) { failure =>
                      val policy = OperationPolicy.fromClaims(
                        ZIO.some(Claims("")),
                        (_: Claims, name: String) =>
                          calls.update(_ :+ name) *> (if (name == "owner") failure else ZIO.succeed(true))
                      )(claimScopes)
                      for {
                        runtime <- Gateway
                                     .compose(Subgraph.federation("claims", remote.endpoint, namedPolicySchema))
                                     .withOperationPolicy(policy)
                                     .interpreter
                        result  <- runtime.execute("{ named }")
                      } yield assertTrue(result.errors.map(_.msg) == List("Operation policy failed."))
                    }
        observed <- calls.get
        sent     <- remote.requests.get
      } yield results.reduce(_ && _) && assertTrue(observed == Vector("owner", "owner"), sent.isEmpty)
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
           |  @link(url: "https://specs.apollo.dev/policy/v0.1")
           |$linkDefinitions
           |directive @login on FIELD_DEFINITION | OBJECT
           |directive @guarded(scopes: [[String!]!]!) on FIELD_DEFINITION | OBJECT
           |directive @policy(policies: [[String!]!]!) on FIELD_DEFINITION | OBJECT
           |type Query @login {
           |  value: String @guarded(scopes: [["read:value"]]) @policy(policies: [["owner"]])
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
        diagnostics.exists(message => message.startsWith("[federation-one]") && message.contains("@policy")),
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
          SecurityRequirement(
            List("value"),
            "Query",
            Some("value"),
            Nil,
            List(Authenticated, RequiresScopes(List(List("read:value"))))
          )
        ),
        alphaSent.isEmpty,
        betaSent.isEmpty
      )
    },
    test("exposes effective security requirements before cached operations reach a source") {
      for {
        remote          <- stub("""{"data":{"node":null}}""")
        observed        <- Ref.make(Vector.empty[List[SecurityRequirement]])
        policy           = OperationPolicy[Any] { operation =>
                             observed.update(_ :+ operation.securityRequirements).as(Reject())
                           }
        runtime         <- Gateway
                             .compose(Subgraph.federation("secure", remote.endpoint, securitySchema))
                             .withOperationPolicy(policy)
                             .interpreter
        included        <- runtime.executeRequest(request)
        skipped         <- runtime.executeRequest(
                             request.copy(variables = Some(Map("show" -> BooleanValue(false))))
                           )
        introspection   <- runtime.execute("{ __schema { queryType { name } } }")
        seen            <- observed.get
        status          <- runtime.status
        sent            <- remote.requests.get
        first            = seen.headOption.getOrElse(Nil)
        second           = seen.drop(1).headOption.getOrElse(Nil)
        privateCondition = List(RuntimeTypeCondition(List("result"), Set("Private")))
      } yield assertTrue(
        included.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        skipped.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        introspection.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        first.contains(
          SecurityRequirement(List("result"), "Query", None, Nil, List(Authenticated))
        ),
        first.contains(
          SecurityRequirement(
            List("result"),
            "Query",
            Some("node"),
            Nil,
            List(Policy(List(List("owner", "region"), List("admin"))))
          )
        ),
        first.contains(
          SecurityRequirement(
            List("result"),
            "Private",
            None,
            privateCondition,
            List(RequiresScopes(List(List("read:private", "tenant:a"), List("admin"))))
          )
        ),
        first.contains(
          SecurityRequirement(
            List("result", "hidden"),
            "Private",
            Some("secret"),
            privateCondition,
            List(RequiresScopes(List(List("read:secret"))))
          )
        ),
        !second.exists(_.responsePath == List("result", "hidden")),
        seen.drop(2).headOption.exists(_.isEmpty),
        status.operationCache.hits == 1,
        seen.size == 3,
        sent.isEmpty
      )
    },
    test("retains ancestor runtime conditions on nested protected selections") {
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
          SecurityRequirement(
            List("node", "child", "secret"),
            "Child",
            Some("secret"),
            List(RuntimeTypeCondition(List("node"), Set("Private"))),
            List(Authenticated)
          )
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
          SecurityRequirement(
            List("node", "value"),
            "ChildNode",
            Some("value"),
            List(RuntimeTypeCondition(List("node"), Set("Child"))),
            List(Authenticated)
          )
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
    test("treats scopes and policies as authenticated transitive requirements") {
      val schema =
        s"""
           |extend schema @link(
           |  url: "https://specs.apollo.dev/federation/v2.9"
           |  import: ["@authenticated", "@requiresScopes", "@policy", "@requires"]
           |)
           |$linkDefinitions
           |directive @authenticated on FIELD_DEFINITION | OBJECT
           |directive @requiresScopes(scopes: [[fed__Scope!]!]!) on FIELD_DEFINITION | OBJECT
           |directive @policy(policies: [[fed__Policy!]!]!) on FIELD_DEFINITION | OBJECT
           |directive @requires(fields: String!) on FIELD_DEFINITION
           |type Query { product: Product }
           |type Product {
           |  secret: String @authenticated
           |  scopedShipping: String
           |    @requires(fields: "secret")
           |    @requiresScopes(scopes: [["read:shipping"]])
           |  governedShipping: String
           |    @requires(fields: "secret")
           |    @policy(policies: [["shipping-policy"]])
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
    test("rejects progressive overrides and contexts before source execution") {
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
        diagnostics.exists(message => message.startsWith("[unsupported]") && message.contains("@override(label:)")),
        diagnostics.exists(message => message.startsWith("[unsupported]") && message.contains("@context")),
        diagnostics.exists(message => message.startsWith("[unsupported]") && message.contains("@fromContext")),
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

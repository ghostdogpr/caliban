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

  def spec = suite("SecurityPolicySpec")(
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
    test("retains security applications from every composed field contribution") {
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

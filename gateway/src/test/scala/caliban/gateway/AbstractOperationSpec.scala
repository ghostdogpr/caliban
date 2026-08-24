package caliban.gateway

import caliban.{ CalibanError, GraphQLRequest, PathValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import zio.{ Scope, ZIO }
import zio.test._

object AbstractOperationSpec extends ZIOSpecDefault {

  private val primaryUnionSchema =
    s"""
       |${federationSchemaPreamble("@shareable")}
       |type Query { response: Response }
       |type Response @shareable { actions: [Action!]! message: String }
       |union Action = Alpha | Beta | Gamma
       |type Alpha @shareable { id: ID! value: String }
       |type Beta { id: ID! name: String details: String }
       |type Gamma { id: ID! label: String }
       |""".stripMargin

  private val partialUnionSchema =
    s"""
       |${federationSchemaPreamble("@shareable")}
       |type Response @shareable { actions: [Action!]! message: String }
       |union Action = Alpha
       |type Alpha @shareable { id: ID! value: String }
       |""".stripMargin

  private val usersSchema =
    s"""
       |${federationSchemaPreamble("@key")}
       |interface User @key(fields: "id") { id: ID! }
       |type Admin implements User @key(fields: "id") { id: ID! isMain: Boolean! }
       |""".stripMargin

  private val profilesSchema =
    s"""
       |${federationSchemaPreamble("@key", "@interfaceObject")}
       |type Query { users: [User!]! }
       |type User @key(fields: "id") @interfaceObject { id: ID! name: String! }
       |""".stripMargin

  private val usersExecutableSchema =
    s"""
       |$usersSchema
       |scalar _Any
       |union _Entity = Admin
       |type Query { _entities(representations: [_Any!]!): [_Entity]! }
       |""".stripMargin

  private val interfaceOwnerSchema =
    s"""
       |${federationSchemaPreamble("@key")}
       |type Query { users: [NodeWithName!]! }
       |interface NodeWithName @key(fields: "id") { id: ID! name: String }
       |type User implements NodeWithName @key(fields: "id") { id: ID! name: String age: Int }
       |""".stripMargin

  private val interfaceObjectSchema =
    s"""
       |${federationSchemaPreamble("@key", "@interfaceObject")}
       |type Query { anotherUsers: [NodeWithName] }
       |type NodeWithName @key(fields: "id") @interfaceObject { id: ID! username: String }
       |""".stripMargin

  private val partialNestedA =
    s"""
       |${federationSchemaPreamble("@key", "@shareable")}
       |type Query { rootA: Container }
       |type Container @key(fields: "id") { id: ID! wrapper: Wrapper @shareable }
       |type Wrapper @shareable { actions: [Action!]! @shareable }
       |union Action = Common | OnlyA
       |type Common @shareable { label: String }
       |type OnlyA { a: String }
       |""".stripMargin

  private val partialNestedB =
    s"""
       |${federationSchemaPreamble("@key", "@shareable")}
       |type Container @key(fields: "id") { id: ID! wrapper: Wrapper @shareable }
       |type Wrapper @shareable { actions: [Action!]! @shareable }
       |union Action = Common | OnlyB
       |type Common @shareable { label: String }
       |type OnlyB { b: String }
       |""".stripMargin

  private val mismatchedUserSchema =
    s"""
       |${federationSchemaPreamble("@shareable")}
       |type User { id: ID @shareable }
       |type Query { users: [User!]! }
       |""".stripMargin

  private val accountSchema =
    s"""
       |${federationSchemaPreamble("@shareable", "@key")}
       |union Account = User | Admin
       |type User @key(fields: "id") { id: ID! name: String similarAccounts: [Account!]! }
       |type Admin { id: ID name: String @shareable similarAccounts: [Account!]! }
       |type Query { accounts: [Account!]! }
       |""".stripMargin

  private val abstractLookupSourceSchema =
    """
      |interface Actor { id: ID! }
      |type User implements Actor { id: ID! }
      |type Admin implements Actor { id: ID! }
      |type Query { actors: [Actor!]! }
      |""".stripMargin

  private val abstractLookupTargetSchema =
    """
      |type User { id: ID! detail: String }
      |type Query { usersByIds(ids: [ID!]!): [User!]! }
      |""".stripMargin

  private val abstractLookup = Lookup.list(
    "User",
    List("id"),
    "usersByIds",
    Lookup.Correlation.ordered,
    "ids" -> Lookup.Argument.batch(Lookup.Argument.key("id"))
  )

  private val nullableAbstractSchema =
    """
      |type Query { outcome: Outcome otherOutcome: Outcome }
      |union Outcome = TextResult | ObjectResult
      |type TextResult { text: String }
      |type ObjectResult { value: String }
      |""".stripMargin

  private val unrelatedSchema = "type Query { other: String }"

  private val aliasedInterfaceSchema =
    """
      |interface Named { label: String }
      |type Concrete implements Named { label: String }
      |type Query { outcome: Named }
      |""".stripMargin

  def spec = suite("AbstractOperationSpec")(
    suite("abstract selections")(
      test("preserves mutually exclusive union fragments on their capable root source") {
        val response =
          """{"data":{"response":{"message":"ok","actions":[{"__typename":"Alpha","id":"a","value":"alpha"},{"__typename":"Beta","id":"b","name":"beta","details":"details"},{"__typename":"Gamma","id":"g","label":"gamma"}]}}}"""
        val query    =
          """query { response { message actions { __typename ... on Alpha { id value } ... on Beta { id name details } ... on Gamma { id label } } } }"""

        for {
          primary   <- stub(response)
          secondary <- stub("""{"data":{}}""")
          gateway   <- Gateway
                         .compose(
                           Subgraph.federation("primary", primary.endpoint, primaryUnionSchema),
                           Subgraph.federation("secondary", secondary.endpoint, partialUnionSchema)
                         )
                         .build
          result    <- gateway.execute(query)
          requests  <- primary.requests.get
          valid     <- ZIO.foreach(requests)(validateRequest(primaryUnionSchema, _).exit)
          actions    = listValues(field(result.data, "response").flatMap(field(_, "actions")))
        } yield assertTrue(
          result.errors.isEmpty,
          valid.forall(_.isSuccess),
          actions.lift(0).flatMap(field(_, "value")).contains(StringValue("alpha")),
          actions.lift(1).flatMap(field(_, "details")).contains(StringValue("details")),
          actions.lift(2).flatMap(field(_, "label")).contains(StringValue("gamma")),
          actions.forall(_.isInstanceOf[ObjectValue])
        )
      },
      test("resolves interface-object selections through their concrete runtime type") {
        val query =
          """query Users($includeMain: Boolean!) { users { name __typename ...AdminFields @include(if: $includeMain) } } fragment AdminFields on Admin { main: isMain }"""

        for {
          users      <- stubByRequest(request => interfaceEntityResponse(request))
          profiles   <-
            stub(
              """{"data":{"users":[{"name":"Ada","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"}]}}"""
            )
          gateway    <- Gateway
                          .compose(
                            Subgraph.federation("users", users.endpoint, usersSchema),
                            Subgraph.federation("profiles", profiles.endpoint, profilesSchema)
                          )
                          .build
          result     <- gateway.executeRequest(
                          GraphQLRequest(
                            query = Some(query),
                            variables = Some(Map("includeMain" -> BooleanValue(true)))
                          )
                        )
          hidden     <- gateway.execute("{ users { name ... on Admin { main: isMain } } }")
          skipped    <- gateway.executeRequest(
                          GraphQLRequest(
                            query = Some(
                              """query Users($includeMain: Boolean!) { users { name ...AdminFields @include(if: $includeMain) } } fragment AdminFields on Admin { main: isMain }"""
                            ),
                            variables = Some(Map("includeMain" -> BooleanValue(false)))
                          )
                        )
          calls      <- users.requests.get
          valid      <- ZIO.foreach(calls)(validateRequest(usersExecutableSchema, _).exit)
          user        = field(result.data, "users").collect { case ListValue(value :: Nil) => value }
          hiddenUser  = field(hidden.data, "users").collect { case ListValue(value :: Nil) => value }
          skippedUser = field(skipped.data, "users").collect { case ListValue(value :: Nil) => value }
        } yield assertTrue(
          result.errors.isEmpty,
          hidden.errors.isEmpty,
          skipped.errors.isEmpty,
          user.flatMap(field(_, "name")).contains(StringValue("Ada")),
          user.flatMap(field(_, "__typename")).contains(StringValue("Admin")),
          user.flatMap(field(_, "main")).contains(BooleanValue(true)),
          hiddenUser.flatMap(field(_, "main")).contains(BooleanValue(true)),
          hiddenUser.flatMap(field(_, "__typename")).isEmpty,
          skippedUser.flatMap(field(_, "name")).contains(StringValue("Ada")),
          skippedUser.flatMap(field(_, "main")).isEmpty,
          calls.size == 2,
          valid.forall(_.isSuccess)
        )
      },
      test("plans mixed concrete and interface-object fields") {
        val query        =
          """{ anotherUsers { ... on User { age id name username } id name } }"""
        val reverseQuery =
          """{ users { ... on User { age id name username } id name } }"""

        for {
          owner          <-
            stubByRequest { request =>
              if (request.query.exists(_.contains("_entities")))
                """{"data":{"_entities":[{"__typename":"User","age":11,"id":"u1","name":"Ada","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User","_caliban_gateway_requirement_name":"Ada","_caliban_gateway_runtime_typename":"User"}]}}"""
              else
                """{"data":{"users":[{"__typename":"User","age":11,"id":"u1","name":"Ada","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User","_caliban_gateway_runtime_typename":"User"}]}}"""
            }
          interface      <-
            stubByRequest { request =>
              if (request.query.exists(_.contains("_entities")))
                """{"data":{"_entities":[{"__typename":"NodeWithName","id":"u1","name":"Ada","username":"ada","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User","_caliban_gateway_requirement_name":"Ada"}]}}"""
              else
                """{"data":{"anotherUsers":[{"__typename":"User","id":"u1","username":"ada","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User","_caliban_gateway_runtime_typename":"User"}]}}"""
            }
          gateway        <- Gateway
                              .compose(
                                Subgraph.federation("owner", owner.endpoint, interfaceOwnerSchema),
                                Subgraph.federation("interface", interface.endpoint, interfaceObjectSchema)
                              )
                              .build
          plan           <- gateway.explain(query).exit
          reverse        <- gateway.explain(reverseQuery).exit
          forwardResult  <- gateway.execute(query)
          result         <- gateway.execute(reverseQuery)
          calls          <- interface.requests.get
          user            = field(result.data, "users").collect { case ListValue(value :: Nil) => value }
          representations = calls.flatMap(_.variables).flatMap(_.get("representations"))
        } yield assertTrue(
          plan.isSuccess,
          reverse.isSuccess,
          forwardResult.errors.isEmpty,
          result.errors.isEmpty,
          field(forwardResult.data, "anotherUsers").exists {
            case ListValue(value :: Nil) => field(value, "username").contains(StringValue("ada"))
            case _                       => false
          },
          user.flatMap(field(_, "username")).contains(StringValue("ada")),
          representations.exists(_.toString.contains("NodeWithName"))
        )
      },
      test("renders nested partial-union selections for the owning source") {
        val query =
          """{ rootA { wrapper { actions { __typename ... on Common { label } ... on OnlyA { a } ... on OnlyB { b } } } } }"""

        for {
          a       <-
            stub(
              """{"data":{"rootA":{"wrapper":{"actions":[{"__typename":"Common","_caliban_gateway_runtime_typename":"Common","label":"common"},{"__typename":"OnlyA","_caliban_gateway_runtime_typename":"OnlyA","a":null}]}}}}"""
            )
          b       <- stub("""{"data":{}}""")
          gateway <- Gateway
                       .compose(
                         Subgraph.federation("a", a.endpoint, partialNestedA),
                         Subgraph.federation("b", b.endpoint, partialNestedB)
                       )
                       .build
          result  <- gateway.execute(query)
          calls   <- a.requests.get
          valid   <- ZIO.foreach(calls)(validateRequest(partialNestedA, _).exit)
          queries  = calls.flatMap(_.query)
        } yield assertTrue(
          result.errors.isEmpty,
          valid.forall(_.isSuccess),
          queries.forall(!_.contains("OnlyA")),
          field(result.data, "rootA").flatMap(field(_, "wrapper")).flatMap(field(_, "actions")).exists {
            case ListValue(values) => values.forall(field(_, "__typename").exists(_ != caliban.Value.NullValue))
            case _                 => false
          }
        )
      },
      test("plans roots whose object nullability differs from an entity source") {
        val query =
          """{ users { id name } accounts { ... on User { id name similarAccounts { ... on User { id name } ... on Admin { id name } } } ... on Admin { id name similarAccounts { ... on User { id name } ... on Admin { id name } } } } }"""

        for {
          users        <-
            stub(
              """{"data":{"users":[{"id":"u1","_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"}]}}"""
            )
          accounts     <-
            stubByRequest { request =>
              if (request.query.exists(_.contains("_entities")))
                """{"data":{"_entities":[{"__typename":"User","name":"u1-name"}]}}"""
              else
                """{"data":{"accounts":[{"_caliban_gateway_runtime_typename":"User","_caliban_gateway_id":"u1","name":"u1-name","similarAccounts":[{"_caliban_gateway_runtime_typename":"User","_caliban_gateway_id":"u1","name":"u1-name"},{"_caliban_gateway_runtime_typename":"Admin","_caliban_gateway_id_1":"a1","name":"a1-name"}]},{"_caliban_gateway_runtime_typename":"Admin","_caliban_gateway_id_1":"a1","name":"a1-name","similarAccounts":[{"_caliban_gateway_runtime_typename":"User","_caliban_gateway_id":"u1","name":"u1-name"},{"_caliban_gateway_runtime_typename":"Admin","_caliban_gateway_id_1":"a1","name":"a1-name"}]}]}}"""
            }
          gateway      <- Gateway
                            .compose(
                              Subgraph.federation("a", users.endpoint, mismatchedUserSchema),
                              Subgraph.federation("b", accounts.endpoint, accountSchema)
                            )
                            .build
          plan         <- gateway.explain(query).exit
          result       <- gateway.execute(query)
          accountCalls <- accounts.requests.get
          valid        <- ZIO.foreach(accountCalls.filterNot(_.query.exists(_.contains("_entities"))))(
                            validateRequest(accountSchema, _).exit
                          )
        } yield assertTrue(
          plan.isSuccess,
          result.errors.isEmpty,
          valid.forall(_.isSuccess),
          field(result.data, "users").exists {
            case ListValue(value :: Nil) => field(value, "name").contains(StringValue("u1-name"))
            case _                       => false
          },
          field(result.data, "accounts").exists {
            case ListValue(values) =>
              values.forall(value =>
                field(value, "name").nonEmpty && field(value, "similarAccounts").exists {
                  case ListValue(nested) => nested.forall(field(_, "name").nonEmpty)
                  case _                 => false
                }
              )
            case _                 => false
          }
        )
      },
      test("gates an ordinary concrete lookup by the abstract runtime type") {
        val sourceResponse =
          """{"data":{"actors":[{"_caliban_gateway_key":"u1","_caliban_gateway_typename":"User","_caliban_gateway_runtime_typename":"User"},{"_caliban_gateway_typename":"Admin","_caliban_gateway_runtime_typename":"Admin"}]}}"""

        for {
          source   <- stub(sourceResponse)
          target   <- stub("""{"data":{"_caliban_gateway_lookup":[{"detail":"user"}]}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.graphql("source", source.endpoint, abstractLookupSourceSchema),
                          Subgraph
                            .graphql("target", target.endpoint, abstractLookupTargetSchema)
                            .withLookup(abstractLookup)
                        )
                        .build
          result   <- gateway.execute("{ actors { __typename ... on User { detail } } }")
          requests <- target.requests.get
          actors    = listValues(field(result.data, "actors"))
        } yield assertTrue(
          result.errors.isEmpty,
          requests.size == 1,
          requests.headOption.flatMap(_.query).exists(_.contains("usersByIds(ids:[\"u1\"])")),
          actors.lift(0).flatMap(field(_, "detail")).contains(StringValue("user")),
          actors.lift(1).flatMap(field(_, "detail")).isEmpty
        )
      },
      test("restores private response aliases in root error paths") {
        val response =
          """{"data":{"accounts":[{"_caliban_gateway_runtime_typename":"User","_caliban_gateway_id":null}]},"errors":[{"message":"failed","path":["accounts",0,"_caliban_gateway_id"]}]}"""

        for {
          users    <- stub("""{"data":{}}""")
          accounts <- stub(response)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("users", users.endpoint, mismatchedUserSchema),
                          Subgraph.federation("accounts", accounts.endpoint, accountSchema)
                        )
                        .build
          result   <- gateway.execute("{ accounts { ... on User { id } ... on Admin { id } } }")
        } yield assertTrue(
          executionErrors(result.errors).map(_.path) ==
            List(List(PathValue.Key("accounts"), PathValue.Index(0), PathValue.Key("id")))
        )
      },
      test("restores private response aliases in entity error paths") {
        val entityResponse =
          """{"data":{"_entities":[{"similarAccounts":[{"_caliban_gateway_runtime_typename":"User","_caliban_gateway_id":null}]}]},"errors":[{"message":"failed","path":["_entities",0,"similarAccounts",0,"_caliban_gateway_id"]}]}"""

        for {
          users    <- stub(
                        """{"data":{"users":[{"_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"}]}}"""
                      )
          accounts <- stub(entityResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("users", users.endpoint, mismatchedUserSchema),
                          Subgraph.federation("accounts", accounts.endpoint, accountSchema)
                        )
                        .build
          result   <- gateway.execute(
                        "{ users { similarAccounts { ... on User { id } ... on Admin { id } } } }"
                      )
        } yield assertTrue(
          executionErrors(result.errors).map(_.path) ==
            List(
              List(
                PathValue.Key("users"),
                PathValue.Index(0),
                PathValue.Key("similarAccounts"),
                PathValue.Index(0),
                PathValue.Key("id")
              )
            )
        )
      },
      test("retains concrete fragments when single-source execution needs runtime evidence") {
        val response =
          """{"data":{"response":{"actions":[{"_caliban_gateway_runtime_typename":"Alpha","value":"alpha"}]}}}"""

        for {
          source   <- stub(response)
          gateway  <- Gateway.compose(Subgraph.graphql("source", source.endpoint, primaryUnionSchema)).build
          result   <- gateway.execute("{ response { actions { ... on Alpha { value } } } }")
          requests <- source.requests.get
          action    = field(result.data, "response")
                        .flatMap(field(_, "actions"))
                        .collect { case ListValue(value :: Nil) => value }
        } yield assertTrue(
          result.errors.isEmpty,
          action.flatMap(field(_, "value")).contains(StringValue("alpha")),
          requests.headOption.flatMap(_.query).exists(_.contains("_caliban_gateway_runtime_typename:__typename"))
        )
      },
      test("uses an aliased typename as single-source runtime evidence") {
        for {
          source  <- stub("""{"data":{"outcome":{"kind":"TextResult"}}}""")
          gateway <- Gateway.compose(Subgraph.graphql("source", source.endpoint, nullableAbstractSchema)).build
          result  <- gateway.execute("{ outcome { kind: __typename } }")
        } yield assertTrue(
          result.errors.isEmpty,
          field(result.data, "outcome").flatMap(field(_, "kind")).contains(StringValue("TextResult"))
        )
      },
      test("prefers an aliased typename over runtime evidence from another path") {
        val response =
          """{"data":{"first":{"_caliban_gateway_runtime_typename":"TextResult","text":"first"},"other":{"_caliban_gateway_runtime_typename":"not-a-type","kind":"TextResult"}}}"""

        for {
          source  <- stub(response)
          gateway <- Gateway.compose(Subgraph.graphql("source", source.endpoint, nullableAbstractSchema)).build
          result  <-
            gateway.execute(
              "{ first: outcome { ... on TextResult { text } } other: otherOutcome { ... on TextResult { _caliban_gateway_runtime_typename: text } kind: __typename } }"
            )
        } yield assertTrue(
          result.errors.isEmpty,
          field(result.data, "other").flatMap(field(_, "kind")).contains(StringValue("TextResult")),
          field(result.data, "other")
            .flatMap(field(_, "_caliban_gateway_runtime_typename"))
            .contains(StringValue("not-a-type"))
        )
      },
      test("prefers a typed typename alias over an ordinary field named typename") {
        for {
          source  <- stub("""{"data":{"outcome":{"__typename":"ordinary label","kind":"Concrete"}}}""")
          gateway <- Gateway.compose(Subgraph.graphql("source", source.endpoint, aliasedInterfaceSchema)).build
          result  <- gateway.execute("{ outcome { __typename: label kind: __typename } }")
        } yield assertTrue(
          result.errors.isEmpty,
          field(result.data, "outcome")
            .flatMap(field(_, "__typename"))
            .contains(StringValue("ordinary label")),
          field(result.data, "outcome").flatMap(field(_, "kind")).contains(StringValue("Concrete"))
        )
      },
      test("rejects a root abstract value without valid runtime evidence") {
        for {
          source  <- stub("""{"data":{"outcome":{"text":"value"}}}""")
          other   <- stub("""{"data":{}}""")
          gateway <- Gateway
                       .compose(
                         Subgraph.graphql("source", source.endpoint, nullableAbstractSchema),
                         Subgraph.graphql("other", other.endpoint, unrelatedSchema)
                       )
                       .build
          result  <- gateway.execute("{ outcome { ... on TextResult { text } } }")
          errors   = executionErrors(result.errors)
        } yield assertTrue(
          field(result.data, "outcome").contains(caliban.Value.NullValue),
          errors.map(_.path) == List(List(PathValue.Key("outcome"))),
          errors.forall(_.msg == "Remote GraphQL request failed.")
        )
      },
      test("rejects an entity abstract value with invalid runtime evidence") {
        val entityResponse =
          """{"data":{"_entities":[{"similarAccounts":[{"_caliban_gateway_runtime_typename":"Unknown","_caliban_gateway_id":"u2"}]}]}}"""

        for {
          users    <- stub(
                        """{"data":{"users":[{"_caliban_gateway_key":"u1","_caliban_gateway_typename":"User"}]}}"""
                      )
          accounts <- stub(entityResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("users", users.endpoint, mismatchedUserSchema),
                          Subgraph.federation("accounts", accounts.endpoint, accountSchema)
                        )
                        .build
          result   <- gateway.execute(
                        "{ users { similarAccounts { ... on User { id } ... on Admin { id } } } }"
                      )
          errors    = executionErrors(result.errors)
        } yield assertTrue(
          errors.exists(
            _.path == List(
              PathValue.Key("users"),
              PathValue.Index(0),
              PathValue.Key("similarAccounts"),
              PathValue.Index(0)
            )
          ),
          errors.exists(_.msg == "Remote GraphQL request failed.")
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential

  private def interfaceEntityResponse(request: GraphQLRequest): String =
    request.query match {
      case Some(query) if query.contains("_entities") =>
        """{"data":{"_entities":[{"__typename":"Admin","_caliban_gateway_runtime_typename":"Admin","main":true}]}}"""
      case _                                          => """{"data":{}}"""
    }
}

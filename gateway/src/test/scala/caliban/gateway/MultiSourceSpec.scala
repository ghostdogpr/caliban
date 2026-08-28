package caliban.gateway

import caliban.ResponseValue.{ ListValue => ResponseListValue }
import caliban.Value.IntValue.IntNumber
import caliban.Value.{ NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.{ CalibanError, GraphQLRequest, PathValue }
import sttp.model.Uri
import zio._
import zio.test._

object MultiSourceSpec extends ZIOSpecDefault {

  def spec = suite("MultiSourceSpec")(
    suite("root execution")(
      test("composes and concurrently executes query roots from two remote graphs") {
        val productsSchema =
          """
            |type Query { product(id: ID!): Product }
            |type Mutation { updateProduct(id: ID!): Boolean! }
            |type Product { id: ID! name: String! }
            |""".stripMargin
        val reviewsSchema  =
          """
            |type Query { reviews(limit: Int!): [Review!]! }
            |type Mutation { addReview(text: String!): Boolean! }
            |type Review { body: String! }
            |""".stripMargin
        val query          =
          """
            |query Dashboard($id: ID!, $limit: Int!) {
            |  recent: reviews(limit: $limit) { body }
            |  featured: product(id: $id) { id name }
            |}
            |""".stripMargin

        for {
          started         <- Ref.make(0)
          release         <- Promise.make[Nothing, Unit]
          beforeResponse   = started.updateAndGet(_ + 1).flatMap {
                               case 2 => release.succeed(()).unit
                               case _ => release.await
                             }
          products        <- stubWith(beforeResponse, """{"data":{"featured":{"id":"p1","name":"Table"}}}""")
          reviews         <-
            stubWith(
              beforeResponse,
              """{"data":{"recent":[{"body":"Solid"}]},"errors":[{"message":"review warning","path":["recent",0,"body"],"locations":[{"line":1,"column":2}]}]}"""
            )
          gateway         <- Gateway
                               .compose(
                                 Subgraph.graphql("products", products.endpoint, productsSchema),
                                 Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                               )
                               .withConfig(_.withRemoteErrorDisclosure(_.withMessages(true)))
                               .interpreter
          request          = GraphQLRequest(
                               query = Some(query),
                               operationName = Some("Dashboard"),
                               variables = Some(Map("id" -> StringValue("p1"), "limit" -> IntNumber(1))),
                               extensions = Some(Map("client" -> StringValue("gateway-spec")))
                             )
          response        <- gateway.executeRequest(request)
          productRequests <- products.requests.get
          reviewRequests  <- reviews.requests.get
          productValid    <- ZIO.foreach(productRequests)(validateRequest(productsSchema, _).exit)
          reviewValid     <- ZIO.foreach(reviewRequests)(validateRequest(reviewsSchema, _).exit)
          names            = fieldNames(response.data)
        } yield assertTrue(
          names == List("recent", "featured"),
          field(response.data, "featured").flatMap(field(_, "name")).contains(StringValue("Table")),
          onlyNested(Some(response.data), "recent").exists(_.contains("body" -> StringValue("Solid"))),
          response.errors.map(_.msg) == List("review warning"),
          response.errors.collectFirst { case error: CalibanError.ExecutionError => error.path }.contains(
            List(StringValue("recent"), IntNumber(0), StringValue("body"))
          ),
          executionErrors(response.errors).forall(_.locationInfo.isEmpty),
          productRequests.size == 1,
          reviewRequests.size == 1,
          productRequests.head.operationName.contains("Dashboard"),
          reviewRequests.head.operationName.contains("Dashboard"),
          productRequests.head.variables.isEmpty,
          reviewRequests.head.variables.isEmpty,
          productValid.forall(_.isSuccess),
          reviewValid.forall(_.isSuccess),
          productRequests.head.extensions.isEmpty,
          reviewRequests.head.extensions.isEmpty,
          productRequests.head.query.exists(query =>
            query.contains("featured:product(id:\"p1\")") && !query.contains("reviews")
          ),
          reviewRequests.head.query.exists(query =>
            query.contains("recent:reviews(limit:1)") && !query.contains("product")
          )
        )
      } @@ TestAspect.timeout(10.seconds),
      test("preserves independent data when one root source fails") {
        val productsSchema = "type Query { featured: Product } type Product { name: String! }"
        val reviewsSchema  = "type Query { recent: [Review!] } type Review { body: String! }"

        for {
          products <- stub("""{"data":{"featured":{"name":"Table"}}}""")
          reviews  <- stub(invalidResponse)
          gateway  <- Gateway
                        .compose(
                          Subgraph.graphql("products", products.endpoint, productsSchema),
                          Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                        )
                        .interpreter
          response <- gateway.execute("{ featured { name } recent { body } }")
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "featured").flatMap(field(_, "name")).contains(StringValue("Table")),
          field(response.data, "recent").contains(NullValue),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("recent")))
        )
      },
      test("relocates an unusable grouped error only to its visible root") {
        val schema = "type Query { first: Result second: Result } type Result { value: String }"
        val result =
          """{"data":{"first":null,"second":{"value":"ok"}},"errors":[{"message":"internal failure","path":["first","_internal"]}]}"""

        for {
          source   <- stub(result)
          gateway  <- Gateway.compose(Subgraph.graphql("values", source.endpoint, schema)).interpreter
          response <- gateway.execute("{ first { value } second { value } }")
          sent     <- source.requests.get
          errors    = executionErrors(response.errors)
        } yield assertTrue(
          field(response.data, "first").contains(NullValue),
          field(response.data, "second").flatMap(field(_, "value")).contains(StringValue("ok")),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("first"))),
          sent.size == 1
        )
      },
      test("orders root errors independently of source completion order") {
        val firstSchema   = "type Query { first: String }"
        val secondSchema  = "type Query { second: String }"
        val firstFailure  = """{"data":{"first":null},"errors":[{"message":"first failed","path":["first"]}]}"""
        val secondFailure =
          """{"data":{"second":null},"errors":[{"message":"second failed","path":["second"]}]}"""

        for {
          secondStartedA <- Promise.make[Nothing, Unit]
          slowFirstA     <- stubWith(
                              secondStartedA.await *> ZIO.foreachDiscard(1 to 100)(_ => ZIO.yieldNow),
                              firstFailure
                            )
          fastSecondA    <- stubWith(secondStartedA.succeed(()).unit, secondFailure)
          gatewayA       <- Gateway
                              .compose(
                                Subgraph.graphql("first", slowFirstA.endpoint, firstSchema),
                                Subgraph.graphql("second", fastSecondA.endpoint, secondSchema)
                              )
                              .withConfig(_.withRemoteErrorDisclosure(_.withMessages(true)))
                              .interpreter
          responseA      <- gatewayA.execute("{ first second }")
          firstStartedB  <- Promise.make[Nothing, Unit]
          fastFirstB     <- stubWith(firstStartedB.succeed(()).unit, firstFailure)
          slowSecondB    <- stubWith(
                              firstStartedB.await *> ZIO.foreachDiscard(1 to 100)(_ => ZIO.yieldNow),
                              secondFailure
                            )
          gatewayB       <- Gateway
                              .compose(
                                Subgraph.graphql("first", fastFirstB.endpoint, firstSchema),
                                Subgraph.graphql("second", slowSecondB.endpoint, secondSchema)
                              )
                              .withConfig(_.withRemoteErrorDisclosure(_.withMessages(true)))
                              .interpreter
          responseB      <- gatewayB.execute("{ first second }")
        } yield assertTrue(
          responseA.errors.map(_.msg) == List("first failed", "second failed"),
          responseB.errors.map(_.msg) == List("first failed", "second failed"),
          executionErrors(responseA.errors).map(_.path) == List(
            List(PathValue.Key("first")),
            List(PathValue.Key("second"))
          ),
          executionErrors(responseB.errors).map(_.path) == List(
            List(PathValue.Key("first")),
            List(PathValue.Key("second"))
          ),
          responseA.data == responseB.data
        )
      },
      test("executes introspection locally against all composed roots") {
        val productsSchema =
          "type Query { product: String } type Mutation { updateProduct: Boolean }"
        val reviewsSchema  =
          "type Query { reviews: [String!]! } type Mutation { addReview: Boolean }"

        for {
          products     <- stub("""{"data":{"product":"Table"}}""")
          reviews      <- stub("""{"data":{"reviews":["Solid"]}}""")
          gateway      <- Gateway
                            .compose(
                              Subgraph.graphql("products", products.endpoint, productsSchema),
                              Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                            )
                            .interpreter
          response     <- gateway.execute(
                            """{
                          |  __schema {
                          |    queryType { fields { name } }
                          |    mutationType { fields { name } }
                          |  }
                          |}""".stripMargin
                          )
          productSent  <- products.requests.get
          reviewSent   <- reviews.requests.get
          schema        = field(response.data, "__schema")
          queryNames    = schema
                            .flatMap(field(_, "queryType"))
                            .flatMap(field(_, "fields"))
                            .collect { case ResponseListValue(fields) =>
                              fields.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                            }
          mutationNames = schema
                            .flatMap(field(_, "mutationType"))
                            .flatMap(field(_, "fields"))
                            .collect { case ResponseListValue(fields) =>
                              fields.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                            }
        } yield assertTrue(
          response.errors.isEmpty,
          queryNames.exists(_.toSet == Set("product", "reviews")),
          mutationNames.exists(_.toSet == Set("updateProduct", "addReview")),
          productSent.isEmpty,
          reviewSent.isEmpty
        )
      },
      test("advertises executable subscription roots") {
        val schema = "type Query { value: String } type Subscription { changes: String }"

        for {
          source   <- stub("""{"data":{"value":"ok"}}""")
          gateway  <- Gateway.compose(Subgraph.graphql("values", source.endpoint, schema)).interpreter
          response <- gateway.execute("{ __schema { subscriptionType { name } } }")
          sent     <- source.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "__schema")
            .flatMap(field(_, "subscriptionType"))
            .flatMap(field(_, "name"))
            .contains(StringValue("Subscription")),
          sent.isEmpty
        )
      },
      test("mixes local introspection with remote root fields") {
        val productsSchema = "type Query { product: String }"
        val reviewsSchema  = "type Query { reviews: [Review!]! } type Review { body: String! }"

        for {
          products    <- stub("""{"data":{"product":"Table"}}""")
          reviews     <- stub("""{"data":{"reviews":[{"body":"Solid"}]}}""")
          gateway     <- Gateway
                           .compose(
                             Subgraph.graphql("products", products.endpoint, productsSchema),
                             Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                           )
                           .interpreter
          response    <- gateway.execute(
                           """{
                          |  product
                          |  __schema { queryType { fields { name } } }
                          |  __type(name: "Review") { name }
                          |}""".stripMargin
                         )
          productSent <- products.requests.get
          reviewSent  <- reviews.requests.get
          names        = fieldNames(response.data)
          queryNames   = field(response.data, "__schema")
                           .flatMap(field(_, "queryType"))
                           .flatMap(field(_, "fields"))
                           .collect { case ResponseListValue(fields) =>
                             fields.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                           }
        } yield assertTrue(
          response.errors.isEmpty,
          names == List("product", "__schema", "__type"),
          field(response.data, "product").contains(StringValue("Table")),
          queryNames.exists(_.toSet == Set("product", "reviews")),
          field(response.data, "__type").flatMap(field(_, "name")).contains(StringValue("Review")),
          productSent.size == 1,
          reviewSent.isEmpty,
          productSent.head.query.exists(query => query.contains("product") && !query.contains("__schema"))
        )
      }
    ),
    suite("operation restrictions")(
      test("executes mutation roots serially in client order across sources") {
        val productsSchema =
          "type Query { product: String } type Mutation { updateProduct(id: ID!): Boolean! }"
        val reviewsSchema  =
          "type Query { reviews: [String!]! } type Mutation { addReview(text: String!): Boolean! }"

        for {
          firstStarted  <- Promise.make[Nothing, Unit]
          releaseFirst  <- Promise.make[Nothing, Unit]
          secondStarted <- Promise.make[Nothing, Unit]
          products      <- stubWith(
                             firstStarted.succeed(()).unit *> releaseFirst.await,
                             """{"data":{"updated":true}}"""
                           )
          reviews       <- stubWith(secondStarted.succeed(()).unit, """{"data":{"added":true}}""")
          gateway       <- Gateway
                             .compose(
                               Subgraph.graphql("products", products.endpoint, productsSchema),
                               Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                             )
                             .interpreter
          execution     <- gateway
                             .execute(
                               """mutation Changes {
                             |  updated: updateProduct(id: "p1")
                             |  added: addReview(text: "Good")
                             |}""".stripMargin,
                               Some("Changes")
                             )
                             .fork
          first         <- firstStarted.await.as(true).race(execution.await.as(false))
          secondBefore  <- secondStarted.isDone
          _             <- releaseFirst.succeed(()).when(first)
          response      <- execution.join
          secondAfter   <- secondStarted.isDone
          productSent   <- products.requests.get
          reviewSent    <- reviews.requests.get
          names          = fieldNames(response.data)
        } yield assertTrue(
          first,
          !secondBefore,
          secondAfter,
          response.errors.isEmpty,
          names == List("updated", "added"),
          field(response.data, "updated").contains(caliban.Value.BooleanValue(true)),
          field(response.data, "added").contains(caliban.Value.BooleanValue(true)),
          productSent.size == 1,
          reviewSent.size == 1
        )
      },
      test("does not coalesce mutation roots on the same source") {
        val schema =
          "type Query { value: Int! } type Mutation { increment(by: Int!): Int! set(value: Int!): Int! }"

        for {
          backend  <- stub("""{"data":{"incremented":1}}""", """{"data":{"assigned":10}}""")
          gateway  <- Gateway.compose(Subgraph.graphql("counter", backend.endpoint, schema)).interpreter
          response <- gateway.execute(
                        """mutation {
                          |  incremented: increment(by: 1)
                          |  assigned: set(value: 10)
                          |}""".stripMargin
                      )
          requests <- backend.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "incremented").contains(IntNumber(1)),
          field(response.data, "assigned").contains(IntNumber(10)),
          requests.size == 2,
          requests.headOption.flatMap(_.query).exists(query => query.contains("increment") && !query.contains("set")),
          requests
            .drop(1)
            .headOption
            .flatMap(_.query)
            .exists(query => query.contains("set") && !query.contains("increment"))
        )
      },
      test("completes entity work before starting the next mutation root") {
        val productsSchema =
          s"""
             |${federationSchemaPreamble("@key")}
             |type Query { product: Product }
             |type Mutation { updateProduct: Product! }
             |type Product @key(fields: "id") { id: ID! price: Float! }
             |""".stripMargin
        val reviewsSchema  =
          s"""
             |${federationSchemaPreamble("@key", "@external", "@requires")}
             |directive @requires(fields: federation__FieldSet!) on FIELD_DEFINITION
             |type Query { available: Boolean! }
             |type Mutation { addReview: Boolean! }
             |type Product @key(fields: "id") {
             |  id: ID! @external
             |  price: Float! @external
             |  isExpensive: Boolean! @requires(fields: "price")
             |  isAvailable: Boolean!
             |}
             |""".stripMargin

        for {
          products <-
            stub(
              """{"data":{"updated":{"price":599.99,"_caliban_gateway_key":"p1","_caliban_gateway_requirement_price":599.99,"_caliban_gateway_typename":"Product"}}}"""
            )
          reviews  <- stubByRequest(request =>
                        if (request.query.exists(_.contains("_entities")))
                          """{"data":{"_entities":[{"isExpensive":true,"isAvailable":true}]}}"""
                        else """{"data":{"added":true}}"""
                      )
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("reviews", reviews.endpoint, reviewsSchema)
                        )
                        .interpreter
          response <- gateway.execute(
                        """mutation {
                          |  updated: updateProduct { price isExpensive isAvailable }
                          |  added: addReview
                          |}""".stripMargin
                      )
          sent     <- reviews.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "updated")
            .flatMap(field(_, "isExpensive"))
            .contains(caliban.Value.BooleanValue(true)),
          field(response.data, "updated")
            .flatMap(field(_, "isAvailable"))
            .contains(caliban.Value.BooleanValue(true)),
          field(response.data, "added").contains(caliban.Value.BooleanValue(true)),
          sent.size == 2,
          sent.headOption.flatMap(_.query).exists(_.contains("_entities")),
          sent.drop(1).headOption.flatMap(_.query).exists(_.contains("addReview"))
        )
      },
      test("continues with later mutation roots after a field error") {
        val productsSchema =
          "type Query { product: String } type Mutation { updateProduct: Boolean }"
        val reviewsSchema  =
          "type Query { reviews: [String!]! } type Mutation { addReview: Boolean! }"

        for {
          products <- stub(
                        """{"data":{"updated":null},"errors":[{"message":"update failed","path":["updated"]}]}"""
                      )
          reviews  <- stub("""{"data":{"added":true}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.graphql("products", products.endpoint, productsSchema),
                          Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                        )
                        .withConfig(_.withRemoteErrorDisclosure(_.withMessages(true)))
                        .interpreter
          response <- gateway.execute("mutation { updated: updateProduct added: addReview }")
          sent     <- reviews.requests.get
        } yield assertTrue(
          field(response.data, "updated").contains(NullValue),
          field(response.data, "added").contains(caliban.Value.BooleanValue(true)),
          response.errors.map(_.msg) == List("update failed"),
          sent.size == 1
        )
      },
      test("stops after a top-level non-null mutation failure") {
        val productsSchema =
          "type Query { product: String } type Mutation { updateProduct: Boolean! }"
        val reviewsSchema  =
          "type Query { reviews: [String!]! } type Mutation { addReview: Boolean! }"

        for {
          products <- stub(
                        """{"data":null,"errors":[{"message":"update failed","path":["updated"]}]}"""
                      )
          reviews  <- stub("""{"data":{"added":true}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.graphql("products", products.endpoint, productsSchema),
                          Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                        )
                        .withConfig(_.withRemoteErrorDisclosure(_.withMessages(true)))
                        .interpreter
          response <- gateway.execute("mutation { updated: updateProduct added: addReview }")
          sent     <- reviews.requests.get
        } yield assertTrue(
          response.data == NullValue,
          response.errors.map(_.msg) == List("update failed"),
          sent.isEmpty
        )
      },
      test("retains earlier completion errors when a later mutation root aborts") {
        val productsSchema =
          "enum Status { READY } type Query { product: String } type Mutation { status: Status fail: Boolean! }"
        val reviewsSchema  =
          "type Query { reviews: [String!]! } type Mutation { addReview: Boolean! }"

        for {
          products <- stub(
                        """{"data":{"status":"BROKEN"}}""",
                        """{"data":{"failed":null},"errors":[{"message":"update failed","path":["failed"]}]}"""
                      )
          reviews  <- stub("""{"data":{"added":true}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.graphql("products", products.endpoint, productsSchema),
                          Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                        )
                        .withConfig(_.withRemoteErrorDisclosure(_.withMessages(true)))
                        .interpreter
          response <- gateway.execute("mutation { status failed: fail added: addReview }")
          sent     <- reviews.requests.get
        } yield assertTrue(
          response.data == NullValue,
          response.errors.map(_.msg) == List("update failed", "Invalid value for enum 'Status'."),
          sent.isEmpty
        )
      },
      test("selects a viable provider for a shareable mutation root") {
        val alphaSchema =
          s"""
             |${federationSchemaPreamble("@shareable")}
             |type Query { alpha: String }
             |type Mutation { publish: Product! @shareable }
             |type Product { id: ID! }
             |""".stripMargin
        val betaSchema  =
          s"""
             |${federationSchemaPreamble("@shareable")}
             |type Query { beta: String }
             |type Mutation { publish: Product! @shareable }
             |type Product { title: String! }
             |""".stripMargin

        for {
          alpha     <- stub("""{"data":{"published":{"id":"p1"}}}""")
          beta      <- stub("""{"data":{"published":{"title":"Ready"}}}""")
          gateway   <- Gateway
                         .compose(
                           Subgraph.federation("alpha", alpha.endpoint, alphaSchema),
                           Subgraph.federation("beta", beta.endpoint, betaSchema)
                         )
                         .interpreter
          response  <- gateway.execute("mutation { published: publish { title } }")
          alphaSent <- alpha.requests.get
          betaSent  <- beta.requests.get
        } yield assertTrue(
          response.errors.isEmpty,
          field(response.data, "published").flatMap(field(_, "title")).contains(StringValue("Ready")),
          alphaSent.isEmpty,
          betaSent.size == 1
        )
      },
      test("collects repeated mutation root fields before remote execution") {
        val productsSchema =
          "type Query { product: String } type Mutation { updateProduct(id: ID!): Boolean! }"
        val reviewsSchema  =
          "type Query { reviews: [String!]! } type Mutation { addReview(text: String!): Boolean! }"

        for {
          products  <- stub("""{"data":{"updated":true}}""")
          reviews   <- stub("""{"data":{"added":true}}""")
          gateway   <- Gateway
                         .compose(
                           Subgraph.graphql("products", products.endpoint, productsSchema),
                           Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                         )
                         .interpreter
          response  <- gateway.execute(
                         """mutation {
                          |  updated: updateProduct(id: "p1")
                          |  updated: updateProduct(id: "p1")
                          |}""".stripMargin
                       )
          sent      <- products.requests.get
          untouched <- reviews.requests.get
          names      = fieldNames(response.data)
        } yield assertTrue(
          response.errors.isEmpty,
          names == List("updated"),
          sent.size == 1,
          untouched.isEmpty
        )
      },
      test("rejects custom operation directives in split requests") {
        val productsSchema =
          "directive @trace(label: String!) on QUERY type Query { product: String }"
        val reviewsSchema  =
          "type Query { reviews: [String!]! }"

        for {
          products    <- stub("""{"data":{"product":"Table"}}""")
          reviews     <- stub("""{"data":{"reviews":["Solid"]}}""")
          gateway     <- Gateway
                           .compose(
                             Subgraph.graphql("products", products.endpoint, productsSchema),
                             Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                           )
                           .interpreter
          response    <- gateway.execute(
                           """query Traced @trace(label: "client") { product reviews }""",
                           Some("Traced")
                         )
          productSent <- products.requests.get
          reviewSent  <- reviews.requests.get
        } yield assertTrue(
          response.data == NullValue,
          response.errors.map(_.msg) == List("Custom executable directives are not supported by this gateway."),
          productSent.isEmpty,
          reviewSent.isEmpty
        )
      },
      test("rejects custom fragment-definition directives before routing") {
        val productsSchema =
          "directive @trace on FRAGMENT_DEFINITION type Query { product: String }"
        val reviewsSchema  =
          "directive @trace on FRAGMENT_DEFINITION type Query { reviews: [String!]! }"

        for {
          products    <- stub("""{"data":{"product":"Table"}}""")
          reviews     <- stub("""{"data":{"reviews":["Solid"]}}""")
          gateway     <- Gateway
                           .compose(
                             Subgraph.graphql("products", products.endpoint, productsSchema),
                             Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                           )
                           .interpreter
          response    <- gateway.execute(
                           """query { ...Fields } fragment Fields on Query @trace { product reviews }"""
                         )
          productSent <- products.requests.get
          reviewSent  <- reviews.requests.get
        } yield assertTrue(
          response.errors.map(_.msg) == List("Custom executable directives are not supported by this gateway."),
          productSent.isEmpty,
          reviewSent.isEmpty
        )
      }
    ),
    suite("composition")(
      test("accumulates deterministic source-attributed composition diagnostics") {
        val endpoint = unreachableEndpoint
        val alpha    = Subgraph.graphql(
          "alpha",
          endpoint,
          "type Query { duplicate: String alpha: Product } type Product { value: String }"
        )
        val beta     = Subgraph.graphql(
          "beta",
          endpoint,
          "type Query { duplicate: Int beta: Product } type Product { value: Int }"
        )

        for {
          forward <- Gateway.compose(alpha, beta).interpreter.exit
          reverse <- Gateway.compose(beta, alpha).interpreter.exit
          first    = buildDiagnostics(forward)
          second   = buildDiagnostics(reverse)
        } yield assertTrue(
          forward.isFailure,
          reverse.isFailure,
          first == second,
          first.size == 2,
          first.exists(_.contains("query.duplicate")),
          first.exists(message =>
            message.contains("type Product") && message.contains("'alpha'") && message.contains("'beta'")
          )
        )
      },
      test("rejects compatible duplicate roots from ordinary subgraphs") {
        val endpoint = unreachableEndpoint
        val schema   = "type Query { duplicate: String }"
        val alpha    = Subgraph.graphql("alpha", endpoint, schema)
        val beta     = Subgraph.graphql("beta", endpoint, schema)

        for {
          forward <- Gateway.compose(alpha, beta).interpreter.exit
          reverse <- Gateway.compose(beta, alpha).interpreter.exit
          first    = buildDiagnostics(forward)
          second   = buildDiagnostics(reverse)
        } yield assertTrue(
          forward.isFailure,
          reverse.isFailure,
          first == second,
          first.exists(message =>
            message.contains("query.duplicate") && message.contains("'alpha'") && message.contains("'beta'")
          )
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

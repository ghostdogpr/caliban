package caliban.gateway

import caliban.ResponseValue.{ ListValue => ResponseListValue, ObjectValue => ResponseObjectValue }
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
                               .build
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
          names            = response.data match {
                               case ResponseObjectValue(fields) => fields.map(_._1)
                               case _                           => Nil
                             }
        } yield assertTrue(
          names == List("recent", "featured"),
          field(response.data, "featured").flatMap(field(_, "name")).contains(StringValue("Table")),
          field(response.data, "recent").exists {
            case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
              review.contains("body" -> StringValue("Solid"))
            case _                                                     => false
          },
          response.errors.map(_.msg) == List("review warning"),
          response.errors.collectFirst { case error: CalibanError.ExecutionError => error.path }.contains(
            List(StringValue("recent"), IntNumber(0), StringValue("body"))
          ),
          response.errors.collect { case error: CalibanError.ExecutionError => error }.forall(_.locationInfo.isEmpty),
          productRequests.size == 1,
          reviewRequests.size == 1,
          productRequests.head.operationName.contains("Dashboard"),
          reviewRequests.head.operationName.contains("Dashboard"),
          productRequests.head.variables.isEmpty,
          reviewRequests.head.variables.isEmpty,
          productValid.forall(_.isSuccess),
          reviewValid.forall(_.isSuccess),
          productRequests.head.extensions == request.extensions,
          reviewRequests.head.extensions == request.extensions,
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
                        .build
          response <- gateway.execute("{ featured { name } recent { body } }")
          errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
        } yield assertTrue(
          field(response.data, "featured").flatMap(field(_, "name")).contains(StringValue("Table")),
          field(response.data, "recent").contains(NullValue),
          errors.map(_.msg) == List("Remote GraphQL request failed."),
          errors.map(_.path) == List(List(PathValue.Key("recent")))
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
                              .build
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
                              .build
          responseB      <- gatewayB.execute("{ first second }")
        } yield assertTrue(
          responseA.errors.map(_.msg) == List("first failed", "second failed"),
          responseB.errors.map(_.msg) == List("first failed", "second failed"),
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
                            .build
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
                           .build
          response    <- gateway.execute(
                           """{
                          |  product
                          |  __schema { queryType { fields { name } } }
                          |  __type(name: "Review") { name }
                          |}""".stripMargin
                         )
          productSent <- products.requests.get
          reviewSent  <- reviews.requests.get
          names        = response.data match {
                           case ResponseObjectValue(fields) => fields.map(_._1)
                           case _                           => Nil
                         }
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
      test("rejects mutations spanning multiple sources until Ticket 16") {
        val productsSchema =
          "type Query { product: String } type Mutation { updateProduct(id: ID!): Boolean! }"
        val reviewsSchema  =
          "type Query { reviews: [String!]! } type Mutation { addReview(text: String!): Boolean! }"

        for {
          products    <- stub("""{"data":{"updated":true}}""")
          reviews     <- stub("""{"data":{"added":true}}""")
          gateway     <- Gateway
                           .compose(
                             Subgraph.graphql("products", products.endpoint, productsSchema),
                             Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema)
                           )
                           .build
          response    <- gateway.execute(
                           """mutation Changes {
                          |  added: addReview(text: "Good")
                          |  updated: updateProduct(id: "p1")
                          |}""".stripMargin,
                           Some("Changes")
                         )
          productSent <- products.requests.get
          reviewSent  <- reviews.requests.get
        } yield assertTrue(
          response.data == NullValue,
          response.errors.map(_.msg) == List(
            "Mutations spanning multiple subgraphs are not supported by this gateway."
          ),
          productSent.isEmpty,
          reviewSent.isEmpty
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
                         .build
          response  <- gateway.execute(
                         """mutation {
                          |  updated: updateProduct(id: "p1")
                          |  updated: updateProduct(id: "p1")
                          |}""".stripMargin
                       )
          sent      <- products.requests.get
          untouched <- reviews.requests.get
          names      = response.data match {
                         case ResponseObjectValue(fields) => fields.map(_._1)
                         case _                           => Nil
                       }
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
                           .build
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
                           .build
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
    suite("composition")(test("accumulates deterministic source-attributed composition diagnostics") {
      val endpoint = Uri.unsafeParse("http://127.0.0.1:1/graphql")
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
        forward <- Gateway.compose(alpha, beta).build.exit
        reverse <- Gateway.compose(beta, alpha).build.exit
        first    = forward.causeOption.flatMap(_.failureOption).map(_.diagnostics)
        second   = reverse.causeOption.flatMap(_.failureOption).map(_.diagnostics)
      } yield assertTrue(
        forward.isFailure,
        reverse.isFailure,
        first == second,
        first.exists(_.size == 2),
        first.exists(_.exists(_.contains("query.duplicate"))),
        first.exists(
          _.exists(message =>
            message.contains("type Product") && message.contains("'alpha'") && message.contains("'beta'")
          )
        )
      )
    })
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

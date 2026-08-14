package caliban.gateway

import caliban.InputValue.ListValue
import caliban.ResponseValue
import caliban.ResponseValue.{ ListValue => ResponseListValue, ObjectValue => ResponseObjectValue }
import caliban.Value.IntValue.IntNumber
import caliban.Value.{ BooleanValue, NullValue, StringValue }
import caliban.execution.RequestPreparation
import caliban.parsing.Parser
import caliban.tools.RemoteSchema
import caliban.{ CalibanError, GraphQLInterpreter, GraphQLRequest }
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import sttp.model.Uri
import zio._
import zio.http.{ Body, Handler, Header, Headers, Method, Request, Response, Routes, Server, Status }
import zio.http.netty.NettyConfig
import zio.test._

object GatewaySpec extends ZIOSpecDefault {

  private final case class Stub(endpoint: Uri, requests: Ref[List[GraphQLRequest]])

  private val schema =
    """
      |type Query {
      |  products(ids: [ID!]!): [Product!]!
      |}
      |
      |type Product {
      |  id: ID!
      |  details: Details!
      |  reviews: [Review!]!
      |  legacyName: String @deprecated(reason: "Use details.name")
      |}
      |
      |type Details {
      |  name: String!
      |}
      |
      |type Review {
      |  body: String!
      |}
      |
      |scalar URL @specifiedBy(url: "https://example.com/url")
      |""".stripMargin

  private val nestedQuery =
    """
      |query Products($ids: [ID!]!, $includeReviews: Boolean!) {
      |  catalog: products(ids: $ids) {
      |    ...ProductDetails
      |    reviews @include(if: $includeReviews) {
      |      body
      |    }
      |  }
      |}
      |
      |fragment ProductDetails on Product {
      |  id
      |  details { name }
      |}
      |""".stripMargin

  private val dataResponse =
    """{"data":{"catalog":[{"id":"p1","details":{"name":"Table"},"reviews":[{"body":"Solid"}]}]}}"""

  private val partialResponse =
    """{"data":{"catalog":null},"errors":[{"message":"catalog unavailable","path":["catalog"]}]}"""

  private val errorsResponse =
    """{"errors":[{"message":"request rejected"}]}"""

  private val invalidResponse = """{"unexpected":true}"""

  private def stub(responses: String*): ZIO[Server with Ref[Int], Nothing, Stub] =
    stubWith(ZIO.unit, responses: _*)

  private def stubWith(beforeResponse: UIO[Unit], responses: String*): ZIO[Server with Ref[Int], Nothing, Stub] =
    for {
      requests <- Ref.make(List.empty[GraphQLRequest])
      index    <- Ref.make(0)
      id       <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
      path      = s"graphql-$id"
      handler   = Handler.fromFunctionZIO[Request] { request =>
                    for {
                      bytes   <- request.body.asArray.orDie
                      decoded <- ZIO.attempt(readFromArray[GraphQLRequest](bytes)).orDie
                      _       <- requests.update(_ :+ decoded)
                      _       <- beforeResponse
                      next    <- index.getAndUpdate(_ + 1)
                      body     = responses(math.min(next, responses.size - 1))
                    } yield Response(
                      Status.Ok,
                      Headers(Header.Custom("Content-Type", "application/graphql-response+json")),
                      Body.fromString(body)
                    )
                  }
      server   <- ZIO.service[Server]
      _        <- server.install(Routes(Method.POST / path -> handler))
      port     <- server.port
    } yield Stub(Uri.unsafeParse(s"http://127.0.0.1:$port/$path"), requests)

  private val testServer: ZLayer[Any, Throwable, Server] = {
    val config = Server.Config.default
      .binding("127.0.0.1", 0)
      .gracefulShutdownTimeout(Duration.Zero)

    (ZLayer.succeed(config) ++ ZLayer.succeed(NettyConfig.defaultWithFastShutdown)) >>> Server.customized
  }

  private val stubIds: ULayer[Ref[Int]] = ZLayer.fromZIO(Ref.make(0))

  private def runtime(stub: Stub): ZIO[Scope, GatewayBuildError, GatewayRuntime[Any]] =
    Gateway.compose(Subgraph.graphql("products", stub.endpoint, schema)).build

  private def field(value: ResponseValue, name: String): Option[ResponseValue] =
    value match {
      case ResponseObjectValue(fields) => fields.collectFirst { case (`name`, value) => value }
      case _                           => None
    }

  private def validateRequest(schema: String, request: GraphQLRequest): IO[CalibanError, Unit] =
    for {
      document <- ZIO.fromEither(Parser.parseQuery(schema))
      rootType <- ZIO.fromEither(RemoteSchema.toRootType(document))
      _        <- RequestPreparation.prepare(request, rootType)
    } yield ()

  def spec = suite("GatewaySpec")(
    test("executes one pinned remote graph end to end through GatewayRuntime") {
      for {
        remote                                            <- stub(dataResponse)
        gateway                                           <- runtime(remote)
        interpreter: GraphQLInterpreter[Any, CalibanError] = gateway
        request                                            = GraphQLRequest(
                                                               query = Some(nestedQuery),
                                                               operationName = Some("Products"),
                                                               variables = Some(
                                                                 Map(
                                                                   "ids"            -> ListValue(List(StringValue("p1"))),
                                                                   "includeReviews" -> BooleanValue(true)
                                                                 )
                                                               ),
                                                               extensions = Some(Map("client" -> StringValue("gateway-spec")))
                                                             )
        response                                          <- interpreter.executeRequest(request)
        requests                                          <- remote.requests.get
        catalog                                            = field(response.data, "catalog")
      } yield assertTrue(
        response.errors.isEmpty,
        catalog.exists {
          case ResponseListValue(ResponseObjectValue(product) :: Nil) =>
            product.collectFirst { case ("details", ResponseObjectValue(details)) =>
              details.contains("name" -> StringValue("Table"))
            }.contains(true)
          case _                                                      => false
        },
        requests == List(request)
      )
    },
    test("accepts partial data plus remote GraphQL errors") {
      for {
        remote   <- stub(partialResponse)
        gateway  <- runtime(remote)
        response <- gateway.execute(
                      nestedQuery,
                      Some("Products"),
                      Map("ids" -> ListValue(List(StringValue("p1"))), "includeReviews" -> BooleanValue(true))
                    )
      } yield assertTrue(
        field(response.data, "catalog").contains(NullValue),
        response.errors.map(_.msg) == List("catalog unavailable")
      )
    },
    test("accepts a remote GraphQL errors-only response") {
      for {
        remote   <- stub(errorsResponse)
        gateway  <- runtime(remote)
        response <- gateway.execute("{ products(ids: [\"p1\"]) { id } }")
      } yield assertTrue(response.data == NullValue, response.errors.map(_.msg) == List("request rejected"))
    },
    test("turns an invalid remote response into a safe gateway error") {
      for {
        remote   <- stub(invalidResponse)
        gateway  <- runtime(remote)
        response <- gateway.execute("{ products(ids: [\"p1\"]) { id } }")
      } yield assertTrue(
        response.data == NullValue,
        response.errors.map(_.msg) == List("Remote GraphQL request failed.")
      )
    },
    test("executes introspection locally without calling the remote graph") {
      for {
        remote   <- stub(dataResponse)
        gateway  <- runtime(remote)
        response <- gateway.execute(
                      """{
                        |  product: __type(name: "Product") {
                        |    visible: fields { name }
                        |    all: fields(includeDeprecated: true) { name }
                        |  }
                        |  scalar: __type(name: "URL") { specifiedByURL }
                        |}""".stripMargin
                    )
        requests <- remote.requests.get
        product   = field(response.data, "product")
        visible   = product.flatMap(field(_, "visible")).collect { case ResponseListValue(values) =>
                      values.flatMap(field(_, "name")).collect { case StringValue(value) => value }
                    }
        all       = product.flatMap(field(_, "all")).collect { case ResponseListValue(values) =>
                      values.flatMap(field(_, "name")).collect { case StringValue(value) => value }
                    }
        url       = field(response.data, "scalar").flatMap(field(_, "specifiedByURL"))
      } yield assertTrue(
        response.errors.isEmpty,
        visible.exists(!_.contains("legacyName")),
        all.exists(_.contains("legacyName")),
        url.contains(StringValue("https://example.com/url")),
        requests.isEmpty
      )
    },
    test("builds SDL and parsed documents through the same validated schema path") {
      for {
        remote   <- stub(dataResponse)
        document <- ZIO.fromEither(Parser.parseQuery(schema))
        extended  = schema + "\nextend type Query { version: String }"
        fromSdl  <- Gateway.compose(Subgraph.graphql("sdl", remote.endpoint, schema)).build.exit
        fromDoc  <- Gateway.compose(Subgraph.graphql("document", remote.endpoint, document)).build.exit
        fromExt  <- Gateway
                      .compose(Subgraph.graphql("extended", remote.endpoint, extended))
                      .build
                      .flatMap {
                        _.check("{ version }")
                      }
                      .exit
        invalid  <- Gateway
                      .compose(Subgraph.graphql("invalid", remote.endpoint, "type Query { broken: Missing }"))
                      .build
                      .exit
      } yield assertTrue(fromSdl.isSuccess, fromDoc.isSuccess, fromExt.isSuccess, invalid.isFailure)
    },
    test("rejects invalid client operations before contacting the remote graph") {
      for {
        remote   <- stub(dataResponse)
        gateway  <- runtime(remote)
        response <- gateway.execute("{ missing }")
        requests <- remote.requests.get
      } yield assertTrue(response.errors.nonEmpty, requests.isEmpty)
    },
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
            """{"data":{"recent":[{"body":"Solid"}]},"errors":[{"message":"review warning","path":["recent",0,"body"]}]}"""
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
    },
    test("keeps single-subgraph meta fields local for a custom remote root") {
      val customRootSchema =
        "schema { query: RootQuery } type RootQuery { product(id: ID!): Product } type Product { id: ID! }"
      val query            =
        """
          |query Dashboard($id: ID!) {
          |  featured: product(id: $id) { id }
          |  __typename
          |  __schema { queryType { name } }
          |  __type(name: "Product") { name }
          |}
          |""".stripMargin

      for {
        products <- stub("""{"data":{"featured":{"id":"p1"}}}""")
        gateway  <- Gateway.compose(Subgraph.graphql("products", products.endpoint, customRootSchema)).build
        request   = GraphQLRequest(
                      query = Some(query),
                      operationName = Some("Dashboard"),
                      variables = Some(Map("id" -> StringValue("p1")))
                    )
        response <- gateway.executeRequest(request)
        sent     <- products.requests.get
        valid    <- ZIO.foreach(sent)(validateRequest(customRootSchema, _).exit)
        names     = response.data match {
                      case ResponseObjectValue(fields) => fields.map(_._1)
                      case _                           => Nil
                    }
      } yield assertTrue(
        response.errors.isEmpty,
        names == List("featured", "__typename", "__schema", "__type"),
        field(response.data, "__typename").contains(StringValue("Query")),
        field(response.data, "__schema")
          .flatMap(field(_, "queryType"))
          .flatMap(field(_, "name"))
          .contains(StringValue("Query")),
        field(response.data, "__type").flatMap(field(_, "name")).contains(StringValue("Product")),
        sent.size == 1,
        sent.head != request,
        sent.head.variables.isEmpty,
        sent.head.query.exists(query =>
          query.contains("featured:product(id:\"p1\")") &&
            !query.contains("__schema") && !query.contains("__type") && !query.contains("__typename")
        ),
        valid.forall(_.isSuccess)
      )
    },
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
    },
    test("accumulates deterministic source-attributed composition diagnostics") {
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
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

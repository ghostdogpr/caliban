package caliban.gateway

import caliban.InputValue.{ ListValue, ObjectValue => InputObjectValue }
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

  private val federationDirectives =
    """
      |directive @link(url: String!, as: String, import: [link__Import], for: link__Purpose) repeatable on SCHEMA
      |directive @key(fields: federation__FieldSet!, resolvable: Boolean = true) repeatable on OBJECT | INTERFACE
      |directive @external on FIELD_DEFINITION
      |scalar link__Import
      |enum link__Purpose { SECURITY EXECUTION }
      |scalar federation__FieldSet
      |scalar _Any
      |type _Service { sdl: String! }
      |""".stripMargin

  private val productsFederationSchema =
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"]) { query: Query }
       |$federationDirectives
       |union _Entity = Product
       |type Query {
       |  product(id: ID!): Product
       |  _entities(representations: [_Any!]!): [_Entity]!
       |  _service: _Service!
       |}
       |type Product @key(fields: "id") { id: ID! name: String! }
       |""".stripMargin

  private val reviewsFederationSchema =
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"]) { query: Query }
       |$federationDirectives
       |union _Entity = Product
       |type Query {
       |  _entities(representations: [_Any!]!): [_Entity]!
       |  _service: _Service!
       |}
       |type Product @key(fields: "id") { id: ID! @external reviews: [Review!]! }
       |type Review { body: String! }
       |""".stripMargin

  private val authoredFederationDirectives =
    """
      |directive @link(url: String!, as: String, import: [link__Import], for: link__Purpose) repeatable on SCHEMA
      |directive @key(fields: federation__FieldSet!, resolvable: Boolean = true) repeatable on OBJECT | INTERFACE
      |directive @external on FIELD_DEFINITION
      |scalar link__Import
      |enum link__Purpose { SECURITY EXECUTION }
      |scalar federation__FieldSet
      |""".stripMargin

  private val authoredProductsFederationSchema =
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"]) { query: Query }
       |$authoredFederationDirectives
       |type Query { product(id: ID!): Product }
       |type Product @key(fields: "id") { id: ID! name: String! }
       |""".stripMargin

  private val authoredReviewsFederationSchema =
    s"""
       |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])
       |$authoredFederationDirectives
       |type Product @key(fields: "id") { id: ID! @external reviews: [Review!]! }
       |type Review { body: String! }
       |""".stripMargin

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
    test("executes one Federation entity join through the executable plan") {
      val productResponse          =
        """{"data":{"product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
      val aliasedProductResponse   =
        """{"data":{"product":{"productId":"p1","__typename":"Product","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
      val collidingProductResponse =
        """{"data":{"product":{"id":"Table","__typename":"Table","_caliban_gateway_key":"Table","_caliban_gateway_typename":"Table","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product"}}}"""
      val reviewResponse           =
        """{"data":{"_entities":[{"reviews":[{"body":"Solid"}]}]}}"""
      val query                    =
        """query Product {
          |  product(id: "p1") {
          |    name
          |    reviews { body }
          |  }
          |}""".stripMargin
      val conditionalQuery         =
        """query Product($includeReviews: Boolean!) {
          |  product(id: "p1") {
          |    name
          |    reviews @include(if: $includeReviews) { body }
          |  }
          |}""".stripMargin

      for {
        products        <- stub(productResponse, aliasedProductResponse, collidingProductResponse)
        reviews         <- stub(reviewResponse)
        gateway         <- Gateway
                             .compose(
                               Subgraph.federation("products", products.endpoint, productsFederationSchema),
                               Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                             )
                             .build
        explanation     <- gateway.explain(query, Some("Product"))
        withoutReviews  <- gateway.explain(
                             GraphQLRequest(
                               query = Some(conditionalQuery),
                               operationName = Some("Product"),
                               variables = Some(Map("includeReviews" -> BooleanValue(false)))
                             )
                           )
        withReviews     <- gateway.explain(
                             GraphQLRequest(
                               query = Some(conditionalQuery),
                               operationName = Some("Product"),
                               variables = Some(Map("includeReviews" -> BooleanValue(true)))
                             )
                           )
        response        <- gateway.execute(query, Some("Product"))
        explicit        <- gateway.execute("{ product(id: \"p1\") { productId: id __typename reviews { body } } }")
        colliding       <-
          gateway.execute(
            "{ product(id: \"p1\") { id: name __typename: name _caliban_gateway_key: name _caliban_gateway_typename: name reviews { body } } }"
          )
        introspection   <- gateway.execute(
                             """{
                             |  query: __type(name: "Query") { fields { name } }
                             |  transport: __type(name: "_Service") { name }
                             |  linkPurpose: __type(name: "link__Purpose") { name }
                             |  schema: __schema { directives { name } }
                             |}""".stripMargin
                           )
        productSent     <- products.requests.get
        reviewSent      <- reviews.requests.get
        productValid    <- ZIO.foreach(productSent)(validateRequest(productsFederationSchema, _).exit)
        reviewValid     <- ZIO.foreach(reviewSent)(validateRequest(reviewsFederationSchema, _).exit)
        product          = field(response.data, "product")
        explicitProduct  = field(explicit.data, "product")
        collidingProduct = field(colliding.data, "product")
        queryFields      = field(introspection.data, "query")
                             .flatMap(field(_, "fields"))
                             .collect { case ResponseListValue(values) =>
                               values.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                             }
        directives       = field(introspection.data, "schema")
                             .flatMap(field(_, "directives"))
                             .collect { case ResponseListValue(values) =>
                               values.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                             }
      } yield assertTrue(
        response.errors.isEmpty,
        product.flatMap(field(_, "name")).contains(StringValue("Table")),
        product.flatMap(field(_, "reviews")).exists {
          case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
            review.contains("body" -> StringValue("Solid"))
          case _                                                     => false
        },
        product.flatMap(field(_, "id")).isEmpty,
        product.flatMap(field(_, "__typename")).isEmpty,
        explicitProduct.flatMap(field(_, "productId")).contains(StringValue("p1")),
        explicitProduct.flatMap(field(_, "id")).isEmpty,
        explicitProduct.flatMap(field(_, "__typename")).contains(StringValue("Product")),
        queryFields.contains(List("product")),
        field(introspection.data, "transport").contains(NullValue),
        field(introspection.data, "linkPurpose").contains(NullValue),
        directives.exists(names => !names.contains("link") && !names.contains("key")),
        colliding.errors.isEmpty,
        collidingProduct.flatMap(field(_, "id")).contains(StringValue("Table")),
        collidingProduct.flatMap(field(_, "__typename")).contains(StringValue("Table")),
        collidingProduct.flatMap(field(_, "_caliban_gateway_key")).contains(StringValue("Table")),
        collidingProduct.flatMap(field(_, "_caliban_gateway_typename")).contains(StringValue("Table")),
        collidingProduct.flatMap(field(_, "_caliban_gateway_key_2")).isEmpty,
        collidingProduct.flatMap(field(_, "_caliban_gateway_typename_2")).isEmpty,
        productSent.size == 3,
        reviewSent.size == 3,
        productValid.forall(_.isSuccess),
        reviewValid.forall(_.isSuccess),
        productSent.head.query.exists(rendered =>
          rendered.contains("product(id:\"p1\")") &&
            rendered.contains("name") && rendered.contains("_caliban_gateway_key:id") &&
            rendered.contains("_caliban_gateway_typename:__typename") &&
            !rendered.contains("reviews")
        ),
        reviewSent.head.query.exists(rendered =>
          rendered.contains("_entities") && rendered.contains("...on Product") && rendered.contains("reviews{body}")
        ),
        reviewSent.head.variables.contains(
          Map(
            "representations" -> ListValue(
              List(InputObjectValue(Map("__typename" -> StringValue("Product"), "id" -> StringValue("p1"))))
            )
          )
        ),
        explanation ==
          """query
            |fetch products at $.product fields [name, id (key), __typename (key)]
            |fetch reviews after products at $.product via Product(id) fields [reviews.body]""".stripMargin,
        !withoutReviews.contains("fetch reviews"),
        withReviews.contains("fetch reviews")
      )
    },
    test("skips an entity lookup when the nullable parent is null") {
      for {
        products <- stub("""{"data":{"product":null}}""")
        reviews  <- stub("""{"data":{"_entities":[]}}""")
        gateway  <- Gateway
                      .compose(
                        Subgraph.federation("products", products.endpoint, productsFederationSchema),
                        Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                      )
                      .build
        response <- gateway.execute("{ product(id: \"missing\") { reviews { body } } }")
        sentA    <- products.requests.get
        sentB    <- reviews.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "product").contains(NullValue),
        sentA.size == 1,
        sentB.isEmpty
      )
    },
    test("rejects list-valued entity joins during planning") {
      val listProducts = productsFederationSchema.replace("product(id: ID!): Product", "products: [Product!]!")

      for {
        products <- stub("""{"data":{"products":[]}}""")
        reviews  <- stub("""{"data":{"_entities":[]}}""")
        gateway  <- Gateway
                      .compose(
                        Subgraph.federation("products", products.endpoint, listProducts),
                        Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                      )
                      .build
        response <- gateway.execute("{ products { reviews { body } } }")
        sentA    <- products.requests.get
        sentB    <- reviews.requests.get
      } yield assertTrue(
        response.errors.map(_.msg) ==
          List("Federation entity joins from list-valued field 'products' are not supported."),
        sentA.isEmpty,
        sentB.isEmpty
      )
    },
    test("executes a join from entity-only authored Federation service SDL with namespaced metadata") {
      val productResponse =
        """{"data":{"product":{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
      val reviewResponse  =
        """{"data":{"_entities":[{"reviews":[{"body":"Solid"}]}]}}"""
      val productsSchema  = authoredProductsFederationSchema
        .replace(", import: [\"@key\"]", ", as: \"fed\"")
        .replace("federation__FieldSet", "fed__FieldSet")
        .replace("@key", "@fed__key")
      val reviewsSchema   = authoredReviewsFederationSchema
        .replace(", import: [\"@key\", \"@external\"]", ", as: \"fed\"")
        .replace("federation__FieldSet", "fed__FieldSet")
        .replace("@key", "@fed__key")
        .replace("@external", "@fed__external")

      for {
        products  <- stub(productResponse)
        reviews   <- stub(reviewResponse)
        gateway   <- Gateway
                       .compose(
                         Subgraph.federation("products", products.endpoint, productsSchema),
                         Subgraph.federation("reviews", reviews.endpoint, reviewsSchema)
                       )
                       .build
        response  <- gateway.execute("{ product(id: \"p1\") { name reviews { body } } }")
        metadata  <- gateway.execute(
                       "{ transport: __type(name: \"fed__FieldSet\") { name } schema: __schema { directives { name } } }"
                     )
        sentA     <- products.requests.get
        sentB     <- reviews.requests.get
        directives = field(metadata.data, "schema")
                       .flatMap(field(_, "directives"))
                       .collect { case ResponseListValue(values) =>
                         values.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                       }
      } yield assertTrue(
        response.errors.isEmpty,
        metadata.errors.isEmpty,
        field(response.data, "product").flatMap(field(_, "name")).contains(StringValue("Table")),
        field(response.data, "product").flatMap(field(_, "reviews")).exists {
          case ResponseListValue(ResponseObjectValue(review) :: Nil) =>
            review.contains("body" -> StringValue("Solid"))
          case _                                                     => false
        },
        field(metadata.data, "transport").contains(NullValue),
        directives.exists(names => !names.contains("fed__key") && !names.contains("fed__external")),
        sentA.size == 1,
        sentB.size == 1,
        sentB.head.query.exists(_.contains("_entities"))
      )
    },
    test("hides imported Federation directive aliases from the client schema") {
      val endpoint = Uri.unsafeParse("http://127.0.0.1:1/graphql")
      val products = productsFederationSchema
        .replace("import: [\"@key\"]", "import: [{ name: \"@key\", as: \"@entityKey\" }]")
        .replace("directive @key", "directive @entityKey")
        .replace("@key(fields:", "@entityKey(fields:")
      val reviews  = reviewsFederationSchema
        .replace(
          "import: [\"@key\", \"@external\"]",
          "import: [{ name: \"@key\", as: \"@entityKey\" }, { name: \"@external\", as: \"@outside\" }]"
        )
        .replace("directive @key", "directive @entityKey")
        .replace("directive @external", "directive @outside")
        .replace("@key(fields:", "@entityKey(fields:")
        .replace("@external", "@outside")

      for {
        gateway       <- Gateway
                           .compose(
                             Subgraph.federation("products", endpoint, products),
                             Subgraph.federation("reviews", endpoint, reviews)
                           )
                           .build
        introspection <- gateway.execute("{ __schema { directives { name } } }")
        directives     = field(introspection.data, "__schema")
                           .flatMap(field(_, "directives"))
                           .collect { case ResponseListValue(values) =>
                             values.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                           }
      } yield assertTrue(
        introspection.errors.isEmpty,
        directives.exists(names => !names.contains("entityKey") && !names.contains("outside"))
      )
    },
    test("rejects impossible Federation joins before contacting a subgraph") {
      val missingKeyProducts = productsFederationSchema.replace("id: ID! name: String!", "name: String!")
      val noLookupReviews    = reviewsFederationSchema.replace(
        "@key(fields: \"id\")",
        "@key(fields: \"id\", resolvable: false)"
      )
      val wrongEntityReviews = reviewsFederationSchema.replace("union _Entity = Product", "union _Entity = Review")
      val cycleProducts      = productsFederationSchema.replace(
        "type Product @key(fields: \"id\") { id: ID! name: String! }",
        "type Product @key(fields: \"id\") { id: ID! name: String! } type Review @key(fields: \"id\") { id: ID! product: Product! }"
      )
      val cycleReviews       = reviewsFederationSchema.replace(
        "type Review { body: String! }",
        "type Review @key(fields: \"id\") { id: ID! body: String! product: Product! @external }"
      )

      def rejected(productsSchema: String, reviewsSchema: String, query: String) =
        for {
          products <- stub("""{"data":{"product":null}}""")
          reviews  <- stub("""{"data":{"_entities":[]}}""")
          gateway  <- Gateway
                        .compose(
                          Subgraph.federation("products", products.endpoint, productsSchema),
                          Subgraph.federation("reviews", reviews.endpoint, reviewsSchema)
                        )
                        .build
          response <- gateway.execute(query)
          sentA    <- products.requests.get
          sentB    <- reviews.requests.get
        } yield (response.errors.map(_.msg), sentA, sentB)

      for {
        missing     <- rejected(
                         missingKeyProducts,
                         reviewsFederationSchema,
                         "{ product(id: \"p1\") { reviews { body } } }"
                       )
        lookup      <- rejected(
                         productsFederationSchema,
                         noLookupReviews,
                         "{ product(id: \"p1\") { reviews { body } } }"
                       )
        wrongEntity <- rejected(
                         productsFederationSchema,
                         wrongEntityReviews,
                         "{ product(id: \"p1\") { reviews { body } } }"
                       )
        cycle       <- rejected(
                         cycleProducts,
                         cycleReviews,
                         "{ product(id: \"p1\") { reviews { product { name } } } }"
                       )
      } yield assertTrue(
        missing._1 == List("Cannot route 'Product.reviews': source 'products' does not provide key field 'id'."),
        lookup._1 == List("Cannot route 'Product.reviews': source 'reviews' has no resolvable entity lookup."),
        wrongEntity._1 == List("Cannot route 'Product.reviews': source 'reviews' has no resolvable entity lookup."),
        cycle._1 == List("Federation routing cycle detected: products -> reviews -> products."),
        missing._2.isEmpty,
        missing._3.isEmpty,
        lookup._2.isEmpty,
        lookup._3.isEmpty,
        wrongEntity._2.isEmpty,
        wrongEntity._3.isEmpty,
        cycle._2.isEmpty,
        cycle._3.isEmpty
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

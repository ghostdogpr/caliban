package caliban.gateway

import caliban.InputValue.ListValue
import caliban.ResponseValue
import caliban.ResponseValue.{ ListValue => ResponseListValue, ObjectValue => ResponseObjectValue }
import caliban.Value.{ BooleanValue, NullValue, StringValue }
import caliban.parsing.Parser
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
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}

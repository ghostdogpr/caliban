package caliban.gateway

import caliban.ResponseValue
import caliban.ResponseValue.ObjectValue
import caliban.execution.RequestPreparation
import caliban.parsing.Parser
import caliban.tools.RemoteSchema
import caliban.{ CalibanError, GraphQLRequest }
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import sttp.model.Uri
import zio._
import zio.http.{ Body, Handler, Header, Headers, Method, Request, Response, Routes, Server, Status }
import zio.http.netty.NettyConfig

private[gateway] object GatewayTestSupport {

  def buildDiagnostics[A](exit: Exit[GatewayBuildError, A]): List[String] =
    exit.causeOption.flatMap(_.failureOption).fold(List.empty[String])(_.diagnostics)

  final case class Stub(
    endpoint: Uri,
    requests: Ref[Vector[GraphQLRequest]],
    headers: Ref[Vector[Headers]]
  )

  val invalidResponse = """{"unexpected":true}"""

  val authoredFederationDirectives =
    """
      |directive @link(url: String!, as: String, import: [link__Import], for: link__Purpose) repeatable on SCHEMA
      |directive @key(fields: federation__FieldSet!, resolvable: Boolean = true) repeatable on OBJECT | INTERFACE
      |directive @external on FIELD_DEFINITION
      |directive @shareable repeatable on OBJECT | FIELD_DEFINITION
      |directive @inaccessible on FIELD_DEFINITION | OBJECT | INTERFACE | UNION | ARGUMENT_DEFINITION | SCALAR | ENUM | ENUM_VALUE | INPUT_OBJECT | INPUT_FIELD_DEFINITION
      |directive @override(from: String!) on FIELD_DEFINITION
      |directive @interfaceObject on OBJECT
      |scalar link__Import
      |enum link__Purpose { SECURITY EXECUTION }
      |scalar federation__FieldSet
      |""".stripMargin

  def federationSchemaPreamble(imports: String*): String =
    federationSchemaPreamble("extend schema", "", imports)

  def federationSchemaPreambleWithQueryRoot(imports: String*): String =
    federationSchemaPreamble("schema", " { query: Query }", imports)

  private def federationSchemaPreamble(declaration: String, root: String, imports: Seq[String]): String = {
    val renderedImports = imports.map(value => "\"" + value + "\"").mkString(", ")
    s"""$declaration @link(url: "https://specs.apollo.dev/federation/v2.3", import: [$renderedImports])$root
       |$authoredFederationDirectives""".stripMargin
  }

  val federationDirectives =
    """
      |directive @link(url: String!, as: String, import: [link__Import], for: link__Purpose) repeatable on SCHEMA
      |directive @key(fields: federation__FieldSet!, resolvable: Boolean = true) repeatable on OBJECT | INTERFACE
      |directive @external on FIELD_DEFINITION
      |directive @shareable repeatable on OBJECT | FIELD_DEFINITION
      |directive @requires(fields: federation__FieldSet!) on FIELD_DEFINITION
      |directive @provides(fields: federation__FieldSet!) on FIELD_DEFINITION
      |scalar link__Import
      |enum link__Purpose { SECURITY EXECUTION }
      |scalar federation__FieldSet
      |scalar _Any
      |type _Service { sdl: String! }
      |""".stripMargin

  val productsFederationSchema =
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

  val reviewsFederationSchema =
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

  def stub(responses: String*): ZIO[Server with Ref[Int], Nothing, Stub] =
    stubWith(ZIO.unit, responses: _*)

  def stubWith(beforeResponse: UIO[Unit], responses: String*): ZIO[Server with Ref[Int], Nothing, Stub] =
    stubResponding(beforeResponse)((_, index) => responses(math.min(index, responses.size - 1)))

  def stubByRequest(response: GraphQLRequest => String): ZIO[Server with Ref[Int], Nothing, Stub] =
    stubResponding(ZIO.unit)((request, _) => response(request))

  private def stubResponding(
    beforeResponse: UIO[Unit]
  )(response: (GraphQLRequest, Int) => String): ZIO[Server with Ref[Int], Nothing, Stub] =
    for {
      requests <- Ref.make(Vector.empty[GraphQLRequest])
      headers  <- Ref.make(Vector.empty[Headers])
      index    <- Ref.make(0)
      id       <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
      path      = s"graphql-$id"
      handler   = Handler.fromFunctionZIO[Request] { request =>
                    for {
                      bytes   <- request.body.asArray.orDie
                      decoded <- ZIO.attempt(readFromArray[GraphQLRequest](bytes)).orDie
                      _       <- requests.update(_ :+ decoded)
                      _       <- headers.update(_ :+ request.headers)
                      _       <- beforeResponse
                      next    <- index.getAndUpdate(_ + 1)
                      body     = response(decoded, next)
                    } yield Response(
                      Status.Ok,
                      Headers(Header.Custom("Content-Type", "application/graphql-response+json")),
                      Body.fromString(body)
                    )
                  }
      server   <- ZIO.service[Server]
      _        <- server.install(Routes(Method.POST / path -> handler))
      port     <- server.port
    } yield Stub(Uri.unsafeParse(s"http://127.0.0.1:$port/$path"), requests, headers)

  val testServer: ZLayer[Any, Throwable, Server] = {
    val config = Server.Config.default
      .binding("127.0.0.1", 0)
      .gracefulShutdownTimeout(Duration.Zero)

    (ZLayer.succeed(config) ++ ZLayer.succeed(NettyConfig.defaultWithFastShutdown)) >>> Server.customized
  }

  val stubIds: ULayer[Ref[Int]] = ZLayer.fromZIO(Ref.make(0))

  def field(value: ResponseValue, name: String): Option[ResponseValue] =
    value match {
      case ObjectValue(fields) => fields.collectFirst { case (`name`, value) => value }
      case _                   => None
    }

  def validateRequest(schema: String, request: GraphQLRequest): IO[CalibanError, Unit] =
    for {
      document <- ZIO.fromEither(Parser.parseQuery(schema))
      rootType <- ZIO.fromEither(RemoteSchema.toRootType(document))
      _        <- RequestPreparation.prepare(request, rootType)
    } yield ()
}

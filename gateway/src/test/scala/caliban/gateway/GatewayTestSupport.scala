package caliban.gateway

import caliban.ResponseValue
import caliban.ResponseValue.ObjectValue
import caliban.execution.RequestPreparation
import caliban.introspection.Introspector
import caliban.parsing.Parser
import caliban.schema.{ GenericSchema, Schema }
import caliban.tools.RemoteSchema
import caliban.validation.Validator
import caliban.{ graphQL, CalibanError, GraphQLRequest, RootResolver }
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import sttp.model.Uri
import zio._
import zio.http.{ Body, Handler, Header, Headers, MediaType, Method, Request, Response, Routes, Server, Status }
import zio.http.netty.NettyConfig
import zio.metrics.Metric
import zio.stream.ZStream

import java.nio.charset.StandardCharsets

private[gateway] object GatewayTestSupport {

  def buildDiagnostics[A](exit: Exit[GatewayBuildError, A]): List[String] =
    exit.causeOption.flatMap(_.failureOption).fold(List.empty[String])(_.diagnostics)

  final case class Stub(
    endpoint: Uri,
    requests: Ref[Vector[GraphQLRequest]],
    headers: Ref[Vector[Headers]]
  )

  val invalidResponse          = """{"unexpected":true}"""
  val unreachableEndpoint: Uri = Uri.unsafeParse("http://127.0.0.1:1/graphql")

  private val baseFederationDirectives =
    """
      |directive @link(url: String!, as: String, import: [link__Import], for: link__Purpose) repeatable on SCHEMA
      |directive @key(fields: federation__FieldSet!, resolvable: Boolean = true) repeatable on OBJECT | INTERFACE
      |directive @external on FIELD_DEFINITION
      |directive @shareable repeatable on OBJECT | FIELD_DEFINITION
      |scalar link__Import
      |enum link__Purpose { SECURITY EXECUTION }
      |scalar federation__FieldSet
      |""".stripMargin

  val authoredFederationDirectives =
    s"""$baseFederationDirectives
       |directive @inaccessible on FIELD_DEFINITION | OBJECT | INTERFACE | UNION | ARGUMENT_DEFINITION | SCALAR | ENUM | ENUM_VALUE | INPUT_OBJECT | INPUT_FIELD_DEFINITION
       |directive @override(from: String!) on FIELD_DEFINITION
       |directive @interfaceObject on OBJECT
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
    s"""$baseFederationDirectives
       |directive @requires(fields: federation__FieldSet!) on FIELD_DEFINITION
       |directive @provides(fields: federation__FieldSet!) on FIELD_DEFINITION
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
    stubResponding(beforeResponse)((_, index) => Status.Ok -> responses(math.min(index, responses.size - 1)))

  def stubByRequest(response: GraphQLRequest => String): ZIO[Server with Ref[Int], Nothing, Stub] =
    stubResponding(ZIO.unit)((request, _) => Status.Ok -> response(request))

  def stubByRequestZIO(response: GraphQLRequest => UIO[String]): ZIO[Server with Ref[Int], Nothing, Stub] =
    stubRespondingZIO(ZIO.unit)((request, _) => response(request).map(Status.Ok -> _))

  def stubWithStatuses(responses: (Status, String)*): ZIO[Server with Ref[Int], Nothing, Stub] =
    stubResponding(ZIO.unit)((_, index) => responses(math.min(index, responses.size - 1)))

  private def stubResponding(
    beforeResponse: UIO[Unit]
  )(response: (GraphQLRequest, Int) => (Status, String)): ZIO[Server with Ref[Int], Nothing, Stub] =
    stubRespondingZIO(beforeResponse)((request, index) => ZIO.succeed(response(request, index)))

  private def stubRespondingZIO(
    beforeResponse: UIO[Unit]
  )(response: (GraphQLRequest, Int) => UIO[(Status, String)]): ZIO[Server with Ref[Int], Nothing, Stub] =
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
                      result  <- response(decoded, next)
                    } yield Response(
                      result._1,
                      Headers(Header.ContentType(MediaType("application", "graphql-response+json")).untyped),
                      Body.fromString(result._2)
                    )
                  }
      server   <- ZIO.service[Server]
      _        <- server.install(Routes(Method.POST / path -> handler))
      port     <- server.port
    } yield Stub(Uri.unsafeParse(s"http://127.0.0.1:$port/$path"), requests, headers)

  def postEndpoint(prefix: String)(handler: Request => UIO[Response]): ZIO[Server with Ref[Int], Nothing, Uri] =
    for {
      id     <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
      path    = s"$prefix-$id"
      server <- ZIO.service[Server]
      _      <- server.install(Routes(Method.POST / path -> Handler.fromFunctionZIO(handler)))
      port   <- server.port
    } yield Uri.unsafeParse(s"http://127.0.0.1:$port/$path")

  def streamingEndpoint(
    stream: ZStream[Any, Throwable, Byte],
    status: Status = Status.Ok,
    mediaType: String = "application/graphql-response+json"
  ): ZIO[Server with Ref[Int], Nothing, Uri] =
    postEndpoint("streaming")(_ =>
      ZIO.succeed(
        Response(
          status,
          Headers(Header.Custom("Content-Type", mediaType)),
          Body.fromStreamChunked(stream)
        )
      )
    )

  def tracked(
    body: String
  ): UIO[(ZStream[Any, Throwable, Byte], Ref[Int], Promise[Nothing, Unit])] =
    for {
      releases <- Ref.make(0)
      released <- Promise.make[Nothing, Unit]
      stream    = ZStream
                    .fromChunk(Chunk.fromArray(body.getBytes(StandardCharsets.UTF_8)))
                    .ensuring(releases.update(_ + 1) *> released.succeed(()).unit)
    } yield (stream, releases, released)

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

  def executionErrors(errors: List[CalibanError]): List[CalibanError.ExecutionError] =
    errors.collect { case error: CalibanError.ExecutionError => error }

  def listValues(value: Option[ResponseValue]): List[ResponseValue] =
    value.collect { case ResponseValue.ListValue(values) => values }.getOrElse(Nil)

  def onlyNested(value: Option[ResponseValue], child: String): Option[List[(String, ResponseValue)]] =
    value
      .flatMap(field(_, child))
      .collect { case ResponseValue.ListValue(ObjectValue(fields) :: Nil) => fields }

  def firstNestedObject(
    value: ResponseValue,
    root: String,
    child: String
  ): Option[List[(String, ResponseValue)]] =
    listValues(field(value, root)).headOption.flatMap(value => onlyNested(Some(value), child))

  def fieldNames(value: ResponseValue): List[String] =
    value match {
      case ObjectValue(fields) => fields.map(_._1)
      case _                   => Nil
    }

  def counter(name: String, label: String, value: String): UIO[Double] =
    Metric.counter(name).tagged(label, value).value.map(_.count)

  def gauge(name: String): UIO[Double] = Metric.gauge(name).value.map(_.value)

  def gauge(name: String, label: String, value: String): UIO[Double] =
    Metric.gauge(name).tagged(label, value).value.map(_.value)

  def histogram(name: String, labels: (String, String)*): UIO[Long] =
    labels
      .foldLeft(Metric.histogram(name, GatewayMetrics.durationBuckets)) { case (metric, (label, value)) =>
        metric.tagged(label, value)
      }
      .value
      .map(_.count)

  def localGraph(effect: UIO[String]) = {
    object LocalApi extends GenericSchema[Any] {
      import auto._
      final case class Query(value: UIO[String])
      implicit val querySchema: Schema[Any, Query] = gen
      val api                                      = graphQL(RootResolver(Query(effect)))
    }
    LocalApi.api
  }

  def localValueGraph(effect: UIO[String]) = {
    object LocalApi extends GenericSchema[Any] {
      import auto._
      final case class Query(localValue: UIO[String])
      implicit val querySchema: Schema[Any, Query] = gen
      val api                                      = graphQL(RootResolver(Query(effect)))
    }
    LocalApi.api
  }

  def recordEvents: UIO[(Ref[Vector[GatewayWrapper.Event]], GatewayWrapper[Any])] =
    Ref.make(Vector.empty[GatewayWrapper.Event]).map { events =>
      val wrapper = new GatewayWrapper[Any] {
        def wrap[R, E, A](event: GatewayWrapper.Event)(effect: ZIO[R, E, A])(
          result: Exit[E, A] => GatewayWrapper.Result
        )(implicit trace: Trace): ZIO[R, E, A] = events.update(_ :+ event) *> effect
      }
      (events, wrapper)
    }

  def validateRequest(schema: String, request: GraphQLRequest): IO[CalibanError, Unit] =
    for {
      schemaDocument <- ZIO.fromEither(Parser.parseQuery(schema))
      rootType       <- ZIO.fromEither(RemoteSchema.toRootType(schemaDocument))
      validationRoot  = Introspector.withIntrospection(rootType)
      document       <- RequestPreparation.parse(request.query.getOrElse(""))
      variables      <- RequestPreparation.coerceVariables(document, request, validationRoot)
      _              <- RequestPreparation.prepareParsed(
                          request,
                          document,
                          variables,
                          validationRoot,
                          skipValidation = false,
                          validations = Some(Validator.AllValidations)
                        )
    } yield ()
}

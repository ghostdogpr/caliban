package caliban.gateway

import caliban.ResponseValue
import caliban.ResponseValue.ObjectValue
import caliban.execution.RequestPreparation
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.composition.{ IntrospectionClient, SupergraphAcquisition }
import caliban.introspection.Introspector
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import caliban.schema.{ GenericSchema, Schema }
import caliban.tools.RemoteSchema
import caliban.validation.Validator
import caliban.{ graphQL, CalibanError, GraphQL, GraphQLRequest, GraphQLResponse, RootResolver, Value }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, writeToString }
import zio._
import zio.http._
import zio.http.netty.NettyConfig
import zio.metrics.Metric
import zio.stream.ZStream
import zio.test.TestClock

import java.nio.charset.StandardCharsets

private[gateway] object GatewayTestSupport {

  def buildDiagnostics[A](exit: Exit[GatewayBuildError, A]): List[String] =
    exit.causeOption.flatMap(_.failureOption).fold(List.empty[String])(_.diagnostics)

  def compositionDiagnostics[R](gateway: Gateway[R]): URIO[Scope, List[String]] =
    gateway.interpreter.exit.map(buildDiagnostics)

  def prepareSubgraph(
    subgraph: Subgraph[Any],
    document: Document,
    federation: Boolean
  ): Either[List[String], PreparedSubgraph] =
    for {
      // Normalizing rather than only building a root type is necessary: it folds `extend schema`
      // into the schema definition, which is where composition reads `@link` from. A projection
      // declares its schema outright, so only the checked-in originals depend on the merge.
      normalized <- RemoteSchema
                      .normalize(document, promoteOrphans = federation)
                      .left
                      .map(error => List(s"[${subgraph.name}] ${error.getMessage}"))
      prepared   <- Gateway
                      .prepareSubgraph(subgraph, normalized, document, federation)
                      .left
                      .map(SubgraphError(subgraph.name, _).diagnostics)
    } yield prepared

  def introspectionResponse(api: GraphQL[Any]): UIO[String] =
    ZIO.fromEither(api.interpreterEither).orDie.flatMap { interpreter =>
      interpreter
        .execute(IntrospectionClient.Query)
        .map(writeToString(_))
    }

  def serviceResponse(sdl: String): String =
    writeToString(
      GraphQLResponse[Any](
        ObjectValue(List("_service" -> ObjectValue(List("sdl" -> Value.StringValue(sdl))))),
        Nil
      )
    )

  def supergraphResource(name: String): UIO[String] =
    ZIO
      .scoped(ZIO.fromAutoCloseable(ZIO.attempt(scala.io.Source.fromResource(s"supergraph/$name"))).map(_.mkString))
      .orDie

  final case class Stub(
    endpoint: URL,
    requests: Ref[Vector[GraphQLRequest]],
    headers: Ref[Vector[Headers]],
    combined: Ref[Vector[GraphQLRequest]]
  )

  val okResponse               = """{"data":{"value":"ok"}}"""
  val invalidResponse          = """{"unexpected":true}"""
  val unreachableEndpoint: URL = url"http://127.0.0.1:1/graphql"

  val minimalSupergraphSdl: String =
    """schema
      |  @link(url: "https://specs.apollo.dev/link/v1.0")
      |  @link(url: "https://specs.apollo.dev/join/v0.5", for: EXECUTION)
      |{
      |  query: Query
      |}
      |
      |enum join__Graph {
      |  A @join__graph(name: "a", url: "http://a/graphql")
      |}
      |
      |type Query @join__type(graph: A) { hello: String }
      |""".stripMargin

  def queryFields(document: Document): List[String] =
    document.objectTypeDefinitions.filter(_.name == "Query").flatMap(_.fields.map(_.name))

  val httpClient: ZLayer[Any, Throwable, GatewayHttpClient] = ZLayer.scoped(GatewayHttpClient.make)

  def acquisitionLoader(source: Supergraph.Source): URIO[GatewayHttpClient, SupergraphAcquisition.Loader] =
    ZIO.serviceWithZIO[GatewayHttpClient](client => SupergraphAcquisition.make(source, Some(client)))

  def localAcquisitionLoader(source: Supergraph.Source): UIO[SupergraphAcquisition.Loader] =
    SupergraphAcquisition.make(source, None)

  /** Fails with the acquisition error, or dies describing what happened instead. */
  def acquisitionFailure[A](exit: Exit[SupergraphAcquisitionError, A]): UIO[SupergraphAcquisitionError] =
    exit match {
      case Exit.Failure(cause) =>
        ZIO
          .fromOption(cause.failureOption)
          .orDieWith(_ => new AssertionError(s"expected a typed failure, got: ${cause.prettyPrint}"))
      case Exit.Success(value) => ZIO.die(new AssertionError(s"expected a failure, got: $value"))
    }

  implicit final class TestGatewayOps[R](private val gateway: Gateway[R]) extends AnyVal {

    /** One-second polls with no jitter; sources without a published floor accept any interval. */
    def reloadableForTest(implicit trace: Trace): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] =
      reloadableEvery(1.second)

    def reloadableEvery(interval: Duration)(implicit
      trace: Trace
    ): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] =
      gateway.withConfig(_.withReloadPollInterval(interval).withReloadJitter(0.0)).reloadable
  }

  // The next poll timer is installed only after the current refresh and retirement finish.
  def awaitPoll(interval: Duration = 1.second): UIO[Unit] =
    Clock.instant.flatMap(now => TestClock.sleeps.repeatUntil(_.contains(now.plus(interval)))).unit

  def poll(runtime: ReloadableGatewayInterpreter[_], interval: Duration = 1.second): UIO[Option[String]] =
    TestClock.adjust(interval) *> awaitPoll(interval) *> runtime.lastReloadFailure

  def nextAnswer[A](remaining: Ref[List[A]], fallback: A): UIO[A] =
    remaining.modify {
      case only :: Nil  => only     -> (only :: Nil)
      case head :: rest => head     -> rest
      case Nil          => fallback -> Nil
    }

  // -----------------------------------------------------------------------------------------------
  // Apollo uplink protocol bodies, shared by the loader spec and the gateway spec
  // -----------------------------------------------------------------------------------------------

  private def routerConfig(fields: (String, ResponseValue)*): String =
    writeToString(GraphQLResponse[Any](ObjectValue(List("routerConfig" -> ObjectValue(fields.toList))), Nil))

  def uplinkConfigResult(id: String, sdl: String, minDelaySeconds: Double = 10.0): String =
    routerConfig(
      "__typename"      -> Value.StringValue("RouterConfigResult"),
      "id"              -> Value.StringValue(id),
      "supergraphSDL"   -> Value.StringValue(sdl),
      "minDelaySeconds" -> Value.FloatValue(minDelaySeconds)
    )

  def uplinkUnchanged(id: String, minDelaySeconds: Double = 10.0): String =
    routerConfig(
      "__typename"      -> Value.StringValue("Unchanged"),
      "id"              -> Value.StringValue(id),
      "minDelaySeconds" -> Value.FloatValue(minDelaySeconds)
    )

  def uplinkFetchError(code: String, message: String): String =
    routerConfig(
      "__typename" -> Value.StringValue("FetchError"),
      "code"       -> Value.StringValue(code),
      "message"    -> Value.StringValue(message)
    )

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

  def productsAndReviews(
    products: Stub,
    reviews: Stub,
    productsSchema: String = productsFederationSchema,
    reviewsSchema: String = reviewsFederationSchema
  ): Gateway[Any] =
    Gateway.compose(
      Subgraph.federation("products", products.endpoint, productsSchema),
      Subgraph.federation("reviews", reviews.endpoint, reviewsSchema)
    )

  val productRootSchema =
    s"""
       |${federationSchemaPreamble("@key")}
       |type Query { product: Product }
       |type Product @key(fields: "id") { id: ID! }
       |""".stripMargin

  val productRootResponse =
    """{"data":{"product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""

  val productPriceSchema =
    s"""
       |${federationSchemaPreamble("@key", "@external")}
       |type Product @key(fields: "id") { id: ID! @external price: Int! }
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
      combined <- Ref.make(Vector.empty[GraphQLRequest])
      index    <- Ref.make(0)
      id       <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
      path      = s"graphql-$id"
      handler   = Handler.fromFunctionZIO[Request] { request =>
                    for {
                      bytes   <- request.body.asArray.orDie
                      decoded <- ZIO.attempt(readFromArray[GraphQLRequest](bytes)).orDie
                      parts    = splitCombined(decoded)
                      _       <- combined.update(_ :+ decoded).when(parts.nonEmpty)
                      _       <- requests.update(_ ++ (if (parts.isEmpty) Vector(decoded) else parts.map(_._2)))
                      _       <- headers.update(_ :+ request.headers)
                      _       <- beforeResponse
                      result  <- if (parts.isEmpty) index.getAndUpdate(_ + 1).flatMap(response(decoded, _))
                                 else
                                   ZIO
                                     .foreach(parts) { case (alias, part) =>
                                       index.getAndUpdate(_ + 1).flatMap(response(part, _)).map(alias -> _)
                                     }
                                     .map(mergeCombined)
                    } yield Response(
                      result._1,
                      Headers(Header.ContentType(MediaType("application", "graphql-response+json")).untyped),
                      Body.fromString(result._2)
                    )
                  }
      server   <- ZIO.service[Server]
      _        <- server.install(Routes(Method.POST / path -> handler))
      port     <- server.port
    } yield Stub(url"http://127.0.0.1:$port/$path", requests, headers, combined)

  private val CombinedAlias    = "_caliban_gateway_entities_"
  private val CombinedVariable = "_caliban_gateway_representations_"

  private def splitCombined(request: GraphQLRequest): List[(String, GraphQLRequest)] = {
    val parts = request.query.getOrElse("").split(CombinedAlias).toList.drop(1)
    parts.zipWithIndex.map { case (part, position) =>
      val slot     = part.takeWhile(_.isDigit)
      val trimmed  = part.drop(slot.length + 1).trim
      val body     = if (position == parts.size - 1) trimmed.dropRight(1) else trimmed
      val query    = body.replace("$" + CombinedVariable + slot, "$representations")
      val variable = CombinedVariable + slot
      (CombinedAlias + slot) -> GraphQLRequest(
        query = Some(s"query __GatewayEntity($$representations:[_Any!]!){$query}"),
        operationName = Some("__GatewayEntity"),
        variables = request.variables.map(values =>
          Map("representations" -> values.getOrElse(variable, caliban.InputValue.ListValue(Nil)))
        ),
        extensions = request.extensions
      )
    }
  }

  private def mergeCombined(results: List[(String, (Status, String))]): (Status, String) =
    results.find(_._2._1 != Status.Ok).map(_._2).getOrElse {
      val codec                 = caliban.interop.jsoniter.ValueJsoniter.responseValueCodec
      val renamed               = results.map { case (alias, (_, body)) =>
        readFromArray[ResponseValue](body.getBytes(StandardCharsets.UTF_8))(codec) match {
          case ObjectValue(fields) =>
            val data   = fields.collectFirst { case ("data", ObjectValue(dataFields)) =>
              dataFields.collect { case ("_entities", entities) => alias -> entities }
            }.getOrElse(Nil)
            val errors = fields.collectFirst { case ("errors", ResponseValue.ListValue(errors)) =>
              errors.map {
                case ObjectValue(errorFields) =>
                  ObjectValue(errorFields.map {
                    case ("path", ResponseValue.ListValue(Value.StringValue("_entities") :: rest)) =>
                      "path" -> ResponseValue.ListValue(Value.StringValue(alias) :: rest)
                    case other                                                                     => other
                  })
                case other                    => other
              }
            }.getOrElse(Nil)
            (data, errors)
          case _                   => (Nil, Nil)
        }
      }
      val data                  = ObjectValue(renamed.flatMap(_._1))
      val errors                = renamed.flatMap(_._2)
      val merged: ResponseValue = ObjectValue(
        ("data" -> data) :: (if (errors.isEmpty) Nil else List("errors" -> ResponseValue.ListValue(errors)))
      )
      Status.Ok -> writeToString(merged)(codec)
    }

  def postEndpoint(prefix: String)(handler: Request => UIO[Response]): ZIO[Server with Ref[Int], Nothing, URL] =
    routesEndpoint(prefix)(path => Routes(Method.POST / path -> Handler.fromFunctionZIO(handler)))

  def getEndpoint(prefix: String)(handler: Request => UIO[Response]): ZIO[Server with Ref[Int], Nothing, URL] =
    routesEndpoint(prefix)(path => Routes(Method.GET / path -> Handler.fromFunctionZIO(handler)))

  def routesEndpoint(prefix: String)(routes: String => Routes[Any, Response]): ZIO[Server with Ref[Int], Nothing, URL] =
    for {
      id     <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
      path    = s"$prefix-$id"
      server <- ZIO.service[Server]
      _      <- server.install(routes(path))
      port   <- server.port
    } yield url"http://127.0.0.1:$port/$path"

  def streamingEndpoint(
    stream: ZStream[Any, Throwable, Byte],
    status: Status = Status.Ok,
    mediaType: String = "application/graphql-response+json"
  ): ZIO[Server with Ref[Int], Nothing, URL] =
    postEndpoint("streaming")(_ =>
      ZIO.succeed(
        Response(
          status,
          Headers(Header.Custom("Content-Type", mediaType)),
          Body.fromStreamChunked(stream)
        )
      )
    )

  def sseEndpoint(body: String): ZIO[Server with Ref[Int], Nothing, URL] =
    streamingEndpoint(ZStream.fromIterable(body.getBytes(StandardCharsets.UTF_8)), mediaType = "text/event-stream")

  def sseBody(events: Int*): String =
    events.map(event => s"""event: next\ndata: {"data":{"event":$event}}\n\n""").mkString + "event: complete\n\n"

  val sseConfig: RemoteGraphQLConfig[Any] =
    RemoteGraphQLConfig.default.withSubscription(RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse()))

  val subscriptionSchema = "type Query { value: String } type Subscription { event: Int }"

  def headersAfterEveryCaller(callers: Int, headerRuns: Ref[Int], ready: Promise[Nothing, Unit]): UIO[List[Header]] =
    headerRuns
      .updateAndGet(_ + 1)
      .flatMap(count => ready.succeed(()).unit.when(count == callers)) *>
      ready.await.as(Nil)

  def renderedHeaderValues(headers: Headers, name: String): List[String] =
    headers.iterator.filter(_.headerName.equalsIgnoreCase(name)).map(_.renderedValue).toList

  def tracked(body: String): UIO[(ZStream[Any, Throwable, Byte], Ref[Int], Promise[Nothing, Unit])] =
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

  def firstNestedObject(value: ResponseValue, root: String, child: String): Option[List[(String, ResponseValue)]] =
    listValues(field(value, root)).headOption.flatMap(value => onlyNested(Some(value), child))

  def introspectedNames(list: Option[ResponseValue]): Option[List[ResponseValue]] =
    list.collect { case ResponseValue.ListValue(values) => values.flatMap(field(_, "name")) }

  def introspectedNameStrings(list: Option[ResponseValue]): Option[List[String]] =
    introspectedNames(list).map(_.collect { case Value.StringValue(name) => name })

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

  def localGateway(effect: UIO[String]): Gateway[Any] =
    Gateway.compose(Subgraph.local("local", localGraph(effect)))

  def localValueGateway(effect: UIO[String]): Gateway[Any] =
    Gateway.compose(Subgraph.local("local", localValueGraph(effect)))

  def localValueGraph(effect: UIO[String]) = {
    object LocalApi extends GenericSchema[Any] {
      import auto._
      final case class Query(localValue: UIO[String])
      implicit val querySchema: Schema[Any, Query] = gen
      val api                                      = graphQL(RootResolver(Query(effect)))
    }
    LocalApi.api
  }

  def subscriptionGraph(events: ZStream[Any, Throwable, Int]): GraphQL[Any] = {
    object SubscriptionApi extends GenericSchema[Any] {
      import auto._
      final case class Query(value: String)
      final case class Subscription(event: ZStream[Any, Throwable, Int])
      implicit val queryType: Schema[Any, Query]               = gen
      implicit val subscriptionType: Schema[Any, Subscription] = gen
      val api                                                  = graphQL(
        RootResolver(
          queryResolver = Some(Query("ok")),
          mutationResolver = Option.empty[Unit],
          subscriptionResolver = Some(Subscription(events))
        )
      )
    }
    SubscriptionApi.api
  }

  /**
   * Attaches the same handler to every phase that reports a [[PhaseHooks.Result]].
   */
  private trait PhaseRecorder {
    def handler[Ev <: PhaseHooks.Event]: PhaseHandler[Any, Ev, Nothing, PhaseHooks.Result]
  }

  private def everyResultPhase(recorder: PhaseRecorder): PhaseHooks[Any] =
    PhaseHooks.subscriptionSetup(recorder.handler) ++
      PhaseHooks.subscriptionEvent(recorder.handler) ++
      PhaseHooks.subscriptionTerminated(recorder.handler) ++
      PhaseHooks.subscriptionAdmission(recorder.handler) ++
      PhaseHooks.subscriptionOverflow(recorder.handler) ++
      PhaseHooks.request(recorder.handler) ++
      PhaseHooks.routing(recorder.handler) ++
      PhaseHooks.subgraphCall(recorder.handler) ++
      PhaseHooks.attempt(recorder.handler) ++
      PhaseHooks.retry(recorder.handler) ++
      PhaseHooks.completion(recorder.handler) ++
      PhaseHooks.cacheAccess(recorder.handler) ++
      PhaseHooks.admission(recorder.handler)

  /**
   * Records every lifecycle event the gateway reaches, in order.
   *
   * The hooks are an ordinary [[PhaseHooks]] value, so a spec that needs targeted behaviour composes it rather than
   * rebuilding the recorder: `gateway.withPhaseHooks(hooks ++ PhaseHooks.SubscriptionSetup(handler))`.
   */
  def recordEvents: UIO[(Ref[Vector[PhaseHooks.Event]], PhaseHooks[Any])] =
    Ref.make(Vector.empty[PhaseHooks.Event]).map { events =>
      val hooks = everyResultPhase(new PhaseRecorder {
        def handler[Ev <: PhaseHooks.Event]: PhaseHandler[Any, Ev, Nothing, PhaseHooks.Result] =
          PhaseHandler.incomingDiscard((event: Ev) => events.update(_ :+ event))
      })

      (events, hooks)
    }

  /**
   * Records every lifecycle event on entry, and on exit the [[PhaseHooks.Result]] each one completed with,
   * paired with the event it belongs to.
   */
  def recordEventsAndResults: UIO[
    (
      Ref[Vector[PhaseHooks.Event]],
      Ref[Vector[(PhaseHooks.Event, PhaseHooks.Result)]],
      PhaseHooks[Any]
    )
  ] =
    for {
      events  <- Ref.make(Vector.empty[PhaseHooks.Event])
      results <- Ref.make(Vector.empty[(PhaseHooks.Event, PhaseHooks.Result)])
    } yield {
      val hooks = everyResultPhase(new PhaseRecorder {
        def handler[Ev <: PhaseHooks.Event]: PhaseHandler[Any, Ev, Nothing, PhaseHooks.Result] =
          PhaseHandler((event: Ev) => events.update(_ :+ event).as((event, ())))((event, _, result) =>
            results.update(_ :+ (event -> result))
          )
      })

      (events, results, hooks)
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

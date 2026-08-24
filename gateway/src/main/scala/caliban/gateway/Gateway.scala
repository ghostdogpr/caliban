package caliban.gateway

import caliban.gateway.Subgraph.Source
import caliban.gateway.internal._
import caliban.introspection.Introspector
import caliban.parsing.adt.Definition.TypeSystemDefinition.SchemaDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension.ObjectTypeExtension
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt.Document
import caliban.schema.RootType
import caliban.tools.RemoteSchema
import sttp.client4.httpclient.zio.{ HttpClientZioBackend, SttpClient }
import zio._

/**
 * An immutable description of a gateway.
 *
 * A description is reusable: each call to [[build]] creates a new [[GatewayRuntime]] whose resources
 * are owned by the surrounding [[zio.Scope]]. `R` describes the environment required when executing
 * requests; constructing and building the description does not require that environment.
 */
final class Gateway[-R] private[gateway] (
  private val subgraphs: List[Subgraph[R]],
  private val resolver: Option[OperationResolver[R]],
  private val policy: Option[OperationPolicy[R]],
  private val config: GatewayConfig,
  private val wrapper: GatewayWrapper[R]
) {

  /**
   * Builds an executable runtime within the current scope.
   */
  def build(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayRuntime[R]] =
    Gateway.build(subgraphs, resolver, policy, config, wrapper)

  /**
   * Transforms the finite operation and admission limits used by each built runtime.
   */
  def withConfig(configure: GatewayConfig => GatewayConfig): Gateway[R] =
    new Gateway(subgraphs, resolver, policy, configure(config), wrapper)

  /**
   * Resolves canonical GraphQL text before parsing and validation.
   */
  def withOperationResolver[R1](value: OperationResolver[R1]): Gateway[R with R1] =
    new Gateway(subgraphs, Some(value), policy, config, wrapper)

  /**
   * Allows or rejects operations after validation and variable coercion.
   */
  def withOperationPolicy[R1](value: OperationPolicy[R1]): Gateway[R with R1] =
    new Gateway(subgraphs, resolver, Some(value), config, wrapper)

  /**
   * Adds an integration around the gateway lifecycle.
   */
  def @@[R1](value: GatewayWrapper[R1]): Gateway[R with R1] =
    new Gateway(subgraphs, resolver, policy, config, wrapper |+| value)
}

object Gateway {

  /**
   * Creates a reusable gateway description from one or more subgraphs.
   */
  def compose[R](first: Subgraph[R], rest: Subgraph[R]*): Gateway[R] =
    new Gateway[R](
      first :: rest.toList,
      None,
      None,
      GatewayConfig.default,
      GatewayWrapper.empty
    )

  private def build[R](
    subgraphs: List[Subgraph[R]],
    resolver: Option[OperationResolver[R]],
    policy: Option[OperationPolicy[R]],
    config: GatewayConfig,
    wrapper: GatewayWrapper[R]
  )(implicit
    trace: Trace
  ): ZIO[Scope, GatewayBuildError, GatewayRuntime[R]] =
    ZIO.fail(GatewayBuildError(config.diagnostics)).when(config.diagnostics.nonEmpty) *>
      ZIO.scopeWith { parent =>
        ZIO.uninterruptibleMask { restore =>
          for {
            child   <- parent.fork
            runtime <- restore(child.extend(buildRuntime(subgraphs, resolver, policy, config, wrapper))).onExit {
                         case failure @ Exit.Failure(_) => child.close(failure)
                         case Exit.Success(_)           => ZIO.unit
                       }
          } yield runtime
        }
      }

  private def buildRuntime[R](
    subgraphs: List[Subgraph[R]],
    resolver: Option[OperationResolver[R]],
    policy: Option[OperationPolicy[R]],
    config: GatewayConfig,
    wrapper: GatewayWrapper[R]
  )(implicit
    trace: Trace
  ): ZIO[Scope, GatewayBuildError, GatewayRuntime[R]] =
    for {
      backend     <-
        if (
          subgraphs.exists(_.source match {
            case _: Source.Remote[_] => true
            case _                   => false
          })
        )
          HttpClientZioBackend
            .scoped()
            .map(Some(_))
            .mapError(error =>
              GatewayBuildError(s"Unable to initialize the remote GraphQL transport: ${error.getMessage}")
            )
        else ZIO.none
      loaded      <- ZIO.foreachPar(subgraphs)(
                       load(_, backend, config.maxConcurrentLocalCalls, config.remoteErrorDisclosure, wrapper).either
                     )
      partitioned  = loaded.foldRight((List.empty[List[String]], List.empty[LoadedSubgraph[R]])) {
                       case (Left(errors), (failures, successes)) => (errors :: failures, successes)
                       case (Right(value), (failures, successes)) => (failures, value :: successes)
                     }
      failures     = nameDiagnostics(subgraphs) ::: partitioned._1.flatten
      _           <- ZIO.fail(GatewayBuildError(failures.distinct.sorted)).when(failures.nonEmpty)
      successes    = partitioned._2
      graph       <- ZIO
                       .fromEither(SchemaComposition.compose(successes.map(_.contribution)))
                       .mapError(errors => GatewayBuildError(errors.distinct.sorted))
      _           <- ZIO
                       .fail(GatewayBuildError(graph.securityPolicyDiagnostics))
                       .when(policy.isEmpty && graph.hasSecurityRequirements)
      rawSources   = successes.map(value => value.contribution.name -> value.source).toMap
      sourceLimits = successes.map(value => value.contribution.name -> value.maxConcurrentCalls).toMap
      control     <- RuntimeControl.make(
                       config.maxConcurrentRequests,
                       sourceLimits,
                       config.requestTimeout,
                       config.drainTimeout
                     )
      sources      = rawSources.map { case (name, source) =>
                       name -> new ObservedGraphQLSource(name, control.source(name, source, wrapper), wrapper)
                     }
      requestRoot  = Introspector.withIntrospection(graph.rootType)
      operations  <- OperationPreparation.make(
                       requestRoot,
                       new OperationPlanner(
                         graph,
                         sources.size,
                         OperationPlanner.Limits(
                           config.maxPlanningCandidates,
                           config.maxPlanningExpansions,
                           config.planningTimeout
                         )
                       ),
                       new OperationHooks(graph.securityRequirements, resolver, policy),
                       config,
                       wrapper
                     )
    } yield new GatewayRuntimeImpl[R](graph, sources, operations, control, wrapper)

  private def load[R](
    subgraph: Subgraph[R],
    backend: Option[SttpClient],
    localCallLimit: Int,
    remoteErrorDisclosure: RemoteGraphQLConfig.ErrorDisclosure,
    wrapper: GatewayWrapper[R]
  )(implicit
    trace: Trace
  ): ZIO[Scope, List[String], LoadedSubgraph[R]] =
    subgraph.source match {
      case Source.Remote(endpoint, schema, federation, config) =>
        val policyDiagnostics = config
          .diagnostics(schema == SchemaInput.Acquired)
          .map(message => s"[${subgraph.name}] $message")
        for {
          _              <- ZIO.fail(policyDiagnostics).when(policyDiagnostics.nonEmpty)
          client         <- ZIO
                              .fromOption(backend)
                              .orElseFail(List(s"[${subgraph.name}] Remote GraphQL transport is unavailable."))
          document       <- RemoteSchemaAcquisition
                              .document(schema, endpoint, federation, config.acquisition, client)
                              .mapError(error => List(s"[${subgraph.name}] $error"))
          rootDocument    = ensureFederationTransportQuery(document, federation)
          sourceRootType <- toRootType(subgraph.name, rootDocument, promoteOrphans = federation).mapError(_ :: Nil)
          contribution   <- ZIO.fromEither(
                              prepareContribution(subgraph, sourceRootType, rootDocument, document, federation)
                            )
          source         <- RemoteGraphQLSource.make(
                              subgraph.name,
                              endpoint,
                              client,
                              config.withDefaultErrorDisclosure(remoteErrorDisclosure),
                              wrapper
                            )
        } yield LoadedSubgraph(contribution, source, config.execution.maxConcurrentCalls)
      case Source.Local(graph)                                 =>
        val document   = graph.toDocument
        val federation = SchemaComposition.isFederation(document)
        for {
          sourceRootType <- toRootType(subgraph.name, document).mapError(_ :: Nil)
          contribution   <- ZIO.fromEither(
                              prepareContribution(subgraph, sourceRootType, document, document, federation)
                            )
          interpreter    <- ZIO
                              .fromEither(graph.interpreterEither)
                              .mapError(error => List(s"[${subgraph.name}] ${error.getMessage}"))
        } yield LoadedSubgraph(contribution, new LocalGraphQLSource(interpreter), localCallLimit)
    }

  private final case class LoadedSubgraph[-R](
    contribution: SchemaContribution,
    source: GraphQLSource[R],
    maxConcurrentCalls: Int
  )

  private def toRootType(
    name: String,
    document: Document,
    promoteOrphans: Boolean = false
  ): IO[String, RootType] =
    ZIO
      .fromEither(RemoteSchema.toRootType(document, promoteOrphans))
      .mapError(error => s"[$name] ${error.getMessage}")

  private[gateway] def prepareContribution[R](
    subgraph: Subgraph[R],
    sourceRootType: RootType,
    rootDocument: Document,
    document: Document,
    federation: Boolean
  ): Either[List[String], SchemaContribution] =
    for {
      mapping  <- SchemaCoordinateMapping.compile(
                    subgraph.name,
                    sourceRootType,
                    document,
                    federation,
                    subgraph.transformations
                  )
      rootType <-
        if (mapping.nonEmpty)
          RemoteSchema
            .toRootType(mapping.transform(rootDocument), promoteOrphans = federation)
            .left
            .map(error => List(s"[${subgraph.name}] ${error.getMessage}"))
        else Right(sourceRootType)
    } yield SchemaContribution(
      subgraph.name,
      rootType,
      mapping.transform(document),
      federation,
      subgraph.lookups.map(mapping.transform),
      mapping
    )

  private def ensureFederationTransportQuery(document: Document, federation: Boolean): Document = {
    val schemaExtensions     = document.typeExtensions.collect { case extension: SchemaExtension => extension }
    val hasDeclaredQuery     =
      document.schemaDefinition.flatMap(_.query).nonEmpty || schemaExtensions.exists(_.query.nonEmpty)
    val hasConventionalQuery =
      document.schemaDefinition.isEmpty && (
        document.objectTypeDefinitions.exists(_.name == "Query") ||
          document.typeExtensions.exists {
            case extension: ObjectTypeExtension => extension.name == "Query"
            case _                              => false
          }
      )

    if (!federation || hasDeclaredQuery || hasConventionalQuery) document
    else {
      val names    = document.typeDefinitions.iterator.map(_.name).toSet ++ document.typeExtensions.collect {
        case extension: ObjectTypeExtension => extension.name
      }
      val rootName = Iterator
        .from(1)
        .map(index => if (index == 1) "CalibanGatewayFederationQuery" else s"CalibanGatewayFederationQuery$index")
        .find(name => !names.contains(name))
        .get
      val root     = ObjectTypeDefinition(
        None,
        rootName,
        Nil,
        Nil,
        List(FieldDefinition(None, "_service", Nil, NamedType("_Service", nonNull = true), Nil))
      )
      val service  =
        if (names.contains("_Service")) Nil
        else
          List(
            ObjectTypeDefinition(
              None,
              "_Service",
              Nil,
              Nil,
              List(FieldDefinition(None, "sdl", Nil, NamedType("String", nonNull = true), Nil))
            )
          )
      val schema   = document.schemaDefinition match {
        case Some(_) =>
          document.definitions.map {
            case definition: SchemaDefinition if definition.query.isEmpty => definition.copy(query = Some(rootName))
            case definition                                               => definition
          }
        case None    =>
          SchemaDefinition(
            Nil,
            Some(rootName),
            if (names.contains("Mutation")) Some("Mutation") else None,
            if (names.contains("Subscription")) Some("Subscription") else None,
            None
          ) :: document.definitions
      }

      Document(schema ::: root :: service, document.sourceMapper)
    }
  }

  private def nameDiagnostics[R](subgraphs: List[Subgraph[R]]): List[String] = {
    val blank     = subgraphs.collect {
      case subgraph if subgraph.name.trim.isEmpty => "[subgraph] Name must not be empty."
    }
    val duplicate = subgraphs
      .groupBy(_.name)
      .collect { case (name, values) if values.size > 1 => s"[subgraph '$name'] Name is used more than once." }
      .toList

    (blank ::: duplicate).sorted
  }
}

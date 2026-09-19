package caliban.gateway

import caliban.gateway.Gateway.{ decomposeSupergraph, Origin }
import caliban.gateway.GatewayBuildError._
import caliban.gateway.internal._
import caliban.gateway.internal.composition._
import caliban.gateway.internal.execution._
import caliban.gateway.internal.planning.{ CandidateSearch, OperationPlanner }
import caliban.gateway.Subgraph.Source
import caliban.gateway.SubgraphBuildError._
import caliban.introspection.Introspector
import caliban.parsing.adt.Definition.TypeSystemDefinition.SchemaDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension.ObjectTypeExtension
import caliban.parsing.adt.Document
import caliban.parsing.adt.Type.NamedType
import caliban.tools.RemoteSchema
import zio._

/**
 * An immutable description of a gateway.
 *
 * A description is reusable: each call to [[interpreter]] creates a new [[GatewayInterpreter]] whose resources
 * are owned by the surrounding [[zio.Scope]]. `R` describes the environment required when executing
 * requests; constructing and building the description does not require that environment.
 */
final class Gateway[-R] private[gateway] (
  private val origin: Gateway.Origin[R],
  private val resolver: Option[OperationResolver[R]],
  private val policy: Option[OperationPolicy[R]],
  private val config: GatewayConfig,
  private val hooks: PhaseHooks[R]
) {

  /**
   * Builds an executable interpreter within the current scope.
   */
  def interpreter(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayInterpreter[R]] = build

  /**
   * Builds a stable interpreter that polls acquired remote schemas and replaces changed generations.
   * Pinned schemas and local graphs remain fixed. Admission limits apply separately to each generation.
   */
  def reloadable(implicit trace: Trace): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] =
    validate(config.diagnostics ::: reloadDiagnostics) *>
      buildInChildScope(
        openHttpClient.flatMap { http =>
          val acquire = origin match {
            case Origin.Composed(subgraphs)        => Exit.succeed(acquireSubgraphSnapshot(subgraphs, http))
            case Origin.FromSupergraph(supergraph) =>
              SupergraphAcquisition
                .make(supergraph.source, http)
                .map(acquireSupergraphSnapshot(supergraph, _))
          }

          acquire.flatMap(
            ReloadableGatewayInterpreterImpl.make(
              _,
              config.reloadPollInterval,
              config.reloadJitter,
              config.drainTimeout,
              http
            )
          )
        }
      )

  /**
   * Transforms the finite operation and admission limits used by each built interpreter.
   */
  def withConfig(configure: GatewayConfig => GatewayConfig): Gateway[R] =
    new Gateway(origin, resolver, policy, configure(config), hooks)

  /**
   * Resolves canonical GraphQL text before parsing and validation.
   */
  def withOperationResolver[R1 <: R](value: OperationResolver[R1]): Gateway[R1] =
    new Gateway(origin, Some(value), policy, config, hooks)

  /**
   * Allows or rejects operations after validation and variable coercion.
   */
  def withOperationPolicy[R1 <: R](value: OperationPolicy[R1]): Gateway[R1] =
    new Gateway(origin, resolver, Some(value), config, hooks)

  /**
   * Adds phase hooks to the gateway lifecycle. Hooks accumulate: each call appends to the hooks already attached,
   * so several independent integrations can be layered onto the same description.
   */
  def withPhaseHooks[R1 <: R](hooks: PhaseHooks[R1]): Gateway[R1] =
    new Gateway(origin, resolver, policy, config, this.hooks ++ hooks)

  /**
   * Symbolic version of `withPhaseHooks`.
   */
  def @@[R1 <: R](hooks: PhaseHooks[R1]): Gateway[R1] =
    withPhaseHooks(hooks)

  private[gateway] def build(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayInterpreterImpl[R]] = {
    val originDiagnostics = origin match {
      case Origin.Composed(subgraphs) => Gateway.nameDiagnostics(subgraphs)
      // Subgraph names come from the supergraph's graph registry, which the decomposition validates.
      case Origin.FromSupergraph(_)   => Nil
    }

    validate(config.diagnostics ::: originDiagnostics) *>
      buildInChildScope(openHttpClient.flatMap(buildInterpreter))
  }

  private def reloadDiagnostics: List[String] =
    origin match {
      case Origin.FromSupergraph(supergraph) =>
        val uplink = supergraph.source match {
          case Supergraph.Source.Uplink(uplinkConfig) =>
            // Uplink asks clients not to poll faster than the `minDelaySeconds` it answers with, and
            // Apollo's published floor is ten seconds. Jitter is what reaches the wire, so the fastest
            // poll the configuration permits is what has to clear the floor.
            uplinkConfig.diagnostics ::: check(
              config.minimumReloadPollInterval >= Gateway.UplinkMinPollInterval,
              "Supergraph uplink polling requires a reload poll interval of at least ten seconds."
            )
          case _                                      => Nil
        }

        check(supergraph.source.refreshable, "Gateway reload from supergraph requires a remote source.") ::: uplink
      case Origin.Composed(subgraphs)        =>
        val acquired = subgraphs.exists(_.source match {
          case Source.Remote(_, SchemaInput.Acquired, _, _) => true
          case _                                            => false
        })
        Gateway.nameDiagnostics(subgraphs) :::
          check(acquired, "Gateway reload requires at least one acquired remote schema.")
    }

  private def validate(diagnostics: List[String])(implicit trace: Trace): IO[GatewayBuildError, Unit] =
    ZIO.fail(GatewayBuildError.InvalidConfiguration(diagnostics)).when(diagnostics.nonEmpty).unit

  private def openHttpClient(implicit trace: Trace): ZIO[Scope, GatewayBuildError, Option[GatewayHttpClient]] =
    if (origin.hasRemote) GatewayHttpClient.make.asSome.mapError(TransportInitializationFailed(_))
    else ZIO.none

  private[gateway] def buildInterpreter(http: Option[GatewayHttpClient])(implicit
    trace: Trace
  ): ZIO[Scope, GatewayBuildError, GatewayInterpreterImpl[R]] =
    for {
      subgraphs  <- origin match {
                      case Origin.Composed(subgraphs)        => Exit.succeed(subgraphs)
                      case Origin.FromSupergraph(supergraph) =>
                        SupergraphAcquisition
                          .make(supergraph.source, http)
                          .flatMap(decomposeSupergraph(supergraph, _))
                          .map(_._1)
                    }
      loaded     <- loadAll(subgraphs)(Gateway.load(_, http, config.remoteErrorMessages, hooks))
      graph      <- ZIO
                      .fromEither(SchemaComposer.compose(loaded.map(_.subgraph)))
                      .mapError(errors => SchemaCompositionFailed(errors.distinct.sorted))
      _          <- ZIO
                      .fail(GatewayBuildError.InvalidConfiguration(graph.securityDiagnostics))
                      .when(policy.isEmpty && graph.hasSecurityRequirements)
      control    <- GatewayExecutionControl.make(
                      config.maxConcurrentRequests,
                      config.subscriptions,
                      hooks,
                      config.requestTimeout,
                      config.drainTimeout
                    )
      executors   = loaded.map { value =>
                      val name                          = value.subgraph.name
                      val executor: SubgraphExecutor[R] =
                        if (hooks.enabled) new ObservedSubgraphExecutor(name, value.executor, hooks)
                        else value.executor
                      name -> executor
                    }.toMap
      requestRoot = Introspector.withIntrospection(graph.rootType)
      operations <- OperationPreparation.make(
                      requestRoot,
                      new OperationPlanner(
                        graph,
                        executors.size,
                        CandidateSearch.Limits(
                          config.maxPlanningCandidates,
                          config.maxPlanningExpansions,
                          config.planningTimeout
                        )
                      ),
                      new OperationHooks(graph.securityRequirements, resolver, policy, hooks),
                      config,
                      hooks,
                      graph.estimateCost
                    )
    } yield new GatewayInterpreterImpl[R](
      operations,
      new PlanExecutor(graph, executors, hooks),
      control,
      hooks
    )

  private def buildInChildScope[A](effect: => ZIO[Scope, GatewayBuildError, A])(implicit
    trace: Trace
  ): ZIO[Scope, GatewayBuildError, A] =
    ZIO.scopeWith { parent =>
      ZIO.uninterruptibleMask { restore =>
        for {
          child <- parent.fork
          value <- restore(child.extend(effect)).onError(cause => child.close(Exit.failCause(cause)))
        } yield value
      }
    }

  private def acquireSupergraphSnapshot[R1 <: R](supergraph: Supergraph[R1], loader: SupergraphAcquisition.Loader)(
    implicit trace: Trace
  ): IO[GatewayBuildError, Gateway.Snapshot[R1]] =
    decomposeSupergraph(supergraph, loader).map { case (subgraphs, fingerprint) =>
      Gateway.Snapshot(new Gateway(Origin.Composed(subgraphs), resolver, policy, config, hooks), fingerprint)
    }

  private def acquireSubgraphSnapshot[R1 <: R](
    subgraphs: List[Subgraph[R1]],
    http: Option[GatewayHttpClient]
  )(implicit trace: Trace): IO[GatewayBuildError, Gateway.Snapshot[R1]] =
    for {
      loaded <- loadAll(subgraphs)(Gateway.pinAcquiredSchema(_, http))
    } yield Gateway.Snapshot(
      new Gateway(Origin.Composed(loaded.map(_._1)), resolver, policy, config, hooks),
      loaded.flatMap(_._2)
    )

  private def loadAll[R0, R1, A](subgraphs: List[Subgraph[R1]])(
    load: Subgraph[R1] => ZIO[R0, SubgraphBuildError, A]
  )(implicit trace: Trace): ZIO[R0, GatewayBuildError, List[A]] =
    ZIO.foreachPar(subgraphs)(subgraph => load(subgraph).mapError(SubgraphError(subgraph.name, _)).either).flatMap {
      results =>
        val failures = results.collect { case Left(error) => error }.sortBy(_.diagnostics.mkString("\n"))
        ZIO.fail(SubgraphLoadingFailed(failures)).when(failures.nonEmpty) *>
          ZIO.succeed(results.collect { case Right(value) => value })
    }

}

object Gateway {

  /**
   * Creates a reusable gateway description from one or more subgraphs.
   */
  def compose[R](first: Subgraph[R], rest: Subgraph[R]*): Gateway[R] =
    new Gateway[R](Origin.Composed(first :: rest.toList), None, None, GatewayConfig.default, PhaseHooks.empty)

  /**
   * Creates a reusable gateway description from an already composed Apollo Federation supergraph, which is
   * decomposed into the subgraphs it was composed from. Subgraph schemas are never acquired: the supergraph is
   * the single source of truth for both the schemas and the routing urls.
   */
  def fromSupergraph[R](supergraph: Supergraph[R]): Gateway[R] =
    new Gateway[R](Origin.FromSupergraph(supergraph), None, None, GatewayConfig.default, PhaseHooks.empty)

  private[gateway] final case class Snapshot[-R](gateway: Gateway[R], fingerprints: List[String])

  private[gateway] sealed trait Origin[-R] {
    def hasRemote: Boolean
  }

  private[gateway] object Origin {
    final case class Composed[R](subgraphs: List[Subgraph[R]])     extends Origin[R] {
      override val hasRemote: Boolean = subgraphs.exists(_.source.isRemote)
    }
    // Always remote: the projections execute over the routing urls the supergraph declares, whatever
    // the supergraph document itself was read from.
    final case class FromSupergraph[-R](supergraph: Supergraph[R]) extends Origin[R] {
      override val hasRemote: Boolean = true
    }
  }

  private val UplinkMinPollInterval = 10.seconds

  private def load[R](
    subgraph: Subgraph[R],
    http: Option[GatewayHttpClient],
    remoteErrorMessages: Boolean,
    hooks: PhaseHooks[R]
  )(implicit trace: Trace): ZIO[Scope, SubgraphBuildError, LoadedSubgraph[R]] =
    subgraph.source match {
      case remote @ Source.Remote(endpoint, _, federation, config) =>
        for {
          httpClient  <- requireHttpClient(remote, http)
          document    <- RemoteSchemaAcquisition.load(remote, httpClient)
          rootDocument = ensureFederationTransportQuery(document, federation)
          normalized  <- ZIO
                           .fromEither(RemoteSchema.normalize(rootDocument, promoteOrphans = federation))
                           .mapError(SchemaValidationFailed(_))
          prepared    <- ZIO.fromEither(prepareSubgraph(subgraph, normalized, document, federation))
          executor    <-
            RemoteSubgraphExecutor.make(subgraph.name, endpoint, httpClient, config, hooks, remoteErrorMessages)
        } yield LoadedSubgraph(prepared, executor)
      case Source.Local(graph)                                     =>
        val document   = graph.toDocument
        val federation = SchemaComposer.isFederation(document)
        for {
          normalized  <- ZIO.fromEither(RemoteSchema.normalize(document)).mapError(SchemaValidationFailed(_))
          prepared    <- ZIO.fromEither(prepareSubgraph(subgraph, normalized, document, federation))
          interpreter <- ZIO.fromEither(graph.interpreterEither).mapError(SchemaValidationFailed(_))
        } yield LoadedSubgraph(prepared, new LocalSubgraphExecutor(interpreter))
    }

  private def pinAcquiredSchema[R](subgraph: Subgraph[R], http: Option[GatewayHttpClient])(implicit
    trace: Trace
  ): IO[SubgraphBuildError, (Subgraph[R], Option[String])] =
    subgraph.source match {
      case remote @ Source.Remote(_, SchemaInput.Acquired, _, _) =>
        requireHttpClient(remote, http)
          .flatMap(RemoteSchemaAcquisition.load(remote, _))
          .map { document =>
            val pinned = remote.copy(schema = SchemaInput.Parsed(document))
            (
              new Subgraph[R](subgraph.name, pinned, subgraph.lookups, subgraph.transformations),
              Some(SchemaFingerprint(document))
            )
          }
      case _                                                     =>
        ZIO.succeed(subgraph -> None)
    }

  private def requireHttpClient(remote: Source.Remote[_], http: Option[GatewayHttpClient])(implicit
    trace: Trace
  ): IO[SubgraphBuildError, GatewayHttpClient] = {
    val diagnostics = remote.config.diagnostics(includeAcquisition = remote.schema == SchemaInput.Acquired)
    ZIO.fail(SubgraphBuildError.InvalidConfiguration(diagnostics)).when(diagnostics.nonEmpty) *>
      ZIO.fromOption(http).orElseFail(MissingHttpClient)
  }

  private def decomposeSupergraph[R](
    supergraph: Supergraph[R],
    loader: SupergraphAcquisition.Loader
  )(implicit trace: Trace): IO[GatewayBuildError, (List[Subgraph[R]], List[String])] =
    for {
      document    <- loader.load.mapError(SupergraphAcquisitionFailed(_))
      projections <- ZIO
                       .fromEither(SupergraphDecomposition.decompose(document))
                       .mapError(SupergraphDecompositionFailed(_))
      subgraphs    = projections.map(projection =>
                       Subgraph.federation(
                         name = projection.graph.name,
                         endpoint = supergraph.endpoints(projection.graph.name).getOrElse(projection.graph.url),
                         schema = projection.document,
                         config = supergraph.config(projection.graph.name)
                       )
                     )
    } yield subgraphs -> List(SchemaFingerprint(document))

  private final case class LoadedSubgraph[-R](subgraph: PreparedSubgraph, executor: SubgraphExecutor[R])

  private[gateway] def prepareSubgraph[R](
    subgraph: Subgraph[R],
    normalized: RemoteSchema.Normalized,
    sourceDocument: Document,
    federation: Boolean
  ): Either[SubgraphBuildError, PreparedSubgraph] =
    for {
      mapping       <-
        SchemaMapping
          .compile(subgraph.name, normalized.rootType, normalized.document, federation, subgraph.transformations)
          .left
          .map(InvalidTransformations(_))
      extensionTypes = SchemaComposer.federation1ExtensionTypes(sourceDocument).map(mapping.clientType)
      transformed   <- if (mapping.nonEmpty)
                         RemoteSchema
                           .normalize(mapping.transform(normalized.document), promoteOrphans = federation)
                           .left
                           .map(SchemaValidationFailed(_))
                       else Right(normalized)
    } yield PreparedSubgraph(
      subgraph.name,
      transformed.rootType,
      transformed.document,
      federation,
      subgraph.lookups.map(mapping.transform),
      mapping,
      extensionTypes
    )

  private def ensureFederationTransportQuery(document: Document, federation: Boolean): Document =
    if (!federation || hasQueryRoot(document)) document
    else addFederationQueryRoot(document)

  private def hasQueryRoot(document: Document): Boolean = {
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
    hasDeclaredQuery || hasConventionalQuery
  }

  private def addFederationQueryRoot(document: Document): Document = {
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
      List(FieldDefinition(None, ServiceField, Nil, NamedType(ServiceType, nonNull = true), Nil))
    )
    val service  =
      if (names.contains(ServiceType)) Nil
      else
        List(
          ObjectTypeDefinition(
            None,
            ServiceType,
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

  private def nameDiagnostics[R](subgraphs: List[Subgraph[R]]): List[String] = {
    val blank     = subgraphs.collect {
      case subgraph if subgraph.name.trim.isEmpty => "[subgraph] Name must not be empty."
    }
    val duplicate = duplicates(subgraphs.map(_.name)).map(name => s"[subgraph '$name'] Name is used more than once.")

    (blank ::: duplicate).sorted
  }
}

package caliban.gateway

import caliban.gateway.Gateway.{ decomposeSupergraph, Origin }
import caliban.gateway.GatewayBuildError._
import caliban.gateway.internal._
import caliban.gateway.internal.composition.{
  RemoteSchemaAcquisition,
  SchemaComposer,
  SchemaMapping,
  SupergraphAcquisition,
  SupergraphDecomposition
}
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
import caliban.schema.RootType
import caliban.tools.RemoteSchema
import sttp.client4.httpclient.zio.{ HttpClientZioBackend, SttpClient }
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
  private val wrapper: GatewayWrapper[R]
) {

  /**
   * Builds an executable interpreter within the current scope.
   */
  def interpreter(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayInterpreter[R]] = build

  /**
   * Builds a stable interpreter that polls acquired remote schemas and replaces changed generations.
   * Pinned schemas and local graphs remain fixed. Admission limits apply separately to each generation.
   */
  def reloadable(implicit trace: Trace): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] = {
    val originDiagnostics = origin match {
      case Origin.FromSupergraph(supergraph) =>
        val refreshable =
          if (supergraph.source.refreshable) Nil
          else List("Gateway reload from supergraph requires a remote source.")

        val uplink = supergraph.source match {
          case Supergraph.Source.Uplink(uplinkConfig) =>
            // Uplink asks clients not to poll faster than the `minDelaySeconds` it answers with, and
            // Apollo's published floor is ten seconds. Jitter is what reaches the wire, so the fastest
            // poll the configuration permits is what has to clear the floor.
            val floor =
              if (config.minimumReloadPollInterval < 10.seconds)
                List("Supergraph uplink polling requires a reload poll interval of at least ten seconds.")
              else Nil
            uplinkConfig.diagnostics ::: floor
          case _                                      => Nil
        }

        refreshable ::: uplink
      case Origin.Composed(subgraphs)        =>
        val acquired = subgraphs.exists(_.source match {
          case Source.Remote(_, SchemaInput.Acquired, _, _) => true
          case _                                            => false
        })
        Gateway.nameDiagnostics(subgraphs) :::
          (if (acquired) Nil else List("Gateway reload requires at least one acquired remote schema."))
    }

    val diagnostics = config.diagnostics ::: originDiagnostics

    ZIO.fail(GatewayBuildError.InvalidConfiguration(diagnostics)).when(diagnostics.nonEmpty) *>
      buildInChildScope(
        HttpClientZioBackend
          .scoped()
          .mapError(TransportInitializationFailed(_))
          .flatMap { backend =>
            val acquirer = origin match {
              case Origin.Composed(subgraphs)        => Exit.succeed(acquireUnmanagedSnapshot(subgraphs, backend))
              case Origin.FromSupergraph(supergraph) =>
                SupergraphAcquisition
                  .make(supergraph.source, Some(backend))
                  .map(acquireManagedSnapshot(supergraph, _))
            }

            acquirer.flatMap(
              ReloadableGatewayInterpreterImpl.make(
                _,
                config.reloadPollInterval,
                config.reloadJitter,
                config.drainTimeout
              )
            )
          }
      )
  }

  private[gateway] def build(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayInterpreterImpl[R]] = {
    val diagnostics = config.diagnostics ::: (origin match {
      case Origin.Composed(subgraphs) => Gateway.nameDiagnostics(subgraphs)
      // Subgraph names come from the supergraph's graph registry, which the decomposition validates.
      case Origin.FromSupergraph(_)   => Nil
    })

    ZIO.fail(GatewayBuildError.InvalidConfiguration(diagnostics)).when(diagnostics.nonEmpty) *>
      buildInChildScope(buildInterpreter)
  }

  private[gateway] def buildInterpreter(implicit
    trace: Trace
  ): ZIO[Scope, GatewayBuildError, GatewayInterpreterImpl[R]] =
    for {
      backend    <- if (origin.hasRemote)
                      HttpClientZioBackend
                        .scoped()
                        .asSome
                        .mapError(TransportInitializationFailed(_))
                    else ZIO.none
      subgraphs  <- origin match {
                      case Origin.Composed(subgraphs)        => Exit.succeed(subgraphs)
                      case Origin.FromSupergraph(supergraph) =>
                        SupergraphAcquisition
                          .make(supergraph.source, backend)
                          .flatMap(decomposeSupergraph(supergraph, _))
                          .map(_._1)
                    }
      successes  <- loadAll(subgraphs)(subgraph =>
                      Gateway.load(
                        subgraph,
                        backend,
                        config.remoteErrorMessages,
                        wrapper
                      )
                    )
      graph      <- ZIO
                      .fromEither(SchemaComposer.compose(successes.map(_.subgraph)))
                      .mapError(errors => SchemaCompositionFailed(errors.distinct.sorted))
      _          <- ZIO
                      .fail(GatewayBuildError.InvalidConfiguration(graph.securityPolicyDiagnostics))
                      .when(policy.isEmpty && graph.hasSecurityRequirements)
      control    <- GatewayExecutionControl.make(
                      config.maxConcurrentRequests,
                      config.subscriptions,
                      wrapper,
                      config.requestTimeout,
                      config.drainTimeout
                    )
      executors   = successes.map { value =>
                      val name                          = value.subgraph.name
                      val executor: SubgraphExecutor[R] =
                        if (wrapper.enabled) new ObservedSubgraphExecutor(name, value.executor, wrapper)
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
                      new OperationHooks(graph.securityRequirements, resolver, policy, wrapper),
                      config,
                      wrapper,
                      graph.estimatedOperationCost
                    )
    } yield new GatewayInterpreterImpl[R](
      operations,
      new PlanExecutor(graph, executors, wrapper),
      control,
      wrapper
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

  private def acquireManagedSnapshot[R1 <: R](supergraph: Supergraph[R1], loader: SupergraphAcquisition.Loader)(implicit
    trace: Trace
  ): IO[GatewayBuildError, Gateway.Snapshot[R1]] =
    decomposeSupergraph(supergraph, loader).map { case (subgraphs, fingerprint) =>
      Gateway.Snapshot(
        new Gateway(Origin.Composed(subgraphs), resolver, policy, config, wrapper),
        fingerprint
      )
    }

  private def acquireUnmanagedSnapshot[R1 <: R](subgraphs: List[Subgraph[R1]], backend: SttpClient)(implicit
    trace: Trace
  ): IO[GatewayBuildError, Gateway.Snapshot[R1]] =
    for {
      loaded <- loadAll(subgraphs) { subgraph =>
                  subgraph.source match {
                    case Source.Remote(endpoint, SchemaInput.Acquired, federation, remoteConfig) =>
                      val diagnostics = remoteConfig.diagnostics(includeAcquisition = true)
                      (ZIO.fail(SubgraphBuildError.InvalidConfiguration(diagnostics)).when(diagnostics.nonEmpty) *>
                        RemoteSchemaAcquisition.load(
                          SchemaInput.Acquired,
                          endpoint,
                          federation,
                          remoteConfig.acquisition,
                          backend
                        )).map { document =>
                        val pinned = new Subgraph[R1](
                          subgraph.name,
                          Source.Remote(endpoint, SchemaInput.Parsed(document), federation, remoteConfig),
                          subgraph.lookups,
                          subgraph.transformations
                        )
                        (pinned, Some(SchemaFingerprint(document)))
                      }
                    case _                                                                       =>
                      ZIO.succeed(subgraph -> Option.empty[String])
                  }
                }
    } yield Gateway.Snapshot(
      new Gateway(Origin.Composed(loaded.map(_._1)), resolver, policy, config, wrapper),
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

  /**
   * Transforms the finite operation and admission limits used by each built interpreter.
   */
  def withConfig(configure: GatewayConfig => GatewayConfig): Gateway[R] =
    new Gateway(origin, resolver, policy, configure(config), wrapper)

  /**
   * Resolves canonical GraphQL text before parsing and validation.
   */
  def withOperationResolver[R1 <: R](value: OperationResolver[R1]): Gateway[R1] =
    new Gateway(origin, Some(value), policy, config, wrapper)

  /**
   * Allows or rejects operations after validation and variable coercion.
   */
  def withOperationPolicy[R1 <: R](value: OperationPolicy[R1]): Gateway[R1] =
    new Gateway(origin, resolver, Some(value), config, wrapper)

  /**
   * Adds an integration around the gateway lifecycle.
   */
  def @@[R1 <: R](value: GatewayWrapper[R1]): Gateway[R1] =
    new Gateway(origin, resolver, policy, config, wrapper |+| value)
}

object Gateway {

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

  /**
   * Creates a reusable gateway description from one or more subgraphs.
   */
  def compose[R](first: Subgraph[R], rest: Subgraph[R]*): Gateway[R] =
    new Gateway[R](Origin.Composed(first :: rest.toList), None, None, GatewayConfig.default, GatewayWrapper.empty)

  /**
   * Creates a reusable gateway description from an already composed Apollo Federation supergraph, which is
   * decomposed into the subgraphs it was composed from. Subgraph schemas are never acquired: the supergraph is
   * the single source of truth for both the schemas and the routing urls.
   */
  def fromSupergraph[R](supergraph: Supergraph[R]): Gateway[R] =
    new Gateway[R](Origin.FromSupergraph(supergraph), None, None, GatewayConfig.default, GatewayWrapper.empty)

  private def load[R](
    subgraph: Subgraph[R],
    backend: Option[SttpClient],
    remoteErrorMessages: Boolean,
    wrapper: GatewayWrapper[R]
  )(implicit trace: Trace): ZIO[Scope, SubgraphBuildError, LoadedSubgraph[R]] =
    subgraph.source match {
      case Source.Remote(endpoint, schema, federation, config) =>
        val policyDiagnostics = config.diagnostics(schema == SchemaInput.Acquired)
        for {
          _                <- ZIO
                                .fail(SubgraphBuildError.InvalidConfiguration(policyDiagnostics))
                                .when(policyDiagnostics.nonEmpty)
          client           <- ZIO
                                .fromOption(backend)
                                .orElseFail(RemoteTransportUnavailable)
          document         <- RemoteSchemaAcquisition
                                .load(schema, endpoint, federation, config.acquisition, client)
          rootDocument      = ensureFederationTransportQuery(document, federation)
          normalized       <- ZIO
                                .fromEither(RemoteSchema.normalize(rootDocument, promoteOrphans = federation))
                                .mapError(InvalidSchema(_))
          preparedSubgraph <- ZIO.fromEither(
                                prepareSubgraph(
                                  subgraph,
                                  normalized.rootType,
                                  normalized.document,
                                  document,
                                  federation
                                )
                              )
          executor         <- RemoteSubgraphExecutor.make(
                                subgraph.name,
                                endpoint,
                                client,
                                config,
                                wrapper,
                                remoteErrorMessages
                              )
        } yield LoadedSubgraph(preparedSubgraph, executor)
      case Source.Local(graph)                                 =>
        val document   = graph.toDocument
        val federation = SchemaComposer.isFederation(document)
        for {
          normalized       <- ZIO
                                .fromEither(RemoteSchema.normalize(document))
                                .mapError(InvalidSchema(_))
          preparedSubgraph <- ZIO.fromEither(
                                prepareSubgraph(
                                  subgraph,
                                  normalized.rootType,
                                  normalized.document,
                                  document,
                                  federation
                                )
                              )
          interpreter      <- ZIO.fromEither(graph.interpreterEither).mapError(InvalidSchema(_))
        } yield LoadedSubgraph(preparedSubgraph, new LocalSubgraphExecutor(interpreter))
    }

  private def decomposeSupergraph[R](
    supergraph: Supergraph[R],
    loader: SupergraphAcquisition.Loader
  )(implicit trace: Trace): IO[GatewayBuildError, (List[Subgraph[R]], List[String])] =
    for {
      supergraphDoc <- loader.load.mapError(SupergraphAcquisitionFailed(_))
      projected     <- ZIO
                         .fromEither(SupergraphDecomposition.decompose(supergraphDoc))
                         .mapError(SupergraphDecompositionFailed(_))
      subgraphs      = projected.map(p =>
                         Subgraph.federation(
                           name = p.graph.name,
                           endpoint = supergraph.endpoints(p.graph.name).getOrElse(p.graph.url),
                           schema = p.document,
                           config = supergraph.config(p.graph.name)
                         )
                       )
    } yield subgraphs -> List(SchemaFingerprint(supergraphDoc))

  private final case class LoadedSubgraph[-R](
    subgraph: PreparedSubgraph,
    executor: SubgraphExecutor[R]
  )

  private[gateway] def prepareSubgraph[R](
    subgraph: Subgraph[R],
    sourceRootType: RootType,
    normalizedDocument: Document,
    sourceDocument: Document,
    federation: Boolean
  ): Either[SubgraphBuildError, PreparedSubgraph] =
    for {
      mapping       <- SchemaMapping
                         .compile(
                           subgraph.name,
                           sourceRootType,
                           normalizedDocument,
                           federation,
                           subgraph.transformations
                         )
                         .left
                         .map(InvalidTransformations(_))
      extensionTypes = SchemaComposer.federation1ExtensionTypes(sourceDocument).map(mapping.clientType)
      normalized    <- if (mapping.nonEmpty)
                         RemoteSchema
                           .normalize(mapping.transform(normalizedDocument), promoteOrphans = federation)
                           .left
                           .map(InvalidSchema(_))
                       else Right(RemoteSchema.Normalized(sourceRootType, normalizedDocument))
    } yield PreparedSubgraph(
      subgraph.name,
      normalized.rootType,
      normalized.document,
      federation,
      subgraph.lookups.map(mapping.transform),
      mapping,
      extensionTypes
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

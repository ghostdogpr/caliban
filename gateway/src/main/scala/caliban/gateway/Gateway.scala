package caliban.gateway

import caliban.gateway.Gateway.Origin
import caliban.gateway.GatewayBuildError._
import caliban.gateway.internal._
import caliban.gateway.internal.acquisition.{ RemoteSchemaAcquisition, SupergraphAcquisition }
import caliban.gateway.internal.composition._
import caliban.gateway.internal.execution._
import caliban.gateway.internal.planning.{ CandidateSearch, OperationPlanner }
import caliban.gateway.Subgraph.{ SchemaInput, Source }
import caliban.introspection.Introspector
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

  private def openHttpClient(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayHttpClient] =
    GatewayHttpClient.make.mapError(TransportInitializationFailed(_))

  private[gateway] def buildInterpreter(http: GatewayHttpClient)(implicit
    trace: Trace
  ): ZIO[Scope, GatewayBuildError, GatewayInterpreterImpl[R]] =
    for {
      subgraphs   <- origin match {
                       case Origin.Composed(subgraphs)        => Exit.succeed(subgraphs)
                       case Origin.FromSupergraph(supergraph) =>
                         SupergraphAcquisition
                           .make(supergraph.source, http)
                           .flatMap(supergraph.load(_))
                           .map(_._1)
                     }
      executables <- loadAll(subgraphs)(_.load(http, config.remoteErrorMessages, hooks))
      graph       <- ZIO.fromEither(SchemaComposer.compose(executables.map(value => value.subgraph -> value.document)))
      _           <- ZIO
                       .fail(GatewayBuildError.InvalidConfiguration(graph.securityDiagnostics))
                       .when(policy.isEmpty && graph.hasSecurityRequirements)
      control     <- GatewayExecutionControl.make(
                       config.subscriptions,
                       hooks,
                       config.requestTimeout,
                       config.drainTimeout
                     )
      executors    = executables.map { value =>
                       val name                          = value.subgraph.name
                       val executor: SubgraphExecutor[R] =
                         if (hooks.enabled) new ObservedSubgraphExecutor(name, value.executor, hooks)
                         else value.executor
                       name -> executor
                     }.toMap
      requestRoot  = Introspector.withIntrospection(graph.rootType)
      operations  <- OperationPreparation.make(
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
    supergraph.load(loader).map { case (subgraphs, fingerprint) =>
      Gateway.Snapshot(new Gateway(Origin.Composed(subgraphs), resolver, policy, config, hooks), fingerprint)
    }

  private def acquireSubgraphSnapshot[R1 <: R](
    subgraphs: List[Subgraph[R1]],
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[GatewayBuildError, Gateway.Snapshot[R1]] =
    for {
      pinned <- loadAll(subgraphs)(Gateway.pinAcquiredSchema(_, http))
    } yield Gateway.Snapshot(
      new Gateway(Origin.Composed(pinned.map(_._1)), resolver, policy, config, hooks),
      pinned.flatMap(_._2)
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

  private[gateway] sealed trait Origin[-R]

  private[gateway] object Origin {
    final case class Composed[R](subgraphs: List[Subgraph[R]])     extends Origin[R]
    final case class FromSupergraph[-R](supergraph: Supergraph[R]) extends Origin[R]
  }

  private val UplinkMinPollInterval = 10.seconds

  private def pinAcquiredSchema[R](subgraph: Subgraph[R], http: GatewayHttpClient)(implicit
    trace: Trace
  ): IO[SubgraphBuildError, (Subgraph[R], Option[String])] =
    subgraph.source match {
      case remote @ Source.Remote(_, SchemaInput.Acquired, _, _) =>
        for {
          _        <- remote.validateConfig
          document <- RemoteSchemaAcquisition.load(remote, http)
          pinned    = remote.copy(schema = SchemaInput.Parsed(document))
        } yield (
          new Subgraph[R](subgraph.name, pinned, subgraph.lookups, subgraph.transformations),
          Some(SchemaFingerprint(document))
        )
      case _                                                     =>
        ZIO.succeed(subgraph -> None)
    }

  private def nameDiagnostics[R](subgraphs: List[Subgraph[R]]): List[String] = {
    val blank     = subgraphs.collect {
      case subgraph if subgraph.name.trim.isEmpty => "[subgraph] Name must not be empty."
    }
    val duplicate = duplicates(subgraphs.map(_.name)).map(name => s"[subgraph '$name'] Name is used more than once.")

    (blank ::: duplicate).sorted
  }
}

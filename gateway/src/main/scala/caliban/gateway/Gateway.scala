package caliban.gateway

import caliban.gateway.Gateway.Origin
import caliban.gateway.GatewayBuildError._
import caliban.gateway.internal._
import caliban.gateway.internal.acquisition.SupergraphAcquisition
import caliban.gateway.internal.composition._
import caliban.gateway.internal.execution._
import caliban.gateway.internal.planning.{ CandidateSearch, OperationCost, OperationPlanner, OperationSecurity }
import caliban.introspection.Introspector
import caliban.parsing.adt.Document
import caliban.rendering.DocumentRenderer
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
  private val config: GatewayConfig,
  private val hooks: PhaseHooks[R]
) {

  /**
   * Builds an executable interpreter within the current scope.
   */
  def interpreter(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayInterpreter[R]] = build

  /**
   * Builds a stable interpreter that polls acquired remote schemas and replaces changed generations.
   * Pinned schemas and local graphs remain fixed. Configured limits apply separately to each generation.
   */
  def reloadable(implicit trace: Trace): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] =
    validate(config.diagnostics ::: buildDiagnostics ::: reloadDiagnostics) *>
      buildInChildScope(
        openHttpClient
          .flatMap(acquisition)
          .flatMap(ReloadableGatewayInterpreterImpl.make(_, config.reloadDelay, config.drainTimeout))
      )

  /**
   * Updates the configuration used by each built interpreter.
   */
  def withConfig(configure: GatewayConfig => GatewayConfig): Gateway[R] =
    new Gateway(origin, configure(config), hooks)

  /**
   * Adds phase hooks to the gateway lifecycle. Hooks accumulate: each call appends to the hooks already attached,
   * so several independent integrations can be layered onto the same description.
   */
  def withPhaseHooks[R1 <: R](hooks: PhaseHooks[R1]): Gateway[R1] =
    new Gateway(origin, config, this.hooks ++ hooks)

  /**
   * Symbolic version of `withPhaseHooks`.
   */
  def @@[R1 <: R](hooks: PhaseHooks[R1]): Gateway[R1] =
    withPhaseHooks(hooks)

  private[gateway] def build(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayInterpreterImpl[R]] =
    validate(config.diagnostics ::: buildDiagnostics) *>
      buildInChildScope(openHttpClient.flatMap(acquisition).flatten.flatMap(_.build))

  private def acquisition(
    http: GatewayHttpClient
  )(implicit trace: Trace): UIO[IO[GatewayBuildError, Gateway.Snapshot[R]]] =
    origin match {
      case Origin.Composed(subgraphs)        => Exit.succeed(snapshot(subgraphs, Nil, http))
      case Origin.FromSupergraph(supergraph) =>
        SupergraphAcquisition
          .make(supergraph.source, http)
          .map(_.mapError(SupergraphAcquisitionFailed(_)).flatMap { document =>
            supergraph.load(document).flatMap { subgraphs =>
              validate(subgraphs.flatMap(_.diagnostics)) *> snapshot(subgraphs, List(document), http)
            }
          })
    }

  private def buildDiagnostics: List[String] =
    origin match {
      case Origin.Composed(subgraphs)        => Gateway.nameDiagnostics(subgraphs) ::: subgraphs.flatMap(_.diagnostics)
      // Subgraph names come from the supergraph's graph registry, which the decomposition validates.
      case Origin.FromSupergraph(supergraph) => supergraph.source.diagnostics
    }

  private def reloadDiagnostics: List[String] =
    origin match {
      case Origin.FromSupergraph(supergraph) => supergraph.source.reloadDiagnostics(config.minimumReloadPollInterval)
      case Origin.Composed(subgraphs)        =>
        check(subgraphs.exists(_.acquired), "Gateway reload requires at least one acquired remote schema.")
    }

  private def validate(diagnostics: List[String])(implicit trace: Trace): IO[GatewayBuildError, Unit] =
    ZIO.fail(GatewayBuildError.InvalidConfiguration(diagnostics)).when(diagnostics.nonEmpty).unit

  private def openHttpClient(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayHttpClient] =
    GatewayHttpClient.make.mapError(TransportInitializationFailed(_))

  private def buildInterpreter[R1 <: R](subgraphs: List[Subgraph.Resolved[R1]], http: GatewayHttpClient)(implicit
    trace: Trace
  ): ZIO[Scope, GatewayBuildError, GatewayInterpreterImpl[R1]] =
    for {
      executables <- loadAll(subgraphs)(_.subgraph.name)(_.load(http, config.remoteErrorMessages, hooks))
      graph       <- ZIO.fromEither(SchemaComposer.compose(executables.map(_.prepared))).flatMap {
                       case (_, first :: rest) if config.fatalCompositionWarnings =>
                         ZIO.fail(GatewayBuildError.SchemaCompositionFailed(::(first, rest)))
                       case (graph, warnings)                                     =>
                         ZIO.foreachDiscard(warnings)(warning => ZIO.logWarning(warning.render)).as(graph)
                     }
      security     = new OperationSecurity(graph)
      _           <- validate(if (hooks.authorization.nonEmpty) Nil else security.diagnostics)
      executors    = executables.map(value => value.prepared.name -> value.executor).toMap
      requestRoot  = Introspector.withIntrospection(graph.rootType)
      operations  <- OperationPreparation.make(
                       requestRoot,
                       new OperationPlanner(
                         graph,
                         CandidateSearch.Limits(
                           config.maxPlanningCandidates,
                           config.maxPlanningExpansions,
                           config.planningTimeout
                         )
                       ),
                       security,
                       config.maxOperationCacheWeight,
                       config.maxOperationCost.map(
                         new OperationCost(graph.rootType.types, graph.possibleTypesByName, graph.costMetadata) -> _
                       ),
                       hooks
                     )
      // Shut down requests and subscriptions in parallel so each starts its drain timeout immediately.
      interpreter <-
        ZIO.parallelFinalizers(
          GatewayExecutionControl
            .make(config.requestTimeout, config.drainTimeout)
            .zipWith(SubscriptionControl.make(config.subscriptions, hooks))(
              new GatewayInterpreterImpl[R1](operations, new PlanExecutor(graph, executors, hooks), _, _, hooks)
            )
        )
    } yield interpreter

  private def buildInChildScope[A](effect: => ZIO[Scope, GatewayBuildError, A])(implicit
    trace: Trace
  ): ZIO[Scope, GatewayBuildError, A] =
    ZIO.scopeWith { parent =>
      ZIO.uninterruptibleMask(restore =>
        parent.fork.flatMap[Any, GatewayBuildError, A](Gateway.buildIn(_, restore)(effect))
      )
    }

  private def snapshot[R1 <: R](subgraphs: List[Subgraph[R1]], sources: List[Document], http: GatewayHttpClient)(
    implicit trace: Trace
  ): IO[GatewayBuildError, Gateway.Snapshot[R1]] =
    loadAll(subgraphs)(_.name)(_.resolve(http)).map { resolved =>
      val acquired = resolved.collect { case value if value.subgraph.acquired => value.document }
      Gateway.Snapshot(buildInterpreter(resolved, http), sources ::: acquired)
    }

  private def loadAll[R0, A, B](values: List[A])(name: A => String)(
    load: A => ZIO[R0, SubgraphBuildError, B]
  )(implicit trace: Trace): ZIO[R0, GatewayBuildError, List[B]] =
    ZIO.foreachPar(values)(value => load(value).mapError(SubgraphError(name(value), _)).either).flatMap { results =>
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
    new Gateway[R](Origin.Composed(first :: rest.toList), GatewayConfig.default, PhaseHooks.empty)

  /**
   * Creates a reusable gateway description from an already composed Apollo Federation supergraph, which is
   * decomposed into the subgraphs it was composed from. Subgraph schemas are never acquired: the supergraph is
   * the single source of truth for both the schemas and the routing urls.
   */
  def fromSupergraph[R](supergraph: Supergraph[R]): Gateway[R] =
    new Gateway[R](Origin.FromSupergraph(supergraph), GatewayConfig.default, PhaseHooks.empty)

  private[gateway] final case class Snapshot[-R](
    build: ZIO[Scope, GatewayBuildError, GatewayInterpreterImpl[R]],
    documents: List[Document]
  ) {

    /**
     * Fingerprints the rendered schema, so source locations and formatting do not change it.
     * The original document is never modified.
     */
    lazy val fingerprints: List[String] = documents.map(DocumentRenderer.render(_))
  }

  private[gateway] sealed trait Origin[-R]

  private[gateway] object Origin {
    final case class Composed[R](subgraphs: List[Subgraph[R]])     extends Origin[R]
    final case class FromSupergraph[-R](supergraph: Supergraph[R]) extends Origin[R]
  }

  private[gateway] def buildIn[E, A](scope: Scope.Closeable, restore: ZIO.InterruptibilityRestorer)(
    effect: ZIO[Scope, E, A]
  )(implicit trace: Trace): IO[E, A] =
    restore(scope.extend[Any](effect)).onError(cause => scope.close(Exit.failCause(cause)))

  private def nameDiagnostics[R](subgraphs: List[Subgraph[R]]): List[String] = {
    val blank     = subgraphs.collect {
      case subgraph if subgraph.name.trim.isEmpty => "[subgraph] Name must not be empty."
    }
    val duplicate = duplicates(subgraphs.map(_.name)).map(name => s"[subgraph '$name'] Name is used more than once.")

    (blank ::: duplicate).sorted
  }
}

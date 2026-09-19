# Gateway examples

Each example is a runnable `ZIOAppDefault` application.

The standalone local gateway needs no other process:

```sh
sbt "gatewayExamples/runMain example.gateway.LocalGatewayApp"
```

For the ordinary GraphQL and mixed-source gateway, start these applications in separate terminals:

```sh
sbt "gatewayExamples/runMain example.gateway.ProductsApi"
sbt "gatewayExamples/runMain example.gateway.ReviewsApi"
sbt "gatewayExamples/runMain example.gateway.GatewayApp"
```

`GatewayApp` uses pinned SDL for products, acquired SDL for reviews, and an in-process Caliban subgraph. Its GraphiQL
page is available at <http://localhost:8080/graphiql>.

For the Federation gateway, start:

```sh
sbt "gatewayExamples/runMain example.gateway.federation.ProductsApi"
sbt "gatewayExamples/runMain example.gateway.federation.ReviewsApi"
sbt "gatewayExamples/runMain example.gateway.federation.FederationGatewayApp"
```

The Federation GraphiQL page is available at <http://localhost:8090/graphiql>.

`ManagedGatewayApp` loads the supergraph from Apollo GraphOS instead. It reads the graph ref and API key from the
`APOLLO_GRAPH_REF` and `APOLLO_KEY` environment variables:

```sh
APOLLO_GRAPH_REF=my-graph@production APOLLO_KEY=service:my-apikey \
  sbt "gatewayExamples/runMain example.gateway.federation.ManagedGatewayApp"
```

It serves GraphiQL on the same port, 8090, so run it instead of `FederationGatewayApp`.

## Observability hooks

Gateway integrations attach hooks to named lifecycle phases. Built-in metrics are opt-in, as shown by `GatewayApp`:

```scala
import caliban.gateway.{ Gateway, GatewayMetrics }

val gateway = Gateway.compose(first, rest: _*) @@ GatewayMetrics.hooks
```

A gateway with no hooks attached never updates a metric registry, reads a clock to time one, allocates a label, or takes
the instrumented admission path. Attach the hooks, and they record bounded-cardinality metrics for requests, routing,
subgraph calls, retries, the operation cache, admission, and subscriptions.

The `gateway-tracing` module provides `GatewayTracing.hooks`. Hooks compose with `++`, so you can install tracing and
metrics together:

```scala
import caliban.gateway.GatewayMetrics
import caliban.gateway.tracing.GatewayTracing

val observed = Gateway.compose(first, rest: _*) @@ (GatewayMetrics.hooks ++ GatewayTracing.hooks)
```

The `@@` operator is a symbolic equivalent of `withPhaseHooks`. Hooks accumulate, so this composes with any hooks applied before or after:

```scala
import caliban.gateway.{ Gateway, GatewayMetrics, PhaseHandler, PhaseHooks }
import zio.ZIO

val logged = Gateway.compose(first, rest: _*)
  .withPhaseHooks(
    PhaseHooks.subgraphCall(
      PhaseHandler.outgoing((event, result) => ZIO.logInfo(s"${event.subgraph} -> ${result.outcome.label}"))
    )
  ) @@ GatewayMetrics.hooks
```

`PhaseHooks.Event` is a sealed trait with one case per lifecycle phase. `Event.Attempt` stands for each physical HTTP
attempt, attempt zero included. A phase handler sees only its own event type, so `PhaseHooks.attempt` receives an
`Event.Attempt` and never has to match on every event.

Pick the constructor that matches how much of the phase you need:

- `PhaseHandler.incoming` runs before the phase and can rewrite the event. `incomingDiscard` is the same without the rewrite.
- `PhaseHandler.outgoing` sees the phase's typed result.
- `PhaseHandler.apply` does both and can carry state from one side to the other, which is what brackets the phase effect.
- `PhaseHandler.scoped` binds a `Scope` to that bracket, so a resource acquired on the way in outlives the outgoing side.

`GatewayTracing` is built from the last two. It opens a span on the way in and records the phase's outcome on it on the
way out, so result metadata lands on the span that covers the phase. Custom logging, profiling, policy, and telemetry
integrations use the same constructors.

Events carry a bounded set of data. Closed event values, such as the operation type, cache result, and admission kind,
match exhaustively. A request that hits its deadline completes with `PhaseHooks.Outcome.Timeout`.

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

## Observability wrappers

Gateway integrations use the same `GatewayWrapper` lifecycle seam. Built-in metrics are opt-in, as shown by `GatewayApp`:

```scala
import caliban.gateway.{ Gateway, GatewayMetrics }

val gateway = Gateway.compose(first, rest: _*) @@ GatewayMetrics.wrapper
```

Keeping metrics opt-in means a gateway without wrappers does not perform metric-registry updates, read clocks for metric
durations, allocate metric labels, or use the instrumented admission path. The wrapper records bounded-cardinality request,
routing, source-call, physical-attempt, retry, cache, admission, in-flight deduplication, body-size, and overdue metrics.

The `gateway-tracing` module provides `GatewayTracing.wrapper`. Wrappers compose with `|+|`, so tracing and metrics can be
installed together:

```scala
import caliban.gateway.GatewayMetrics
import caliban.gateway.tracing.GatewayTracing

val observed = Gateway.compose(first, rest: _*) @@
  (GatewayMetrics.wrapper |+| GatewayTracing.wrapper)
```

`GatewayWrapper.Event` is one lifecycle algebra; `Event.Attempt` represents each physical HTTP attempt, including attempt zero.
A wrapper receives the event, the effect, and a typed completion function in one `wrap` call. This keeps result metadata inside
the span or metric scope that owns it and lets custom logging, profiling, policy, or telemetry integrations use the same seam.
Events expose bounded metadata only and never include raw GraphQL documents, variables, headers, or response bodies. Closed
event values such as operation type, cache result, admission kind, and deduplication result can be matched exhaustively; request
deadlines complete with `GatewayWrapper.Outcome.Timeout`.

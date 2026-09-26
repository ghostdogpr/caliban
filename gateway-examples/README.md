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

## Metrics and tracing

`GatewayApp` enables `GatewayMetrics.hooks` to record request execution, preparation, subgraph calls, retries, cache activity, and subscriptions. Export these ZIO metrics with a connector such as Prometheus.

For tracing, add the optional `caliban-gateway-tracing` module and attach `GatewayTracing.hooks`. See the [metrics and tracing guide](../vuepress/docs/docs/gateway/hooks.md#metrics-and-tracing) for dependencies and exporter setup, or [Hooks](../vuepress/docs/docs/gateway/hooks.md) for custom logging and authorization.

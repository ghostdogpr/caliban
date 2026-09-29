package example.gateway

import caliban.gateway.{ Gateway, GatewayMetrics, Subgraph }

object GatewayApp extends ExampleApp("Gateway", 8080) {
  def interpreter =
    (Gateway.compose(
      Subgraph.graphql("products", ProductsApi.endpoint, ProductsApi.api.toDocument),
      Subgraph.graphql("reviews", ReviewsApi.endpoint),
      Subgraph.graphql("gateway", LocalGatewayApp.localApi)
    ) @@ GatewayMetrics.hooks).interpreter
}

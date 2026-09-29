package example.gateway.federation

import caliban.gateway.{ Gateway, Subgraph }
import example.gateway.ExampleApp

object FederationGatewayApp extends ExampleApp("Federation gateway", 8090) {
  def interpreter =
    Gateway
      .compose(
        Subgraph.federation("products", ProductsApi.endpoint),
        Subgraph.federation("reviews", ReviewsApi.endpoint)
      )
      .interpreter
}

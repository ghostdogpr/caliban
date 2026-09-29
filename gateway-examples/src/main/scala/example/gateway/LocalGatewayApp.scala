package example.gateway

import caliban.gateway.{ Gateway, Subgraph }
import caliban.schema.Schema.auto._
import caliban.{ graphQL, RootResolver }

object LocalGatewayApp extends ExampleApp("Local gateway", 8080) {
  final case class Query(greeting: String)

  val localApi = graphQL(RootResolver(Query("Hello from a local subgraph")))

  def interpreter = Gateway.compose(Subgraph.graphql("local", localApi)).interpreter
}

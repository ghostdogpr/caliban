package caliban.federation

import caliban.GraphQL

trait FederatedGraphQL[R] extends GraphQL[R] {
  /** The service schema for the federated subgraph. */
  val service: GraphQL[R]
}

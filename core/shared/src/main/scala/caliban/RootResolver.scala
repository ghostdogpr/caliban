package caliban

/**
 * A `root resolver` contains resolvers for the 3 types of operations allowed in GraphQL: queries, mutations and subscriptions.
 *
 * A `resolver` is a simple value of the case class describing the API.
 */
case class RootResolver[+Query, +Mutation, +Subscription](
  queryResolver: Option[Query],
  mutationResolver: Option[Mutation],
  subscriptionResolver: Option[Subscription]
)

object RootResolver {

  /**
   * Constructs a [[RootResolver]] with only a query resolver.
   */
  def apply[Query](queryResolver: Query): RootResolver[Query, Unit, Unit] =
    RootResolver(Some(queryResolver), Option.empty[Unit], Option.empty[Unit])

  /**
   * Constructs a [[RootResolver]] with a query resolver and a mutation resolver.
   */
  def apply[Query, Mutation](queryResolver: Query, mutationResolver: Mutation): RootResolver[Query, Mutation, Unit] =
    RootResolver(Some(queryResolver), Some(mutationResolver), Option.empty[Unit])

  /**
   * Constructs a [[RootResolver]] with a query resolver, a mutation resolver and a subscription resolver.
   */
  def apply[Query, Mutation, Subscription](
    queryResolver: Query,
    mutationResolver: Mutation,
    subscriptionResolver: Subscription
  ): RootResolver[Query, Mutation, Subscription] =
    RootResolver(Some(queryResolver), Some(mutationResolver), Some(subscriptionResolver))
}

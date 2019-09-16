package caliban

case class RootResolver[Query, Mutation, Subscription](
  queryResolver: Query,
  mutationResolver: Option[Mutation],
  subscriptionResolver: Option[Subscription]
)

object RootResolver {
  def apply[Query](queryResolver: Query): RootResolver[Query, Unit, Unit] =
    RootResolver(queryResolver, Option.empty[Unit], Option.empty[Unit])

  def apply[Query, Mutation](queryResolver: Query, mutationResolver: Mutation): RootResolver[Query, Mutation, Unit] =
    RootResolver(queryResolver, Some(mutationResolver), Option.empty[Unit])

  def apply[Query, Mutation, Subscription](
    queryResolver: Query,
    mutationResolver: Mutation,
    subscriptionResolver: Subscription
  ): RootResolver[Query, Mutation, Subscription] =
    RootResolver(queryResolver, Some(mutationResolver), Some(subscriptionResolver))
}

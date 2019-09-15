package caliban

case class RootResolver[Query, Mutation, Subscription](
  queryResolver: Query,
  mutationResolver: Option[Mutation],
  subscriptionResolver: Option[Subscription]
)

object RootResolver {
  def apply[Query](queryResolver: Query): RootResolver[Query, Unit, Unit] =
    RootResolver(queryResolver, Option.empty[Unit], Option.empty[Unit])
}

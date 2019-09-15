package caliban

case class RootSchema[Query, Mutation, Subscription](
  query: Operation[Query],
  mutation: Option[Operation[Mutation]],
  subscription: Option[Operation[Subscription]]
)

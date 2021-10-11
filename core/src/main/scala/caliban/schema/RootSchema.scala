package caliban.schema

final case class RootSchema[-R](query: Operation[R], mutation: Option[Operation[R]], subscription: Option[Operation[R]])

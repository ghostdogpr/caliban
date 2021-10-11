package caliban.introspection.adt

final case class __Schema(
  queryType: __Type,
  mutationType: Option[__Type],
  subscriptionType: Option[__Type],
  types: List[__Type],
  directives: List[__Directive]
)

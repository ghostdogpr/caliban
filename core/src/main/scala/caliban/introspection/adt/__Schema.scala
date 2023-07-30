package caliban.introspection.adt

case class __Schema(
  description: Option[String],
  queryType: __Type,
  mutationType: Option[__Type],
  subscriptionType: Option[__Type],
  types: List[__Type],
  directives: List[__Directive]
)

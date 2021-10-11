package caliban.introspection.adt

final case class __Introspection(
  __schema: __Schema,
  __type: __TypeArgs => Option[__Type]
)

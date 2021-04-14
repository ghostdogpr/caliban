package caliban.introspection.adt

case class __Introspection(
  __schema: __Schema,
  __type: __TypeArgs => Option[__Type]
)

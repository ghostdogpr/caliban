package caliban.introspection.adt

case class __Type(
  kind: __TypeKind,
  name: Option[String] = None,
  description: Option[String] = None,
  fields: __DeprecatedArgs => Option[List[__Field]] = _ => None,
  interfaces: Option[List[__Type]] = None,
  possibleTypes: Option[List[__Type]] = None,
  enumValues: __DeprecatedArgs => Option[List[__EnumValue]] = _ => None,
  inputFields: Option[List[__InputValue]] = None,
  ofType: Option[__Type] = None
)

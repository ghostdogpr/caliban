package caliban.introspection.adt

sealed trait __DirectiveLocation

object __DirectiveLocation {
  case object QUERY                  extends __DirectiveLocation
  case object MUTATION               extends __DirectiveLocation
  case object SUBSCRIPTION           extends __DirectiveLocation
  case object FIELD                  extends __DirectiveLocation
  case object FRAGMENT_DEFINITION    extends __DirectiveLocation
  case object FRAGMENT_SPREAD        extends __DirectiveLocation
  case object INLINE_FRAGMENT        extends __DirectiveLocation
  case object SCHEMA                 extends __DirectiveLocation
  case object SCALAR                 extends __DirectiveLocation
  case object OBJECT                 extends __DirectiveLocation
  case object FIELD_DEFINITION       extends __DirectiveLocation
  case object ARGUMENT_DEFINITION    extends __DirectiveLocation
  case object INTERFACE              extends __DirectiveLocation
  case object UNION                  extends __DirectiveLocation
  case object ENUM                   extends __DirectiveLocation
  case object ENUM_VALUE             extends __DirectiveLocation
  case object INPUT_OBJECT           extends __DirectiveLocation
  case object INPUT_FIELD_DEFINITION extends __DirectiveLocation
}

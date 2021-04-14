package caliban.introspection.adt

import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveLocation
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveLocation._

sealed trait __DirectiveLocation { self =>
  def toDirectiveLocation: DirectiveLocation =
    self match {
      case __DirectiveLocation.QUERY                  => ExecutableDirectiveLocation.QUERY
      case __DirectiveLocation.MUTATION               => ExecutableDirectiveLocation.MUTATION
      case __DirectiveLocation.SUBSCRIPTION           => ExecutableDirectiveLocation.SUBSCRIPTION
      case __DirectiveLocation.FIELD                  => ExecutableDirectiveLocation.FIELD
      case __DirectiveLocation.FRAGMENT_DEFINITION    => ExecutableDirectiveLocation.FRAGMENT_DEFINITION
      case __DirectiveLocation.FRAGMENT_SPREAD        => ExecutableDirectiveLocation.FRAGMENT_SPREAD
      case __DirectiveLocation.INLINE_FRAGMENT        => ExecutableDirectiveLocation.INLINE_FRAGMENT
      case __DirectiveLocation.SCHEMA                 => TypeSystemDirectiveLocation.SCHEMA
      case __DirectiveLocation.SCALAR                 => TypeSystemDirectiveLocation.SCALAR
      case __DirectiveLocation.OBJECT                 => TypeSystemDirectiveLocation.OBJECT
      case __DirectiveLocation.FIELD_DEFINITION       => TypeSystemDirectiveLocation.FIELD_DEFINITION
      case __DirectiveLocation.ARGUMENT_DEFINITION    => TypeSystemDirectiveLocation.ARGUMENT_DEFINITION
      case __DirectiveLocation.INTERFACE              => TypeSystemDirectiveLocation.INTERFACE
      case __DirectiveLocation.UNION                  => TypeSystemDirectiveLocation.UNION
      case __DirectiveLocation.ENUM                   => TypeSystemDirectiveLocation.ENUM
      case __DirectiveLocation.ENUM_VALUE             => TypeSystemDirectiveLocation.ENUM_VALUE
      case __DirectiveLocation.INPUT_OBJECT           => TypeSystemDirectiveLocation.INPUT_OBJECT
      case __DirectiveLocation.INPUT_FIELD_DEFINITION => TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION
    }
}

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

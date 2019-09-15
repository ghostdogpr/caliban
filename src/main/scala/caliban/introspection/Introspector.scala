package caliban.introspection

import caliban.schema.{ RootType, Schema }
import caliban.schema.Types.{ Argument, Type }

object Introspector {

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

  case class __Directive(
    name: String,
    description: Option[String],
    locations: Set[__DirectiveLocation],
    args: List[Argument]
  )
  case class __Schema(
    queryType: Type,
    mutationType: Option[Type],
    subscriptionType: Option[Type],
    types: Set[Type],
    directives: List[__Directive]
  )
  case class TypeArgs(name: String)
  case class Introspection(__schema: __Schema, __type: TypeArgs => Type)

  implicit lazy val typeSchema: Schema[Type] = Schema.gen[Type]

  def introspect(rootType: RootType): (Schema[Introspection], Introspection) = {
    val types = rootType.types.values.toSet
    val resolver = Introspection(
      __Schema(rootType.queryType, rootType.mutationType, rootType.subscriptionType, types, Nil),
      args => types.find(_.name.contains(args.name)).get
    )

    val introspectionSchema = Schema.gen[Introspection]
    (introspectionSchema, resolver)
  }
}

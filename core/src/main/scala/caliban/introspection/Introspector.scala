package caliban.introspection

import caliban.schema.RootSchema.Operation
import caliban.schema.{ RootSchema, RootType, Schema }
import caliban.schema.Types.{ __InputValue, __Type }

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
    args: List[__InputValue]
  )
  case class __Schema(
    queryType: __Type,
    mutationType: Option[__Type],
    subscriptionType: Option[__Type],
    types: List[__Type],
    directives: List[__Directive]
  )
  case class TypeArgs(name: String)
  case class Introspection(__schema: __Schema, __type: TypeArgs => __Type)

  implicit lazy val typeSchema: Schema[__Type] = Schema.gen[__Type]

  def introspect(rootType: RootType): RootSchema[Introspection, Nothing, Nothing] = {
    val types = rootType.types.values.toList.sortBy(_.name.getOrElse(""))
    val resolver = Introspection(
      __Schema(rootType.queryType, rootType.mutationType, rootType.subscriptionType, types, Nil),
      args => types.find(_.name.contains(args.name)).get
    )
    val introspectionSchema = Schema.gen[Introspection]
    RootSchema(Operation(introspectionSchema, resolver), None, None)
  }
}

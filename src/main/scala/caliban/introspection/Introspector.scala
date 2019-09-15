package caliban.introspection

import caliban.schema.{ Schema, Types }
import caliban.schema.Types.Type

object Introspector {

  case class __Schema(queryType: Type, types: Set[Type])
  case class TypeArgs(name: String)
  case class Introspection(__schema: __Schema, __type: TypeArgs => Type)

  implicit lazy val typeSchema: Schema[Type] = Schema.gen[Type]

  def introspect[G](schema: Schema[G]): (Schema[Introspection], Introspection) = {
    val schemaType: Type = schema.toType
    val types: Set[Type] = Types.collectTypes(schemaType).values.toSet
    val resolver         = Introspection(__Schema(schemaType, types), args => types.find(_.name.contains(args.name)).get)

    val introspectionSchema = Schema.gen[Introspection]
    (introspectionSchema, resolver)
  }
}

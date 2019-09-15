package caliban.introspection

import caliban.schema.{ RootType, Schema }
import caliban.schema.Types.Type

object Introspector {

  case class __Schema(queryType: Type, mutationType: Option[Type], subscriptionType: Option[Type], types: Set[Type])
  case class TypeArgs(name: String)
  case class Introspection(__schema: __Schema, __type: TypeArgs => Type)

  implicit lazy val typeSchema: Schema[Type] = Schema.gen[Type]

  def introspect(rootType: RootType): (Schema[Introspection], Introspection) = {
    val types = rootType.types.values.toSet
    val resolver = Introspection(
      __Schema(rootType.queryType, rootType.mutationType, rootType.subscriptionType, types),
      args => types.find(_.name.contains(args.name)).get
    )

    val introspectionSchema = Schema.gen[Introspection]
    (introspectionSchema, resolver)
  }
}

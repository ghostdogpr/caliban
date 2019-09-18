package caliban.introspection

import caliban.introspection.adt._
import caliban.parsing.adt.Document
import caliban.parsing.adt.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Selection.Field
import caliban.schema.RootSchema.Operation
import caliban.schema.{ RootSchema, RootType, Schema }

object Introspector {

  implicit lazy val typeSchema: Schema[__Type] = Schema.gen[__Type]

  def introspect(rootType: RootType): RootSchema[__Introspection, Nothing, Nothing] = {
    val types = rootType.types.values.toList.sortBy(_.name.getOrElse(""))
    val resolver = __Introspection(
      __Schema(rootType.queryType, rootType.mutationType, rootType.subscriptionType, types, Nil),
      args => types.find(_.name.contains(args.name)).get
    )
    val introspectionSchema = Schema.gen[__Introspection]
    RootSchema(Operation(introspectionSchema, resolver), None, None)
  }

  def isIntrospection(document: Document): Boolean =
    document.definitions.forall {
      case OperationDefinition(_, _, _, _, selectionSet) =>
        selectionSet.forall {
          case Field(_, name, _, _, _) => name == "__schema" || name == "__type"
          case _                       => true
        }
      case _ => true
    }
}

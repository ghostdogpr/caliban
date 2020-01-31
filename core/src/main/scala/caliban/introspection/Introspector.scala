package caliban.introspection

import caliban.introspection.adt._
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Document
import caliban.parsing.adt.Selection.Field
import caliban.schema.RootSchema.Operation
import caliban.schema.{ RootSchema, RootType, Schema, Types }

object Introspector {

  implicit lazy val typeSchema: Schema[Any, __Type] = Schema.gen[__Type]

  private[caliban] val directives = List(
    __Directive(
      "skip",
      Some(
        "The @skip directive may be provided for fields, fragment spreads, and inline fragments, and allows for conditional exclusion during execution as described by the if argument."
      ),
      Set(__DirectiveLocation.FIELD, __DirectiveLocation.FRAGMENT_SPREAD, __DirectiveLocation.INLINE_FRAGMENT),
      List(__InputValue("if", None, () => Types.boolean, None))
    ),
    __Directive(
      "include",
      Some(
        "The @include directive may be provided for fields, fragment spreads, and inline fragments, and allows for conditional inclusion during execution as described by the if argument."
      ),
      Set(__DirectiveLocation.FIELD, __DirectiveLocation.FRAGMENT_SPREAD, __DirectiveLocation.INLINE_FRAGMENT),
      List(__InputValue("if", None, () => Types.boolean, None))
    )
  )

  /**
   * Generates a schema for introspecting the given type.
   */
  def introspect(rootType: RootType): RootSchema[Any] = {
    val types = rootType.types.updated("Boolean", Types.boolean).values.toList.sortBy(_.name.getOrElse(""))
    val resolver = __Introspection(
      __Schema(
        rootType.queryType,
        rootType.mutationType,
        rootType.subscriptionType,
        types,
        directives ++ (rootType.additionalDirectives getOrElse List.empty)
      ),
      args => types.find(_.name.contains(args.name)).get
    )
    val introspectionSchema = Schema.gen[__Introspection]
    RootSchema(Operation(introspectionSchema.toType(), introspectionSchema.resolve(resolver)), None, None)
  }

  private[caliban] def isIntrospection(document: Document): Boolean =
    document.definitions.forall {
      case OperationDefinition(_, _, _, _, selectionSet) =>
        selectionSet.forall {
          case Field(_, name, _, _, _, _) => name == "__schema" || name == "__type"
          case _                          => true
        }
      case _ => true
    }
}

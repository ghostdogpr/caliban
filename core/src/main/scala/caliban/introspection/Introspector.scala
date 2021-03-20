package caliban.introspection

import caliban.CalibanError.ExecutionError
import caliban.introspection.adt._
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Document
import caliban.parsing.adt.Selection.Field
import caliban.schema.Step.QueryStep
import caliban.schema._
import caliban.wrappers.Wrapper.IntrospectionWrapper
import zio.ZIO
import zio.query.ZQuery

import scala.annotation.tailrec

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
  def introspect[R](
    rootType: RootType,
    introWrappers: List[IntrospectionWrapper[R]] = Nil
  ): RootSchema[R] = {

    @tailrec
    def wrap(
      query: ZIO[R, ExecutionError, __Introspection]
    )(wrappers: List[IntrospectionWrapper[R]]): ZIO[R, ExecutionError, __Introspection] =
      wrappers match {
        case Nil             => query
        case wrapper :: tail => wrap(wrapper.f(query))(tail)
      }

    val types    = rootType.types.updated("Boolean", Types.boolean).values.toList.sortBy(_.name.getOrElse(""))
    val resolver = __Introspection(
      __Schema(
        rootType.queryType,
        rootType.mutationType,
        rootType.subscriptionType,
        types,
        directives ++ rootType.additionalDirectives
      ),
      args => types.find(_.name.contains(args.name))
    )

    val introspectionSchema = Schema.gen[__Introspection]
    RootSchema(
      Operation(
        introspectionSchema.toType_(),
        QueryStep(ZQuery.fromEffect(wrap(ZIO.succeed(resolver))(introWrappers)).map(introspectionSchema.resolve))
      ),
      None,
      None
    )
  }

  private[caliban] def isIntrospection(document: Document): Boolean =
    document.definitions.forall {
      case OperationDefinition(_, _, _, _, selectionSet) =>
        selectionSet.nonEmpty && selectionSet.forall {
          case Field(_, name, _, _, _, _) => name == "__schema" || name == "__type"
          case _                          => false
        }
      case _                                             => true
    }
}

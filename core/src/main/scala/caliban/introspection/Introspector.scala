package caliban.introspection

import caliban.CalibanError.ExecutionError
import caliban.introspection.adt._
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Document
import caliban.parsing.adt.Selection.Field
import caliban.schema.Step.QueryStep
import caliban.schema._
import caliban.wrappers.Wrapper.IntrospectionWrapper
import zio.{ Exit, ZIO }
import zio.query.ZQuery

import scala.annotation.tailrec
import scala.collection.mutable

object Introspector extends IntrospectionDerivation {
  private[caliban] val directives = List(
    __Directive(
      "skip",
      Some(
        "The @skip directive may be provided for fields, fragment spreads, and inline fragments, and allows for conditional exclusion during execution as described by the if argument."
      ),
      Set(__DirectiveLocation.FIELD, __DirectiveLocation.FRAGMENT_SPREAD, __DirectiveLocation.INLINE_FRAGMENT),
      _ => List(__InputValue("if", None, () => Types.boolean.nonNull, None)),
      isRepeatable = false
    ),
    __Directive(
      "include",
      Some(
        "The @include directive may be provided for fields, fragment spreads, and inline fragments, and allows for conditional inclusion during execution as described by the if argument."
      ),
      Set(__DirectiveLocation.FIELD, __DirectiveLocation.FRAGMENT_SPREAD, __DirectiveLocation.INLINE_FRAGMENT),
      _ => List(__InputValue("if", None, () => Types.boolean.nonNull, None)),
      isRepeatable = false
    ),
    __Directive(
      "specifiedBy",
      Some(
        "The @specifiedBy directive is used within the type system definition language to provide a URL for specifying the behavior of custom scalar types. The URL should point to a human-readable specification of the data format, serialization, and coercion rules. It must not appear on built-in scalar types."
      ),
      Set(__DirectiveLocation.SCALAR),
      _ => List(__InputValue("url", None, () => Types.string.nonNull, None)),
      isRepeatable = false
    )
  )

  private val introspectionType       = introspectionSchema.toType_()
  val introspectionRootType: RootType = RootType(introspectionType, None, None)

  private val oneOfDirective =
    __Directive(
      "oneOf",
      Some(
        "The `@oneOf` directive is used within the type system definition language to indicate an Input Object is a OneOf Input Object."
      ),
      Set(__DirectiveLocation.INPUT_OBJECT),
      _ => Nil,
      isRepeatable = false
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
        case wrapper :: tail => wrap(wrapper.wrap(query))(tail)
      }

    val typesMap = new mutable.HashMap[String, __Type]()
    typesMap.sizeHint(rootType.types.size + introspectionRootType.types.size + 2)
    typesMap ++= rootType.types
    typesMap ++= introspectionRootType.types
    typesMap.remove("__Introspection")
    typesMap.update("Boolean", Types.boolean) // because of skip and include
    typesMap.update("String", Types.string)   // because of specifiedBy

    val types    = typesMap.values.toList.sortBy(_.name)
    val hasOneOf = types.exists(_._isOneOfInput)

    val resolver = __Introspection(
      __Schema(
        rootType.description,
        rootType.queryType,
        rootType.mutationType,
        rootType.subscriptionType,
        types,
        directives ++ (if (hasOneOf) List(oneOfDirective) else Nil) ++ rootType.additionalDirectives
      ),
      args => typesMap.get(args.name)
    )

    val step = introWrappers match {
      case Nil => introspectionSchema.resolve(resolver)
      case ws  => QueryStep(ZQuery.fromZIO(wrap(ZIO.succeed(resolver))(ws).map(introspectionSchema.resolve)))
    }

    RootSchema(Operation(introspectionType, step), None, None)
  }

  private[caliban] def isIntrospection(document: Document): Boolean =
    document.definitions.forall {
      case OperationDefinition(_, _, _, _, selectionSet) =>
        selectionSet.nonEmpty && selectionSet.forall {
          case Field(_, "__schema" | "__type", _, _, _, _) => true
          case _                                           => false
        }
      case _                                             => true
    }
}

package caliban.schema

import caliban.ResponseValue.ObjectValue
import caliban.Value.{ EnumValue, NullValue, StringValue }
import caliban.introspection.adt.{ __DeprecatedArgs, __Field, __Type }
import caliban.schema.Step.MetadataFunctionStep
import caliban.parsing.adt.Directive

private[schema] object SchemaUtils {

  /**
   * Directive used to mark a field as semantically non-nullable.
   */
  val SemanticNonNull = Directive("semanticNonNull")

  private val fakeField =
    Some(
      List(
        __Field(
          "_",
          Some(
            "Fake field because GraphQL does not support empty objects. Do not query, use __typename instead."
          ),
          _ => Nil,
          () => Types.makeScalar("Boolean")
        )
      )
    )

  def isEmptyUnionObject(t: __Type): Boolean =
    t.fields(__DeprecatedArgs.include).contains(Nil)

  // see https://github.com/graphql/graphql-spec/issues/568
  def fixEmptyUnionObject(t: __Type): __Type =
    if (isEmptyUnionObject(t)) t.copy(fields = (_: __DeprecatedArgs) => fakeField)
    else t

  def resolveEmptyUnionStep[R](step: Step[R]): Step[R] = step match {
    case s @ PureStep(EnumValue(v)) =>
      MetadataFunctionStep[R] { field =>
        field.fields.view
          .filter(_._condition.forall(_.contains(v)))
          .collectFirst {
            case f if f.name == "__typename" => ObjectValue(List(f.aliasedName -> StringValue(v)))
            case f if f.name == "_"          => NullValue
          }
          .fold(s)(PureStep(_))
      }
    case _                          => step
  }
}

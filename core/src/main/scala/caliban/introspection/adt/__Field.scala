package caliban.introspection.adt

import caliban.{ InputValue, ResponseValue }
import caliban.Value.StringValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{ FieldDefinition, InputValueDefinition }
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLExcluded
import caliban.schema.Schema

case class __Field(
  name: String,
  description: Option[String],
  args: __DeprecatedArgs => List[__InputValue],
  `type`: () => __Type,
  isDeprecated: Boolean = false,
  deprecationReason: Option[String] = None,
  @GQLExcluded directives: Option[List[Directive]] = None,
  @GQLExcluded extend: Option[Extend] = None,
  @GQLExcluded renameInput: String => String = identity
) {
  final override lazy val hashCode: Int = super.hashCode()

  def toFieldDefinition: FieldDefinition = {
    val allDirectives = (if (isDeprecated)
                           List(
                             Directive(
                               "deprecated",
                               List(deprecationReason.map(reason => "reason" -> StringValue(reason))).flatten.toMap
                             )
                           )
                         else Nil) ++ directives.getOrElse(Nil)
    FieldDefinition(description, name, allArgs.map(_.toInputValueDefinition), _type.toType(), allDirectives)
  }

  def toInputValueDefinition: InputValueDefinition =
    InputValueDefinition(description, name, _type.toType(), None, directives.getOrElse(Nil))

  lazy val allArgs: List[__InputValue] =
    args(__DeprecatedArgs.include)

  private[caliban] lazy val _type: __Type = `type`()

  private[caliban] lazy val renameArguments: String => String =
    allArgs.foldLeft(identity[String] _) { case (renameInput, arg) => renameInput andThen arg.renameInput }
}

case class Extend(
  sourceGraph: String,
  sourceFieldName: String,
  argumentMappings: Map[String, InputValue => (String, InputValue)] = Map.empty,
  filterBatchResults: Option[(ResponseValue.ObjectValue, ResponseValue.ObjectValue) => Boolean] = None,
  additionalFields: List[String] = Nil,
  target: Option[String] = None
)

object Extend {
  // fake schema to allow schema derivation
  implicit val schema: Schema[Any, Extend] = Schema.unitSchema.contramap(_ => ())
}

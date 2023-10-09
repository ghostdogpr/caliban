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
  args: List[__InputValue],
  `type`: () => __Type,
  isDeprecated: Boolean = false,
  deprecationReason: Option[String] = None,
  @GQLExcluded directives: Option[List[Directive]] = None,
  @GQLExcluded extend: Option[Extend] = None,
  @GQLExcluded renameInput: String => String = identity
) {
  def toFieldDefinition: FieldDefinition = {
    val allDirectives = (if (isDeprecated)
                           List(
                             Directive(
                               "deprecated",
                               List(deprecationReason.map(reason => "reason" -> StringValue(reason))).flatten.toMap
                             )
                           )
                         else Nil) ++ directives.getOrElse(Nil)
    FieldDefinition(description, name, args.map(_.toInputValueDefinition), _type.toType(), allDirectives)
  }

  def toInputValueDefinition: InputValueDefinition =
    InputValueDefinition(description, name, _type.toType(), None, directives.getOrElse(Nil))

  private[caliban] lazy val _type: __Type = `type`()

  private[caliban] lazy val renameArguments: String => String =
    args.foldLeft(identity[String] _) { case (renameInput, arg) => renameInput andThen arg.renameInput }
}

case class Extend(
  sourceGraph: String,
  sourceFieldName: String,
  argumentMappings: Map[String, InputValue => (String, InputValue)] = Map.empty,
  filterBatchResults: (ResponseValue.ObjectValue, ResponseValue.ObjectValue) => Boolean = (_, _) => true
)

object Extend {
  // fake schema to allow schema derivation
  implicit val schema: Schema[Any, Extend] = Schema.unitSchema.contramap(_ => ())
}

package caliban.introspection.adt

import caliban.Value.StringValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{ FieldDefinition, InputValueDefinition }
import caliban.parsing.adt.Directive

final case class __Field(
  name: String,
  description: Option[String],
  args: List[__InputValue],
  `type`: () => __Type,
  isDeprecated: Boolean = false,
  deprecationReason: Option[String] = None,
  directives: Option[List[Directive]] = None
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
    FieldDefinition(description, name, args.map(_.toInputValueDefinition), `type`().toType(), allDirectives)
  }

  def toInputValueDefinition: InputValueDefinition =
    InputValueDefinition(description, name, `type`().toType(), None, directives.getOrElse(Nil))
}

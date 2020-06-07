package caliban.introspection.adt

import caliban.Value.StringValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.EnumValueDefinition
import caliban.parsing.adt.Directive

case class __EnumValue(
  name: String,
  description: Option[String],
  isDeprecated: Boolean,
  deprecationReason: Option[String]
) {
  def toEnumValueDefinition: EnumValueDefinition =
    EnumValueDefinition(
      description,
      name,
      if (isDeprecated)
        List(
          Directive(
            "deprecated",
            List(deprecationReason.map(reason => "reason" -> StringValue(reason))).flatten.toMap
          )
        )
      else Nil
    )
}

package caliban.introspection.adt

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.EnumValueDefinition
import caliban.parsing.adt.{ Directive, Directives }
import caliban.schema.Annotations.GQLExcluded

case class __EnumValue(
  name: String,
  description: Option[String],
  isDeprecated: Boolean,
  deprecationReason: Option[String],
  @GQLExcluded directives: Option[List[Directive]]
) {
  def toEnumValueDefinition: EnumValueDefinition =
    EnumValueDefinition(
      description,
      name,
      Directives.deprecation(isDeprecated, deprecationReason) ++ directives.getOrElse(Nil)
    )
}

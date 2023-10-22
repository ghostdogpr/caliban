package caliban.introspection.adt

import caliban.Value.StringValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.EnumValueDefinition
import caliban.parsing.adt.Directive
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
      (if (isDeprecated)
         List(
           Directive(
             "deprecated",
             List(deprecationReason.map(reason => "reason" -> StringValue(reason))).flatten.toMap
           )
         )
       else Nil) ++ directives.getOrElse(Nil)
    )
}

object __EnumValue {
  def simple(
    name: String,
    description: Option[String] = None,
    isDeprecated: Boolean = false,
    deprecationReason: Option[String] = None,
    directives: List[Directive] = List.empty
  ): __EnumValue = new __EnumValue(
    name,
    description,
    isDeprecated,
    deprecationReason,
    if (directives.isEmpty) None else Some(directives)
  )
}

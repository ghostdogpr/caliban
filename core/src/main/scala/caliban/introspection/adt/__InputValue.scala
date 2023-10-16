package caliban.introspection.adt

import caliban.Value.StringValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition
import caliban.parsing.adt.Directive
import caliban.parsing.Parser
import caliban.schema.Annotations.GQLExcluded

case class __InputValue(
  name: String,
  description: Option[String],
  `type`: () => __Type,
  defaultValue: Option[String],
  isDeprecated: Boolean = false,
  deprecationReason: Option[String] = None,
  @GQLExcluded directives: Option[List[Directive]] = None,
  @GQLExcluded renameInput: String => String = identity
) {
  def toInputValueDefinition: InputValueDefinition = {
    val default       = defaultValue.flatMap(v => Parser.parseInputValue(v).toOption)
    val allDirectives = (if (isDeprecated)
                           List(
                             Directive(
                               "deprecated",
                               List(deprecationReason.map(reason => "reason" -> StringValue(reason))).flatten.toMap
                             )
                           )
                         else Nil) ++ directives.getOrElse(Nil)
    InputValueDefinition(description, name, _type.toType(), default, allDirectives)
  }

  private[caliban] lazy val _type: __Type = `type`()
}

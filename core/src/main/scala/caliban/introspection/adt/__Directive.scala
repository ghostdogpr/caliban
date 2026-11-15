package caliban.introspection.adt

import caliban.Value.StringValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveDefinition
import caliban.parsing.adt.{ Directive, Directives }
import caliban.schema.Annotations.GQLExcluded

case class __Directive(
  name: String,
  description: Option[String],
  locations: Set[__DirectiveLocation],
  args: __DeprecatedArgs => List[__InputValue],
  isRepeatable: Boolean,
  isDeprecated: Boolean = false,
  deprecationReason: Option[String] = None,
  @GQLExcluded directives: Option[List[Directive]] = None
) {
  def toDirectiveDefinition: DirectiveDefinition =
    DirectiveDefinition(
      description,
      name,
      allArgs.map(_.toInputValueDefinition),
      isRepeatable,
      locations.map(_.toDirectiveLocation),
      allDirectives
    )

  lazy val allArgs: List[__InputValue] =
    args(__DeprecatedArgs.include)

  private[caliban] lazy val allDirectives: List[Directive] =
    (if (isDeprecated)
       List(
         Directive(
           Directives.DeprecatedDirective,
           deprecationReason.map(reason => "reason" -> StringValue(reason)).toMap
         )
       )
     else Nil) ++ directives.getOrElse(Nil)
}

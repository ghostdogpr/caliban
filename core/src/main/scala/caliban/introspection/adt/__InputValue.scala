package caliban.introspection.adt

import caliban.parsing.Parser
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition
import caliban.parsing.adt.{ Directive, Directives }
import caliban.schema.Annotations.GQLExcluded

import scala.annotation.tailrec

case class __InputValue(
  name: String,
  description: Option[String],
  `type`: () => __Type,
  defaultValue: Option[String],
  isDeprecated: Boolean = false,
  deprecationReason: Option[String] = None,
  @GQLExcluded directives: Option[List[Directive]] = None,
  @GQLExcluded parentType: () => Option[__Type] = () => None
) {
  def toInputValueDefinition: InputValueDefinition =
    InputValueDefinition(
      description,
      name,
      _type.toType(),
      parsedDefaultValue,
      Directives.deprecation(isDeprecated, deprecationReason) ++ directives.getOrElse(Nil)
    )

  private[caliban] lazy val _type: __Type      = `type`()
  private[caliban] lazy val _parentType        = parentType()
  private[caliban] lazy val parsedDefaultValue = defaultValue.flatMap(value => Parser.parseInputValue(value).toOption)

  /**
   * Makes the [[`type`]] nullable as required by the spec for OneOf Input Objects
   */
  private[caliban] def nullable: __InputValue = {
    @tailrec
    def loop(tpe: __Type): __Type =
      (tpe.kind, tpe.ofType) match {
        case (__TypeKind.NON_NULL, Some(inner)) => loop(inner)
        case _                                  => tpe
      }

    copy(`type` = () => loop(_type))
  }
}

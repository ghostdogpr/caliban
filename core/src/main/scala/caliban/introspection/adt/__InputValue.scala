package caliban.introspection.adt

import caliban.Value.StringValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition
import caliban.parsing.adt.Directive
import caliban.parsing.Parser
import caliban.schema.Annotations.GQLExcluded
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
  @GQLExcluded parentTypeName: Option[String] = None
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

    val t = loop(_type)
    copy(`type` = () => t)
  }
}

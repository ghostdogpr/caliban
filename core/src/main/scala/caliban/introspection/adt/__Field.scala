package caliban.introspection.adt

import caliban.Scala3Annotations.threadUnsafe
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{ FieldDefinition, InputValueDefinition }
import caliban.parsing.adt.{ Directive, Directives }
import caliban.schema.Annotations.GQLExcluded

case class __Field(
  name: String,
  description: Option[String],
  args: __DeprecatedArgs => List[__InputValue],
  `type`: () => __Type,
  isDeprecated: Boolean = false,
  deprecationReason: Option[String] = None,
  @GQLExcluded directives: Option[List[Directive]] = None
) {
  @transient @threadUnsafe
  final override lazy val hashCode: Int = caliban.Hash.caseClassHash(this)

  def toFieldDefinition: FieldDefinition =
    FieldDefinition(
      description,
      name,
      allArgs.map(_.toInputValueDefinition),
      _type.toType(),
      Directives.deprecation(isDeprecated, deprecationReason) ++ directives.getOrElse(Nil)
    )

  def toInputValueDefinition: InputValueDefinition =
    InputValueDefinition(description, name, _type.toType(), None, directives.getOrElse(Nil))

  lazy val allArgs: List[__InputValue] =
    args(__DeprecatedArgs.include)

  private[caliban] lazy val _type: __Type = `type`()

  private[caliban] lazy val allArgNames: Set[String] =
    allArgs.view.map(_.name).toSet
}

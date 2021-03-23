package caliban.introspection.adt

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Directive, Type }

case class __Type(
  kind: __TypeKind,
  name: Option[String] = None,
  description: Option[String] = None,
  fields: __DeprecatedArgs => Option[List[__Field]] = _ => None,
  interfaces: () => Option[List[__Type]] = () => None,
  possibleTypes: Option[List[__Type]] = None,
  enumValues: __DeprecatedArgs => Option[List[__EnumValue]] = _ => None,
  inputFields: Option[List[__InputValue]] = None,
  ofType: Option[__Type] = None,
  directives: Option[List[Directive]] = None,
  origin: Option[String] = None
) {
  def |+|(that: __Type): __Type = __Type(
    kind,
    (name ++ that.name).reduceOption((_, b) => b),
    (description ++ that.description).reduceOption((_, b) => b),
    args => (fields(args) ++ that.fields(args)).reduceOption(_ ++ _),
    () => (interfaces() ++ that.interfaces()).reduceOption(_ ++ _),
    (possibleTypes ++ that.possibleTypes).reduceOption(_ ++ _),
    args => (enumValues(args) ++ that.enumValues(args)).reduceOption(_ ++ _),
    (inputFields ++ that.inputFields).reduceOption(_ ++ _),
    (ofType ++ that.ofType).reduceOption(_ |+| _),
    (directives ++ that.directives).reduceOption(_ ++ _),
    (origin ++ that.origin).reduceOption((_, b) => b)
  )

  def toType(nonNull: Boolean = false): Type =
    ofType match {
      case Some(of) =>
        kind match {
          case __TypeKind.LIST     => ListType(of.toType(false), nonNull)
          case __TypeKind.NON_NULL => of.toType(true)
          case _                   => NamedType(name.getOrElse(""), nonNull)
        }
      case None     => NamedType(name.getOrElse(""), nonNull)
    }

  def toTypeDefinition: Option[TypeDefinition] =
    kind match {
      case __TypeKind.SCALAR       =>
        Some(ScalarTypeDefinition(description, name.getOrElse(""), directives.getOrElse(Nil)))
      case __TypeKind.OBJECT       =>
        Some(
          ObjectTypeDefinition(
            description,
            name.getOrElse(""),
            interfaces().getOrElse(Nil).map(t => NamedType(t.name.getOrElse(""), nonNull = false)),
            directives.getOrElse(Nil),
            fields(__DeprecatedArgs(Some(true))).getOrElse(Nil).map(_.toFieldDefinition)
          )
        )
      case __TypeKind.INTERFACE    =>
        Some(
          InterfaceTypeDefinition(
            description,
            name.getOrElse(""),
            directives.getOrElse(Nil),
            fields(__DeprecatedArgs(Some(true))).getOrElse(Nil).map(_.toFieldDefinition)
          )
        )
      case __TypeKind.UNION        =>
        Some(
          UnionTypeDefinition(
            description,
            name.getOrElse(""),
            directives.getOrElse(Nil),
            possibleTypes.getOrElse(Nil).flatMap(_.name)
          )
        )
      case __TypeKind.ENUM         =>
        Some(
          EnumTypeDefinition(
            description,
            name.getOrElse(""),
            directives.getOrElse(Nil),
            enumValues(__DeprecatedArgs(Some(true))).getOrElse(Nil).map(_.toEnumValueDefinition)
          )
        )
      case __TypeKind.INPUT_OBJECT =>
        Some(
          InputObjectTypeDefinition(
            description,
            name.getOrElse(""),
            directives.getOrElse(Nil),
            inputFields.getOrElse(Nil).map(_.toInputValueDefinition)
          )
        )
      case _                       => None
    }

  def isNullable: Boolean =
    kind match {
      case __TypeKind.NON_NULL => false
      case _                   => true
    }
}

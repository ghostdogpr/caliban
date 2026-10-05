package caliban.gateway

/**
 * A schema element named as in the GraphQL Schema Coordinates RFC: `Product`, `Product.price`,
 * `Query.product(id:)`, `@tag` or `@tag(name:)`. A member is a field, an input field or an enum value.
 */
sealed trait SchemaCoordinate {
  import SchemaCoordinate._

  def render: String = this match {
    case Type(name)                                 => name
    case Member(typeName, memberName)               => s"$typeName.$memberName"
    case Argument(typeName, fieldName, argument)    => s"$typeName.$fieldName($argument:)"
    case Directive(name)                            => s"@$name"
    case DirectiveArgument(directiveName, argument) => s"@$directiveName($argument:)"
  }
}

object SchemaCoordinate {
  final case class Type(name: String)                                                  extends SchemaCoordinate
  final case class Member(typeName: String, memberName: String)                        extends SchemaCoordinate
  final case class Argument(typeName: String, fieldName: String, argumentName: String) extends SchemaCoordinate
  final case class Directive(name: String)                                             extends SchemaCoordinate
  final case class DirectiveArgument(directiveName: String, argumentName: String)      extends SchemaCoordinate
}

package caliban.parsing.adt

sealed trait SchemaCoordinate extends Serializable { self =>
  import SchemaCoordinate._

  override def toString: String = self match {
    case TypeCoordinate(name)                                 => name
    case MemberCoordinate(typeName, memberName)               => s"$typeName.$memberName"
    case ArgumentCoordinate(typeName, fieldName, argument)    => s"$typeName.$fieldName($argument:)"
    case DirectiveCoordinate(name)                            => s"@$name"
    case DirectiveArgumentCoordinate(directiveName, argument) => s"@$directiveName($argument:)"
  }
}

object SchemaCoordinate {
  final case class TypeCoordinate(name: String)                                             extends SchemaCoordinate
  final case class MemberCoordinate(typeName: String, memberName: String)                   extends SchemaCoordinate
  final case class ArgumentCoordinate(typeName: String, fieldName: String, argumentName: String)
      extends SchemaCoordinate
  final case class DirectiveCoordinate(name: String)                                        extends SchemaCoordinate
  final case class DirectiveArgumentCoordinate(directiveName: String, argumentName: String) extends SchemaCoordinate
}

package caliban.parsing.adt

import scala.annotation.tailrec

sealed trait Type

object Type {

  case class NamedType(name: String, nonNull: Boolean) extends Type
  case class ListType(ofType: Type, nonNull: Boolean)  extends Type

  case class FieldDefinition(
    description: Option[String],
    name: String,
    args: List[(String, Type)],
    ofType: Type,
    directives: List[Directive]
  )

  @tailrec
  def innerType(t: Type): String = t match {
    case NamedType(name, _)  => name
    case ListType(ofType, _) => innerType(ofType)
  }
}

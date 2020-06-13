package caliban.parsing.adt

import scala.annotation.tailrec

sealed trait Type { self =>
  val nonNull: Boolean
  var nullable: Boolean = !nonNull

  override def toString: String = self match {
    case Type.NamedType(name, nonNull)  => if (nonNull) s"$name!" else name
    case Type.ListType(ofType, nonNull) => if (nonNull) s"[$ofType]!" else s"[$ofType]"
  }
}

object Type {

  case class NamedType(name: String, nonNull: Boolean) extends Type
  case class ListType(ofType: Type, nonNull: Boolean)  extends Type

  @tailrec
  def innerType(t: Type): String = t match {
    case NamedType(name, _)  => name
    case ListType(ofType, _) => innerType(ofType)
  }
}

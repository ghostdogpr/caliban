package caliban.parsing.adt

sealed trait Type

object Type {
  case class NamedType(name: String, nonNull: Boolean) extends Type
  case class ListType(ofType: Type, nonNull: Boolean)  extends Type
}

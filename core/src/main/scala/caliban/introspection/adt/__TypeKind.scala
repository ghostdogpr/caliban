package caliban.introspection.adt

sealed trait __TypeKind

object __TypeKind {
  case object SCALAR       extends __TypeKind
  case object OBJECT       extends __TypeKind
  case object INTERFACE    extends __TypeKind
  case object UNION        extends __TypeKind
  case object ENUM         extends __TypeKind
  case object INPUT_OBJECT extends __TypeKind
  case object LIST         extends __TypeKind
  case object NON_NULL     extends __TypeKind
}

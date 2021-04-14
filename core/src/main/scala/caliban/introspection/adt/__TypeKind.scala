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

  implicit val kindOrdering: Ordering[__TypeKind] = Ordering
    .by[__TypeKind, Int] {
      case __TypeKind.SCALAR       => 1
      case __TypeKind.NON_NULL     => 2
      case __TypeKind.LIST         => 3
      case __TypeKind.UNION        => 4
      case __TypeKind.ENUM         => 5
      case __TypeKind.INPUT_OBJECT => 6
      case __TypeKind.INTERFACE    => 7
      case __TypeKind.OBJECT       => 8
    }

  implicit val typeOrdering: Ordering[__Type] =
    Ordering.by(o => (o.kind, o.name.getOrElse("")))
}

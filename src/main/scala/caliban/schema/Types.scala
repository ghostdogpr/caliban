package caliban.schema

object Types {

  sealed trait TypeKind

  object TypeKind {
    case object SCALAR       extends TypeKind
    case object OBJECT       extends TypeKind
    case object INTERFACE    extends TypeKind
    case object UNION        extends TypeKind
    case object ENUM         extends TypeKind
    case object INPUT_OBJECT extends TypeKind
    case object LIST         extends TypeKind
    case object NON_NULL     extends TypeKind
  }

  def makeScalar(name: String) = Type(TypeKind.SCALAR, Some(name))

  def makeList(underlying: Type) = Type(TypeKind.LIST, ofType = Some(underlying))

  def makeNonNull(underlying: Type) = Type(TypeKind.NON_NULL, ofType = Some(underlying))

  def makeEnum(name: Option[String], description: Option[String], values: List[String]) =
    Type(TypeKind.ENUM, name, description, values = values)

  def makeObject(name: Option[String], description: Option[String], fields: List[Field]) =
    Type(TypeKind.OBJECT, name, description, fields)

  def makeInputObject(name: Option[String], description: Option[String], fields: List[Field]) =
    Type(TypeKind.INPUT_OBJECT, name, description, fields)

  def makeUnion(name: Option[String], description: Option[String], subTypes: List[Type]) =
    Type(TypeKind.UNION, name, description, subTypes = subTypes)

  case class Type(
    kind: TypeKind,
    name: Option[String] = None,
    description: Option[String] = None,
    fields: List[Field] = Nil,
    values: List[String] = Nil,
    subTypes: List[Type] = Nil,
    ofType: Option[Type] = None
  )

  case class Argument(name: String, description: Option[String], argumentType: Type)

  case class Field(name: String, description: Option[String], arguments: List[Argument], `type`: Type)

  def collectTypes(t: Type): Set[Type] =
    t.kind match {
      case TypeKind.SCALAR   => Set()
      case TypeKind.ENUM     => Set(t)
      case TypeKind.LIST     => t.ofType.fold(Set.empty[Type])(collectTypes)
      case TypeKind.NON_NULL => t.ofType.fold(Set.empty[Type])(collectTypes)
      case _                 =>
        // TODO recursive types
        (t :: t.fields.flatMap(f => collectTypes(f.`type`) ++ f.arguments.map(_.argumentType).flatMap(collectTypes))
          ++ t.subTypes.flatMap(s => collectTypes(s))).toSet
    }
}

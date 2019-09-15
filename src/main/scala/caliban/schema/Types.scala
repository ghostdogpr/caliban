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

  def makeEnum(name: Option[String], description: Option[String], values: List[EnumValue]) =
    Type(
      TypeKind.ENUM,
      name,
      description,
      enumValues = args => values.filter(v => args.includeDeprecated.getOrElse(false) || !v.isDeprecated)
    )

  def makeObject(name: Option[String], description: Option[String], fields: List[Field]) =
    Type(
      TypeKind.OBJECT,
      name,
      description,
      fields = args => fields.filter(v => args.includeDeprecated.getOrElse(false) || !v.isDeprecated)
    )

  def makeInputObject(name: Option[String], description: Option[String], fields: List[InputValue]) =
    Type(TypeKind.INPUT_OBJECT, name, description, inputFields = fields)

  def makeUnion(name: Option[String], description: Option[String], subTypes: List[Type]) =
    Type(TypeKind.UNION, name, description, possibleTypes = subTypes)

  case class Type(
    kind: TypeKind,
    name: Option[String] = None,
    description: Option[String] = None,
    fields: DeprecatedArgs => List[Field] = _ => Nil,
    interfaces: List[Type] = Nil,
    possibleTypes: List[Type] = Nil,
    enumValues: DeprecatedArgs => List[EnumValue] = _ => Nil,
    inputFields: List[InputValue] = Nil,
    ofType: Option[Type] = None
  )

  case class DeprecatedArgs(includeDeprecated: Option[Boolean] = None)

  case class EnumValue(
    name: String,
    description: Option[String],
    isDeprecated: Boolean,
    deprecationReason: Option[String]
  )

  case class InputValue(name: String, description: Option[String], `type`: () => Type, defaultValue: Option[String])

  case class Field(
    name: String,
    description: Option[String],
    args: List[InputValue],
    `type`: () => Type,
    isDeprecated: Boolean,
    deprecationReason: Option[String]
  )

  def collectTypes(t: Type, existingTypes: Map[String, Type] = Map()): Map[String, Type] =
    t.kind match {
      case TypeKind.SCALAR   => existingTypes
      case TypeKind.ENUM     => t.name.fold(existingTypes)(name => existingTypes.updated(name, t))
      case TypeKind.LIST     => t.ofType.fold(existingTypes)(collectTypes(_, existingTypes))
      case TypeKind.NON_NULL => t.ofType.fold(existingTypes)(collectTypes(_, existingTypes))
      case _ =>
        val map1          = t.name.fold(existingTypes)(name => existingTypes.updated(name, t))
        val embeddedTypes = t.fields(DeprecatedArgs(Some(true))).flatMap(f => f.`type` :: f.args.map(_.`type`))
        val map2 = embeddedTypes.foldLeft(map1) {
          case (types, f) =>
            val t = innerType(f())
            t.name.fold(types)(name => if (types.contains(name)) types else collectTypes(t, types.updated(name, t)))
        }
        t.possibleTypes.foldLeft(map2) { case (types, subtype) => collectTypes(subtype, types) }
    }

  def innerType(t: Type): Type = t.ofType.map(innerType).getOrElse(t)
}

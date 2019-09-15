package caliban.schema

object Types {

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

  def makeScalar(name: String) = __Type(__TypeKind.SCALAR, Some(name))

  def makeList(underlying: __Type) = __Type(__TypeKind.LIST, ofType = Some(underlying))

  def makeNonNull(underlying: __Type) = __Type(__TypeKind.NON_NULL, ofType = Some(underlying))

  def makeEnum(name: Option[String], description: Option[String], values: List[__EnumValue]) =
    __Type(
      __TypeKind.ENUM,
      name,
      description,
      enumValues = args => Some(values.filter(v => args.includeDeprecated.getOrElse(false) || !v.isDeprecated))
    )

  def makeObject(name: Option[String], description: Option[String], fields: List[__Field]) =
    __Type(
      __TypeKind.OBJECT,
      name,
      description,
      fields = args => Some(fields.filter(v => args.includeDeprecated.getOrElse(false) || !v.isDeprecated))
    )

  def makeInputObject(name: Option[String], description: Option[String], fields: List[__InputValue]) =
    __Type(__TypeKind.INPUT_OBJECT, name, description, inputFields = Some(fields))

  def makeUnion(name: Option[String], description: Option[String], subTypes: List[__Type]) =
    __Type(__TypeKind.UNION, name, description, possibleTypes = Some(subTypes))

  case class __Type(
    kind: __TypeKind,
    name: Option[String] = None,
    description: Option[String] = None,
    fields: DeprecatedArgs => Option[List[__Field]] = _ => None,
    interfaces: Option[List[__Type]] = None,
    possibleTypes: Option[List[__Type]] = None,
    enumValues: DeprecatedArgs => Option[List[__EnumValue]] = _ => None,
    inputFields: Option[List[__InputValue]] = None,
    ofType: Option[__Type] = None
  )

  case class DeprecatedArgs(includeDeprecated: Option[Boolean] = None)

  case class __EnumValue(
    name: String,
    description: Option[String],
    isDeprecated: Boolean,
    deprecationReason: Option[String]
  )

  case class __InputValue(name: String, description: Option[String], `type`: () => __Type, defaultValue: Option[String])

  case class __Field(
    name: String,
    description: Option[String],
    args: List[__InputValue],
    `type`: () => __Type,
    isDeprecated: Boolean,
    deprecationReason: Option[String]
  )

  def collectTypes(t: __Type, existingTypes: Map[String, __Type] = Map()): Map[String, __Type] =
    t.kind match {
      case __TypeKind.SCALAR   => existingTypes
      case __TypeKind.ENUM     => t.name.fold(existingTypes)(name => existingTypes.updated(name, t))
      case __TypeKind.LIST     => t.ofType.fold(existingTypes)(collectTypes(_, existingTypes))
      case __TypeKind.NON_NULL => t.ofType.fold(existingTypes)(collectTypes(_, existingTypes))
      case _ =>
        val map1 = t.name.fold(existingTypes)(name => existingTypes.updated(name, t))
        val embeddedTypes =
          t.fields(DeprecatedArgs(Some(true))).getOrElse(Nil).flatMap(f => f.`type` :: f.args.map(_.`type`))
        val map2 = embeddedTypes.foldLeft(map1) {
          case (types, f) =>
            val t = innerType(f())
            t.name.fold(types)(name => if (types.contains(name)) types else collectTypes(t, types.updated(name, t)))
        }
        t.possibleTypes.getOrElse(Nil).foldLeft(map2) { case (types, subtype) => collectTypes(subtype, types) }
    }

  def innerType(t: __Type): __Type = t.ofType.map(innerType).getOrElse(t)
}

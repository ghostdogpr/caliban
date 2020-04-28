package caliban.schema

import caliban.introspection.adt._
import caliban.parsing.adt.Directive

object Types {

  /**
   * Creates a new scalar type with the given name.
   */
  def makeScalar(name: String, description: Option[String] = None): __Type =
    __Type(__TypeKind.SCALAR, Some(name), description)

  val boolean: __Type = makeScalar("Boolean")
  val string: __Type  = makeScalar("String")
  val int: __Type     = makeScalar("Int")
  val long: __Type    = makeScalar("Long")
  val float: __Type   = makeScalar("Float")
  val double: __Type  = makeScalar("Double")

  def makeList(underlying: __Type): __Type = __Type(__TypeKind.LIST, ofType = Some(underlying))

  def makeNonNull(underlying: __Type): __Type = __Type(__TypeKind.NON_NULL, ofType = Some(underlying))

  def makeEnum(
    name: Option[String],
    description: Option[String],
    values: List[__EnumValue],
    origin: Option[String]
  ): __Type =
    __Type(
      __TypeKind.ENUM,
      name,
      description,
      enumValues = args => Some(values.filter(v => args.includeDeprecated.getOrElse(false) || !v.isDeprecated)),
      origin = origin
    )

  def makeObject(
    name: Option[String],
    description: Option[String],
    fields: List[__Field],
    directives: List[Directive],
    origin: Option[String] = None
  ): __Type =
    __Type(
      __TypeKind.OBJECT,
      name,
      description,
      fields = args => Some(fields.filter(v => args.includeDeprecated.getOrElse(false) || !v.isDeprecated)),
      interfaces = () => Some(Nil),
      directives = Some(directives),
      origin = origin
    )

  def makeInputObject(
    name: Option[String],
    description: Option[String],
    fields: List[__InputValue],
    origin: Option[String] = None
  ): __Type =
    __Type(__TypeKind.INPUT_OBJECT, name, description, inputFields = Some(fields), origin = origin)

  def makeUnion(
    name: Option[String],
    description: Option[String],
    subTypes: List[__Type],
    origin: Option[String] = None
  ): __Type =
    __Type(__TypeKind.UNION, name, description, possibleTypes = Some(subTypes), origin = origin)

  def makeInterface(
    name: Option[String],
    description: Option[String],
    fields: List[__Field],
    subTypes: List[__Type],
    origin: Option[String] = None
  ): __Type =
    __Type(
      __TypeKind.INTERFACE,
      name,
      description,
      fields = args => Some(fields.filter(v => args.includeDeprecated.getOrElse(false) || !v.isDeprecated)),
      possibleTypes = Some(subTypes),
      origin = origin
    )

  /**
   * Returns a map of all the types nested within the given root type.
   */
  def collectTypes(t: __Type, existingTypes: List[__Type] = Nil): List[__Type] =
    t.kind match {
      case __TypeKind.SCALAR | __TypeKind.ENUM =>
        t.name.fold(existingTypes)(_ => if (existingTypes.exists(same(t, _))) existingTypes else t :: existingTypes)
      case __TypeKind.LIST | __TypeKind.NON_NULL =>
        t.ofType.fold(existingTypes)(collectTypes(_, existingTypes))
      case _ =>
        val list1 =
          t.name.fold(existingTypes)(_ => if (existingTypes.exists(same(t, _))) existingTypes else t :: existingTypes)
        val embeddedTypes =
          t.fields(__DeprecatedArgs(Some(true))).getOrElse(Nil).flatMap(f => f.`type` :: f.args.map(_.`type`)) ++
            t.inputFields.getOrElse(Nil).map(_.`type`)
        val list2 = embeddedTypes.foldLeft(list1) {
          case (types, f) =>
            val t = innerType(f())
            t.name.fold(types)(_ => if (existingTypes.exists(same(t, _))) types else collectTypes(t, types))
        }
        t.possibleTypes.getOrElse(Nil).foldLeft(list2) { case (types, subtype) => collectTypes(subtype, types) }
    }

  def same(t1: __Type, t2: __Type): Boolean =
    t1.name == t2.name && t1.kind == t2.kind && (t1.origin.isEmpty || t2.origin.isEmpty || t1.origin == t2.origin)

  def innerType(t: __Type): __Type = t.ofType.fold(t)(innerType)

  def name(t: __Type): String =
    (t.kind match {
      case __TypeKind.LIST     => t.ofType.map("ListOf" + name(_))
      case __TypeKind.NON_NULL => t.ofType.map(name)
      case _                   => t.name
    }).getOrElse("")
}

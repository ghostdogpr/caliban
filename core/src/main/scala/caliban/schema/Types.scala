package caliban.schema

import caliban.introspection.adt._
import caliban.parsing.adt.Directive

import scala.annotation.tailrec

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
      enumValues =
        args => if (args.includeDeprecated.getOrElse(false)) Some(values) else Some(values.filter(!_.isDeprecated)),
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
      fields =
        args => if (args.includeDeprecated.getOrElse(false)) Some(fields) else Some(fields.filter(!_.isDeprecated)),
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
    fields: () => List[__Field],
    subTypes: List[__Type],
    origin: Option[String] = None
  ): __Type =
    __Type(
      __TypeKind.INTERFACE,
      name,
      description,
      fields =
        args => if (args.includeDeprecated.getOrElse(false)) Some(fields()) else Some(fields().filter(!_.isDeprecated)),
      possibleTypes = Some(subTypes),
      origin = origin
    )

  /**
   * Returns a map of all the types nested within the given root type.
   */
  def collectTypes(t: __Type, existingTypes: List[__Type] = Nil): List[__Type] =
    t.kind match {
      case __TypeKind.SCALAR | __TypeKind.ENUM   =>
        t.name.fold(existingTypes)(_ => if (existingTypes.exists(same(t, _))) existingTypes else t :: existingTypes)
      case __TypeKind.LIST | __TypeKind.NON_NULL =>
        t.ofType.fold(existingTypes)(collectTypes(_, existingTypes))
      case _                                     =>
        val list1         =
          t.name.fold(existingTypes)(_ =>
            if (existingTypes.exists(same(t, _))) {
              existingTypes.map {
                case ex if same(ex, t) =>
                  ex.copy(interfaces =
                    () =>
                      (ex.interfaces(), t.interfaces()) match {
                        case (None, None)              => None
                        case (Some(interfaces), None)  => Some(interfaces)
                        case (None, Some(interfaces))  => Some(interfaces)
                        case (Some(left), Some(right)) =>
                          Some(left ++ right.filterNot(t => left.exists(_.name == t.name)))
                      }
                  )
                case other             => other
              }
            } else t :: existingTypes
          )
        val embeddedTypes =
          t.fields(__DeprecatedArgs(Some(true))).getOrElse(Nil).flatMap(f => f.`type` :: f.args.map(_.`type`)) ++
            t.inputFields.getOrElse(Nil).map(_.`type`) ++
            t.interfaces().getOrElse(Nil).map(() => _)
        val list2         = embeddedTypes.foldLeft(list1) { case (types, f) =>
          val t = innerType(f())
          t.name.fold(types)(_ => if (existingTypes.exists(same(t, _))) types else collectTypes(t, types))
        }
        t.possibleTypes.getOrElse(Nil).foldLeft(list2) { case (types, subtype) => collectTypes(subtype, types) }
    }

  @tailrec
  def same(t1: __Type, t2: __Type): Boolean =
    if (t1.kind == t2.kind && t1.ofType.nonEmpty)
      same(t1.ofType.getOrElse(t1), t2.ofType.getOrElse(t2))
    else
      t1.name == t2.name && t1.kind == t2.kind && (t1.origin.isEmpty || t2.origin.isEmpty || t1.origin == t2.origin)

  def innerType(t: __Type): __Type = t.ofType.fold(t)(innerType)

  def listOf(t: __Type): Option[__Type] =
    t.kind match {
      case __TypeKind.LIST     => t.ofType
      case __TypeKind.NON_NULL => t.ofType.flatMap(listOf)
      case _                   => None
    }

  def name(t: __Type): String =
    (t.kind match {
      case __TypeKind.LIST     => t.ofType.map("ListOf" + name(_))
      case __TypeKind.NON_NULL => t.ofType.map(name)
      case _                   => t.name
    }).getOrElse("")
}

package caliban.schema

import caliban.introspection.adt._

object Types {

  def makeScalar(name: String, description: Option[String] = None) = __Type(__TypeKind.SCALAR, Some(name), description)

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
      fields = args => Some(fields.filter(v => args.includeDeprecated.getOrElse(false) || !v.isDeprecated)),
      interfaces = Some(Nil)
    )

  def makeInputObject(name: Option[String], description: Option[String], fields: List[__InputValue]) =
    __Type(__TypeKind.INPUT_OBJECT, name, description, inputFields = Some(fields))

  def makeUnion(name: Option[String], description: Option[String], subTypes: List[__Type]) =
    __Type(__TypeKind.UNION, name, description, possibleTypes = Some(subTypes))

  def collectTypes(t: __Type, existingTypes: Map[String, __Type] = Map()): Map[String, __Type] =
    t.kind match {
      case __TypeKind.SCALAR | __TypeKind.ENUM   => t.name.fold(existingTypes)(name => existingTypes.updated(name, t))
      case __TypeKind.LIST | __TypeKind.NON_NULL => t.ofType.fold(existingTypes)(collectTypes(_, existingTypes))
      case _ =>
        val map1 = t.name.fold(existingTypes)(name => existingTypes.updated(name, t))
        val embeddedTypes =
          t.fields(__DeprecatedArgs(Some(true))).getOrElse(Nil).flatMap(f => f.`type` :: f.args.map(_.`type`))
        val map2 = embeddedTypes.foldLeft(map1) {
          case (types, f) =>
            val t = innerType(f())
            t.name.fold(types)(name => if (types.contains(name)) types else collectTypes(t, types))
        }
        t.possibleTypes.getOrElse(Nil).foldLeft(map2) { case (types, subtype) => collectTypes(subtype, types) }
    }

  def innerType(t: __Type): __Type = t.ofType.map(innerType).getOrElse(t)
}

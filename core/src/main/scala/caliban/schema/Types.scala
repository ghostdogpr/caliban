package caliban.schema

import caliban.introspection.adt._
import caliban.parsing.adt.{ Directive, Directives }

import scala.annotation.tailrec

object Types {

  /**
   * Creates a new scalar type with the given name.
   */
  def makeScalar(
    name: String,
    description: Option[String] = None,
    specifiedByURL: Option[String] = None,
    directives: Option[List[Directive]] = None
  ): __Type =
    __Type(__TypeKind.SCALAR, Some(name), description, specifiedByURL = specifiedByURL, directives = directives)

  val boolean: __Type = makeScalar("Boolean")
  val string: __Type  = makeScalar("String")
  val int: __Type     = makeScalar("Int")
  val long: __Type    = makeScalar("Long")
  val float: __Type   = makeScalar("Float")
  val double: __Type  = makeScalar("Double")

  private[caliban] val builtInScalars: Map[String, __Type] =
    Map("Int" -> int, "Float" -> float, "String" -> string, "Boolean" -> boolean, "ID" -> makeScalar("ID"))

  def makeEnum(
    name: Option[String],
    description: Option[String],
    values: List[__EnumValue],
    origin: Option[String],
    directives: Option[List[Directive]] = None
  ): __Type =
    __Type(
      __TypeKind.ENUM,
      name,
      description,
      enumValues = args => Some(args.filter(values)(_.isDeprecated)),
      origin = origin,
      directives = directives
    )

  def makeObject(
    name: Option[String],
    description: Option[String],
    fields: List[__Field],
    directives: List[Directive],
    origin: Option[String] = None,
    interfaces: () => Option[List[__Type]] = () => Some(Nil)
  ): __Type =
    __Type(
      __TypeKind.OBJECT,
      name,
      description,
      fields = args => Some(args.filter(fields)(_.isDeprecated)),
      interfaces = interfaces,
      directives = Some(directives),
      origin = origin
    )

  def makeField(
    name: String,
    description: Option[String],
    arguments: List[__InputValue],
    `type`: () => __Type,
    isDeprecated: Boolean = false,
    deprecationReason: Option[String] = None,
    directives: Option[List[Directive]] = None
  ): __Field =
    __Field(
      name,
      description,
      _.filter(arguments)(_.isDeprecated),
      `type`,
      isDeprecated,
      deprecationReason,
      directives
    )

  def makeInputObject(
    name: Option[String],
    description: Option[String],
    fields: List[__InputValue],
    origin: Option[String] = None,
    directives: Option[List[Directive]] = None
  ): __Type =
    __Type(
      __TypeKind.INPUT_OBJECT,
      name,
      description,
      inputFields = args => Some(args.filter(fields)(_.isDeprecated)),
      origin = origin,
      directives = directives,
      isOneOf = Some(Directives.isOneOf(directives.getOrElse(Nil)))
    )

  def makeUnion(
    name: Option[String],
    description: Option[String],
    subTypes: List[__Type],
    origin: Option[String] = None,
    directives: Option[List[Directive]] = None
  ): __Type =
    __Type(
      __TypeKind.UNION,
      name,
      description,
      possibleTypes = Some(subTypes),
      origin = origin,
      directives = directives
    )

  def makeInterface(
    name: Option[String],
    description: Option[String],
    fields: () => List[__Field],
    subTypes: List[__Type],
    origin: Option[String] = None,
    directives: Option[List[Directive]] = None
  ): __Type =
    __Type(
      __TypeKind.INTERFACE,
      name,
      description,
      fields = args => Some(args.filter(fields())(_.isDeprecated)),
      interfaces = () => Some(Nil),
      possibleTypes = Some(subTypes),
      origin = origin,
      directives = directives
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
          t.allFields.flatMap(f => f._type :: f.allArgs.map(_._type)) ++
            t.allInputFields.map(_._type) ++
            t.interfaces().getOrElse(Nil)
        val list2         = embeddedTypes.foldLeft(list1) { case (types, f) =>
          val t = innerType(f)
          t.name.fold(types)(_ => if (existingTypes.exists(same(t, _))) types else collectTypes(t, types))
        }
        t.possibleTypes.getOrElse(Nil).foldLeft(list2) { case (types, subtype) => collectTypes(subtype, types) }
    }

  private[caliban] def collectRootTypes(additionalTypes: List[__Type], roots: List[__Type]): List[__Type] = {
    val init = additionalTypes.foldLeft(List.empty[__Type]) { case (types, tpe) => collectTypes(tpe, types) }
    (init ++ roots.flatMap(collectTypes(_, init)))
      .groupBy(tpe => (tpe.name, tpe.kind, tpe.origin))
      .flatMap(_._2.headOption)
      .toList
  }

  /**
   * Tries to find a common widened type among a list of fields.
   *
   * @param l a list of fields to unify
   * @return the unified type if one could be found
   */
  def unify(l: List[__Field]): Option[__Type] =
    l.headOption.flatMap { first =>
      val args                            = first.allArgs
      def _unify(f2: __Field)(t1: __Type) =
        if (
          args.length == f2.allArgs.length &&
          args.forall(a => f2.allArgs.exists(b => a.name == b.name && same(a._type, b._type)))
        )
          unify(t1, f2._type)
        else None

      l.drop(1).foldLeft(Option(first._type))((acc, t) => acc.flatMap(_unify(t)))
    }

  /**
   * Tries to unify two types by widening them to a common supertype.
   *
   * @example {{{unify(string, makeNonNull(string)) // => Some(__Type(SCALAR, Some("String")))}}}
   * @param t1 type second type to unify
   * @param t2 the first type to unify
   * @return the unified type if one could be found
   */
  def unify(t1: __Type, t2: __Type): Option[__Type] =
    if (same(t1, t2)) Option(t1)
    else
      (t1.kind, t2.kind) match {
        case (__TypeKind.NON_NULL, __TypeKind.NON_NULL) =>
          for {
            a <- t1.ofType
            b <- t2.ofType
            u <- unify(a, b)
          } yield u.nonNull
        case (__TypeKind.NON_NULL, _)                   => t1.ofType.flatMap(unify(_, t2))
        case (_, __TypeKind.NON_NULL)                   => t2.ofType.flatMap(unify(_, t1))
        case _                                          => commonInterface(t1, t2)
      }

  /**
   * Finds the closest type reachable from both `t1` and `t2` via their `interfaces()` chain
   * (including each type itself, so that a direct implementer/interface pair returns the
   * interface). BFS order is used so the closest match wins.
   *
   * Used by [[unify]] as a fallback when two types differ but covariantly narrow a shared interface
   * field (per https://spec.graphql.org/October2021/#IsValidImplementationFieldType()).
   */
  private def commonInterface(t1: __Type, t2: __Type): Option[__Type] = {
    val a2 = withAncestors(List(t2))
    withAncestors(List(t1)).find(i1 => a2.exists(same(i1, _)))
  }

  /**
   * Returns `types` followed by every type reachable through their `interfaces()` chain, in BFS order and without
   * duplicates, so cyclic interface graphs terminate.
   */
  private[caliban] def withAncestors(types: List[__Type]): List[__Type] = {
    @tailrec
    def loop(queue: List[__Type], acc: List[__Type]): List[__Type] = queue match {
      case Nil                                 => acc.reverse
      case h :: rest if acc.exists(same(_, h)) => loop(rest, acc)
      case h :: rest                           => loop(rest ::: h.interfaces().toList.flatten, h :: acc)
    }
    loop(types, Nil)
  }

  def extractCommonDescription(l: List[__Field]): Option[String] =
    l.map(_.description).distinct match {
      case desc :: Nil => desc
      case _           => None
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

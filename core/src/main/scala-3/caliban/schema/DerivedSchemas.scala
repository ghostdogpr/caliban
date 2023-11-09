package caliban.schema

import caliban.Value.EnumValue
import caliban.introspection.adt.*
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.*
import caliban.schema.DerivationUtils.*
import caliban.schema.Step.{ MetadataFunctionStep, ObjectStep }
import caliban.schema.Types.*
import magnolia1.TypeInfo

final private class ValueTypeSchema[R, A](
  _schema: => Schema[R, Any],
  info: TypeInfo,
  anns: List[Any]
) extends Schema[R, A] {
  private val name = getName(anns, info)

  private lazy val schema = _schema

  def toType(isInput: Boolean, isSubscription: Boolean): __Type =
    if (anns.contains(GQLValueType(true))) makeScalar(name, getDescription(anns))
    else schema.toType_(isInput, isSubscription)

  def resolve(value: A): Step[R] = schema.resolve(value.asInstanceOf[Product].productElement(0))
}

final private class EnumValueSchema[R, A](
  info: TypeInfo,
  anns: List[Any]
) extends Schema[R, A] {

  def toType(isInput: Boolean, isSubscription: Boolean): __Type =
    if (isInput) mkInputObject[R](anns, Nil, info)(isInput, isSubscription)
    else mkObject[R](anns, Nil, info)(isInput, isSubscription)

  private val step               = PureStep(EnumValue(getName(anns, info)))
  def resolve(value: A): Step[R] = step
}

final private class ObjectSchema[R, A](
  _fields: => List[(String, Schema[R, Any], Int)],
  info: TypeInfo,
  anns: List[Any],
  paramAnnotations: Map[String, List[Any]]
) extends Schema[R, A] {
  private val name = getName(anns, info)

  private lazy val fields = _fields.map { (label, schema, index) =>
    val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
    (getName(fieldAnnotations, label), fieldAnnotations, schema, index)
  }

  def toType(isInput: Boolean, isSubscription: Boolean): __Type =
    if (isInput) mkInputObject[R](anns, fields, info)(isInput, isSubscription)
    else mkObject[R](anns, fields, info)(isInput, isSubscription)

  def resolve(value: A): Step[R] = MetadataFunctionStep[R] { f =>
    val fb = Map.newBuilder[String, Step[R]]

    var remaining = fields
    while (!remaining.isEmpty) {
      val (name, _, schema, i) = remaining.head
      if (f.fieldNames.contains(name)) fb += name -> schema.resolve(value.asInstanceOf[Product].productElement(i))
      remaining = remaining.tail
    }

    ObjectStep(name, fb.result())
  }
}

final private class SumSchema[R, A](
  _members: => List[(String, List[Any], Schema[R, Any])],
  info: TypeInfo,
  annotations: List[Any]
)(ordinal: A => Int)
    extends Schema[R, A] {
  private lazy val members = _members

  private lazy val subTypes = members.map { (label, subTypeAnnotations, schema) =>
    (label, schema.toType_(), subTypeAnnotations)
  }.sortBy(_._1)

  private lazy val schemas = members.map(_._3).toVector // Vector's .apply is O(1) vs List's O(N)

  private lazy val isEnum = subTypes.forall { (_, t, _) =>
    t.allFields.isEmpty && t.allInputFields.isEmpty
  }

  private val isInterface = annotations.contains(GQLInterface())
  private val isUnion     = annotations.contains(GQLUnion())

  def toType(isInput: Boolean, isSubscription: Boolean): __Type =
    if (!isInterface && !isUnion && subTypes.nonEmpty && isEnum) mkEnum(annotations, info, subTypes)
    else if (!isInterface)
      makeUnion(
        Some(getName(annotations, info)),
        getDescription(annotations),
        subTypes.map(_._2).distinctBy(_.name).map(fixEmptyUnionObject),
        Some(info.full),
        Some(getDirectives(annotations))
      )
    else {
      val impl = subTypes.map(_._2.copy(interfaces = () => Some(List(toType(isInput, isSubscription)))))
      mkInterface(annotations, info, impl)
    }

  def resolve(value: A): Step[R] = schemas(ordinal(value)).resolve(value)
}

private object DerivationUtils {

  /**
   * Default naming logic for input types.
   * This is needed to avoid a name clash between a type used as an input and the same type used as an output.
   * GraphQL needs 2 different types, and they can't have the same name.
   * By default, the "Input" suffix is added after the type name, given that it is not already present.
   */
  def customizeInputTypeName(name: String): String =
    if (name.endsWith("Input")) name
    else s"${name}Input"

  // see https://github.com/graphql/graphql-spec/issues/568
  def fixEmptyUnionObject(t: __Type): __Type =
    t.fields(__DeprecatedArgs(Some(true))) match {
      case Some(Nil) =>
        t.copy(
          fields = (_: __DeprecatedArgs) =>
            Some(
              List(
                __Field(
                  "_",
                  Some(
                    "Fake field because GraphQL does not support empty objects. Do not query, use __typename instead."
                  ),
                  _ => Nil,
                  () => makeScalar("Boolean")
                )
              )
            )
        )
      case _         => t
    }

  def getName(annotations: Seq[Any], info: TypeInfo): String =
    annotations.collectFirst { case GQLName(name) => name }.getOrElse {
      info.typeParams match {
        case Nil  => info.short
        case args => info.short + args.map(getName(Nil, _)).mkString
      }
    }

  def getName(annotations: Seq[Any], label: String): String =
    annotations.collectFirst { case GQLName(name) => name }.getOrElse(label)

  def getInputName(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLInputName(suffix) => suffix }

  def getDescription(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLDescription(desc) => desc }

  def getDirectives(annotations: Seq[Any]): List[Directive] =
    annotations.collect { case GQLDirective(dir) => dir }.toList

  def getDefaultValue(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLDefault(v) => v }

  def getDeprecatedReason(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLDeprecated(reason) => reason }

  def mkEnum(annotations: List[Any], info: TypeInfo, subTypes: List[(String, __Type, List[Any])]): __Type =
    makeEnum(
      Some(getName(annotations, info)),
      getDescription(annotations),
      subTypes.collect { case (name, __Type(_, _, description, _, _, _, _, _, _, _, _, _), annotations) =>
        __EnumValue(
          getName(annotations, name),
          description,
          getDeprecatedReason(annotations).isDefined,
          getDeprecatedReason(annotations),
          Some(annotations.collect { case GQLDirective(dir) => dir }.toList).filter(_.nonEmpty)
        )
      },
      Some(info.full),
      Some(getDirectives(annotations))
    )

  def mkInterface(
    annotations: List[Any],
    info: TypeInfo,
    impl: List[__Type]
  ): __Type = {
    val commonFields = () =>
      impl
        .flatMap(_.allFields)
        .groupBy(_.name)
        .collect {
          case (_, list) if list.lengthCompare(impl.size) == 0 =>
            Types
              .unify(list)
              .flatMap(t =>
                list.headOption.map(_.copy(description = Types.extractCommonDescription(list), `type` = () => t))
              )
        }
        .flatten
        .toList
        .sortBy(_.name)

    makeInterface(
      Some(getName(annotations, info)),
      getDescription(annotations),
      commonFields,
      impl,
      Some(info.full),
      Some(getDirectives(annotations))
    )
  }

  def mkInputObject[R](
    annotations: List[Any],
    fields: List[(String, List[Any], Schema[R, Any], Int)],
    info: TypeInfo
  )(isInput: Boolean, isSubscription: Boolean): __Type = makeInputObject(
    Some(getInputName(annotations).getOrElse(customizeInputTypeName(getName(annotations, info)))),
    getDescription(annotations),
    fields.map { (name, fieldAnnotations, schema, _) =>
      __InputValue(
        name,
        getDescription(fieldAnnotations),
        () =>
          if (schema.optional) schema.toType_(isInput, isSubscription)
          else schema.toType_(isInput, isSubscription).nonNull,
        getDefaultValue(fieldAnnotations),
        getDeprecatedReason(fieldAnnotations).isDefined,
        getDeprecatedReason(fieldAnnotations),
        Some(getDirectives(fieldAnnotations)).filter(_.nonEmpty)
      )
    },
    Some(info.full),
    Some(getDirectives(annotations))
  )

  def mkObject[R](
    annotations: List[Any],
    fields: List[(String, List[Any], Schema[R, Any], Int)],
    info: TypeInfo
  )(isInput: Boolean, isSubscription: Boolean): __Type = makeObject(
    Some(getName(annotations, info)),
    getDescription(annotations),
    fields.map { (name, fieldAnnotations, schema, _) =>
      val deprecatedReason = getDeprecatedReason(fieldAnnotations)
      Types.makeField(
        name,
        getDescription(fieldAnnotations),
        schema.arguments,
        () =>
          if (schema.optional) schema.toType_(isInput, isSubscription)
          else schema.toType_(isInput, isSubscription).nonNull,
        deprecatedReason.isDefined,
        deprecatedReason,
        Option(getDirectives(fieldAnnotations)).filter(_.nonEmpty)
      )
    },
    getDirectives(annotations),
    Some(info.full)
  )

}

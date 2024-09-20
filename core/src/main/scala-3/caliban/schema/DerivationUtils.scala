package caliban.schema

import caliban.introspection.adt.*
import caliban.parsing.adt.{ Directive, Directives }
import caliban.schema.Annotations.*
import caliban.schema.Types.*
import caliban.schema.macros.Macros
import magnolia1.{ Macro as MagnoliaMacro, TypeInfo }

import scala.compiletime.erasedValue

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

  transparent inline def isValueType[A, Labels]: Boolean =
    inline erasedValue[Labels] match {
      case _: EmptyTuple => false
      case _             => MagnoliaMacro.isValueClass[A] || Macros.hasAnnotation[A, GQLValueType]
    }

  def mkEnum(annotations: List[Any], info: TypeInfo, subTypes: List[(String, __Type, List[Any])]): __Type =
    makeEnum(
      Some(getName(annotations, info)),
      getDescription(annotations),
      subTypes.map { case (name, __Type(_, _, description, _, _, _, _, _, _, _, _, _, _), annotations) =>
        __EnumValue(
          getName(annotations, name),
          description,
          getDeprecatedReason(annotations).isDefined,
          getDeprecatedReason(annotations),
          Some(annotations.collect { case GQLDirective(dir) => dir }).filter(_.nonEmpty)
        )
      }.sortBy(_.name),
      Some(info.full),
      Some(getDirectives(annotations))
    )

  def mkInterface(
    annotations: List[Any],
    info: TypeInfo,
    impl: List[__Type]
  ): __Type = {
    val excl         = annotations.collectFirst { case i: GQLInterface => i.excludedFields.toSet }.getOrElse(Set.empty)
    val commonFields = () =>
      impl
        .flatMap(_.allFields)
        .groupBy(_.name)
        .collect {
          case (name, list) if list.lengthCompare(impl.size) == 0 && !excl.contains(name) =>
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
    fields: List[(String, List[Any], Schema[R, Any])],
    info: TypeInfo
  )(isInput: Boolean, isSubscription: Boolean): __Type = {
    lazy val tpe: __Type = makeInputObject(
      Some(getInputName(annotations).getOrElse(customizeInputTypeName(getName(annotations, info)))),
      getDescription(annotations),
      fields.map { (name, fieldAnnotations, schema) =>
        val deprecationReason = getDeprecatedReason(fieldAnnotations)
        __InputValue(
          name,
          description = getDescription(fieldAnnotations),
          `type` = () =>
            if (schema.optional) schema.toType_(isInput, isSubscription)
            else schema.toType_(isInput, isSubscription).nonNull,
          defaultValue = getDefaultValue(fieldAnnotations),
          isDeprecated = deprecationReason.isDefined,
          deprecationReason = deprecationReason,
          directives = Some(getDirectives(fieldAnnotations)).filter(_.nonEmpty),
          parentType = () => Some(tpe)
        )
      },
      Some(info.full),
      Some(getDirectives(annotations))
    )
    tpe
  }

  def mkOneOfInput[R](
    annotations: List[Any],
    schemas: List[Schema[R, Any]],
    info: TypeInfo
  ): __Type =
    makeInputObject(
      Some(getInputName(annotations).getOrElse(customizeInputTypeName(getName(annotations, info)))),
      getDescription(annotations),
      schemas.flatMap { s =>
        val pTpe = s.toType_(isInput = true)
        pTpe.allInputFields.map { t =>
          t.nullable.copy(
            description = t.description.orElse(pTpe.description),
            directives = t.directives.orElse(pTpe.directives)
          )
        }
      }.sortBy(_.name),
      Some(info.full),
      Some(List(Directive(Directives.OneOf)))
    )

  def mkObject[R](
    annotations: List[Any],
    fields: List[(String, List[Any], Schema[R, Any])],
    info: TypeInfo,
    enableSemanticNonNull: Boolean
  )(isInput: Boolean, isSubscription: Boolean): __Type = makeObject(
    Some(getName(annotations, info)),
    getDescription(annotations),
    fields.map { (name, fieldAnnotations, schema) =>
      val deprecatedReason                = getDeprecatedReason(fieldAnnotations)
      val (isNullable, isSemanticNonNull) = {
        val hasNullableAnn = fieldAnnotations.contains(GQLNullable())
        val hasNonNullAnn  = fieldAnnotations.contains(GQLNonNullable())

        if (hasNonNullAnn) (false, false)
        else if (hasNullableAnn) (true, false)
        else if (schema.optional) (true, !schema.nullable)
        else (false, false)
      }
      Types.makeField(
        name,
        getDescription(fieldAnnotations),
        schema.arguments,
        () =>
          if (isNullable) schema.toType_(isInput, isSubscription)
          else schema.toType_(isInput, isSubscription).nonNull,
        deprecatedReason.isDefined,
        deprecatedReason,
        Option(
          getDirectives(fieldAnnotations) ++ {
            if (enableSemanticNonNull && isSemanticNonNull) Some(SchemaUtils.SemanticNonNull)
            else None
          }
        ).filter(_.nonEmpty)
      )
    },
    getDirectives(annotations),
    Some(info.full)
  )

}

package caliban.schema

import caliban.Rendering
import caliban.Value.EnumValue
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import caliban.schema.Annotations._
import caliban.schema.Step.ObjectStep
import caliban.schema.Types._
import caliban.schema.macros.{Macros, TypeInfo}

import scala.deriving.Mirror
import scala.compiletime._

trait SchemaDerivation[R] {

  /**
   * Default naming logic for input types.
   * This is needed to avoid a name clash between a type used as an input and the same type used as an output.
   * GraphQL needs 2 different types, and they can't have the same name.
   * By default, we add the "Input" suffix after the type name.
   */
  def customizeInputTypeName(name: String): String = s"${name}Input"

  inline def recurse[Label, A <: Tuple](index: Int = 0): List[(String, List[Any], TypeInfo, Schema[R, Any], Int)] =
    inline erasedValue[(Label, A)] match {
      case (_: (name *: names), _: (t *: ts)) =>
        val label = constValue[name].toString
        val annotations = Macros.annotations[t]
        val info = Macros.typeInfo[t]
        val builder = summonInline[Schema[R, t]].asInstanceOf[Schema[R, Any]]
        (label, annotations, info, builder, index) :: recurse[names, ts](index + 1)
      case (_: EmptyTuple, _) => Nil
    }

  inline def derived[A]: Schema[R, A] =
    inline summonInline[Mirror.Of[A]] match {
      case m: Mirror.SumOf[A] =>
        lazy val subTypes = recurse[m.MirroredElemLabels, m.MirroredElemTypes]()
        lazy val info = Macros.typeInfo[A]
        lazy val annotations = Macros.annotations[A]
        new Schema[R, A] {
          def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
            val subtypes =
              subTypes
                .map { case (_, subTypeAnnotations, _, schema, _) => schema.toType_() -> subTypeAnnotations }
                .sortBy { case (tpe, _) =>
                  tpe.name.getOrElse("")
                }
            val isEnum = subtypes.forall {
              case (t, _)
                if t.fields(__DeprecatedArgs(Some(true))).forall(_.isEmpty)
                  && t.inputFields.forall(_.isEmpty) =>
                true
              case _ => false
            }
            if (isEnum && subtypes.nonEmpty)
              makeEnum(
                Some(getName(annotations, info)),
                getDescription(annotations),
                subtypes.collect { case (__Type(_, Some(name), description, _, _, _, _, _, _, _, _), annotations) =>
                  __EnumValue(
                    name,
                    description,
                    annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
                    annotations.collectFirst { case GQLDeprecated(reason) => reason }
                  )
                },
                Some(info.full)
              )
            else {
              annotations.collectFirst { case GQLInterface() =>
                ()
              }.fold(
                makeUnion(
                  Some(getName(annotations, info)),
                  getDescription(annotations),
                  subtypes.map { case (t, _) => fixEmptyUnionObject(t) },
                  Some(info.full)
                )
              ) { _ =>
                val impl = subtypes.map(_._1.copy(interfaces = () => Some(List(toType(isInput, isSubscription)))))
                val commonFields = impl
                  .flatMap(_.fields(__DeprecatedArgs(Some(true))))
                  .flatten
                  .groupBy(_.name)
                  .collect {
                    case (name, list)
                      if impl.forall(_.fields(__DeprecatedArgs(Some(true))).getOrElse(Nil).exists(_.name == name)) &&
                        list.map(t => Types.name(t.`type`())).distinct.length == 1 =>
                      list.headOption
                  }
                  .flatten

                makeInterface(Some(getName(annotations, info)), getDescription(annotations), commonFields.toList, impl, Some(info.full))
              }
            }
          }

          def resolve(value: A): Step[R] = {
            val (_, _, _, schema, _) = subTypes(m.ordinal(value))
            schema.resolve(value)
          }
        }
      case m: Mirror.ProductOf[A] =>
        lazy val fields = recurse[m.MirroredElemLabels, m.MirroredElemTypes]()
        lazy val isValueClass = Macros.isValueClass[A]
        lazy val isObject = Macros.isObject[A]
        lazy val info = Macros.typeInfo[A]
        lazy val annotations = Macros.annotations[A]
        lazy val paramAnnotations = Macros.paramAnnotations[A].toMap
        new Schema[R, A] {
          def toType(isInput: Boolean, isSubscription: Boolean): __Type =
            if (isValueClass && fields.nonEmpty) fields.head._4.toType_(isInput, isSubscription)
            else if (isInput)
              makeInputObject(
                Some(annotations.collectFirst { case GQLInputName(suffix) => suffix }
                  .getOrElse(customizeInputTypeName(getName(annotations, info)))),
                getDescription(annotations),
                fields
                  .map { case (label, _, _, schema, _) =>
                    val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
                    __InputValue(
                      getName(paramAnnotations.getOrElse(label, Nil), label),
                      getDescription(fieldAnnotations),
                      () =>
                        if (schema.optional) schema.toType_(isInput, isSubscription)
                        else makeNonNull(schema.toType_(isInput, isSubscription)),
                      None,
                      Some(fieldAnnotations.collect { case GQLDirective(dir) => dir }).filter(_.nonEmpty)
                    )
                  },
                Some(info.full)
              )
            else
              makeObject(
                Some(getName(annotations, info)),
                getDescription(annotations),
                fields
                  .map { case (label, _, _, schema, _) =>
                    val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
                    __Field(
                      getName(fieldAnnotations, label),
                      getDescription(fieldAnnotations),
                      schema.arguments,
                      () =>
                        if (schema.optional) schema.toType_(isInput, isSubscription)
                        else makeNonNull(schema.toType_(isInput, isSubscription)),
                      fieldAnnotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
                      fieldAnnotations.collectFirst { case GQLDeprecated(reason) => reason },
                      Option(fieldAnnotations.collect { case GQLDirective(dir) => dir }).filter(_.nonEmpty)
                    )
                  },
                getDirectives(annotations),
                Some(info.full)
              )

          def resolve(value: A): Step[R] =
            if (isObject) PureStep(EnumValue(getName(annotations, info)))
            else if (isValueClass && fields.nonEmpty) {
              val (_, _, _, schema, index) = fields.head
              schema.resolve(value.asInstanceOf[Product].productElement(index))
            } else {
              val fieldsBuilder = Map.newBuilder[String, Step[R]]
              fields.foreach { case (label, _, _, schema, index) =>
                val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
                fieldsBuilder += getName(fieldAnnotations, label) -> schema.resolve(value.asInstanceOf[Product].productElement(index))
              }
              ObjectStep(getName(annotations, info), fieldsBuilder.result())
            }
        }
    }

  // see https://github.com/graphql/graphql-spec/issues/568
  private def fixEmptyUnionObject(t: __Type): __Type =
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
                  Nil,
                  () => makeScalar("Boolean")
                )
              )
            )
        )
      case _         => t
    }

  private def getName(annotations: Seq[Any], info: TypeInfo): String =
    annotations.collectFirst { case GQLName(name) => name }.getOrElse {
      info.typeParams match {
        case Nil  => info.short
        case args => info.short + args.map(getName(Nil, _)).mkString
      }
    }

  private def getName(annotations: Seq[Any], label: String): String =
    annotations.collectFirst { case GQLName(name) => name }.getOrElse(label)

  private def getDescription(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLDescription(desc) => desc }

  private def getDirectives(annotations: Seq[Any]): List[Directive] =
    annotations.collect { case GQLDirective(dir) => dir }.toList

  inline given gen[A]: Schema[R, A] = derived
}

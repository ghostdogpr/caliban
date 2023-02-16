package caliban.schema

import caliban.Value.EnumValue
import caliban.introspection.adt.*
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.*
import caliban.schema.Step.ObjectStep
import caliban.schema.Types.*
import caliban.schema.macros.{Macros, TypeInfo}

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.*

object PrintDerived {
  import scala.quoted.*
  inline def apply[T](inline any: T): T                                = ${ printDerived('any) }
  def printDerived[T: Type](any: Expr[T])(using qctx: Quotes): Expr[T] = {
    import qctx.reflect.*
    println(Printer.TreeShortCode.show(any.asTerm))
    any
  }
}

trait SchemaDerivation[A] {

  /**
   * Default naming logic for input types.
   * This is needed to avoid a name clash between a type used as an input and the same type used as an output.
   * GraphQL needs 2 different types, and they can't have the same name.
   * By default, we add the "Input" suffix after the type name.
   */
  def customizeInputTypeName(name: String): String = s"${name}Input"

  inline def recurse[R, Label, A <: Tuple](index: Int = 0): List[(String, List[Any], Schema[R, Any], Int)] =
    inline erasedValue[(Label, A)] match {
      case (_: (name *: names), _: (t *: ts)) =>
        val label       = constValue[name].toString
        val annotations = Macros.annotations[t]
        val builder     = summonInline[Schema[R, t]].asInstanceOf[Schema[R, Any]]
        (label, annotations, builder, index) :: recurse[R, names, ts](index + 1)
      case (_: EmptyTuple, _)                 => Nil
    }

  inline def derived[R, A]: Schema[R, A] =
    inline summonInline[Mirror.Of[A]] match {
      case m: Mirror.SumOf[A]     =>
        lazy val members     = recurse[R, m.MirroredElemLabels, m.MirroredElemTypes]()
        lazy val info        = Macros.typeInfo[A]
        lazy val annotations = Macros.annotations[A]
        lazy val subTypes    =
          members.map { case (label, subTypeAnnotations, schema, _) =>
            (label, schema.toType_(), subTypeAnnotations)
          }.sortBy { case (label, _, _) => label }

        lazy val isEnum = subTypes.forall {
          case (_, t, _)
              if t.fields(__DeprecatedArgs(Some(true))).forall(_.isEmpty)
                && t.inputFields.forall(_.isEmpty) =>
            true
          case _ => false
        }

        new Schema[R, A] {
          def toType(isInput: Boolean, isSubscription: Boolean): __Type =
            if (
              Macros.hasFields[m.MirroredElemLabels]
              && !Macros.isInterface[A]
              && !Macros.isUnion[A]
              && isEnum
            ) {
              makeEnum(
                Some(getName(annotations, info)),
                getDescription(annotations),
                subTypes.collect { case (name, __Type(_, _, description, _, _, _, _, _, _, _, _, _), annotations) =>
                  __EnumValue(
                    name,
                    description,
                    getDeprecatedReason(annotations).isDefined,
                    getDeprecatedReason(annotations)
                  )
                },
                Some(info.full),
                Some(getDirectives(annotations))
              )
            } else if (!Macros.isInterface[A]) {
              makeUnion(
                Some(getName(annotations, info)),
                getDescription(annotations),
                subTypes.map { case (_, t, _) => fixEmptyUnionObject(t) },
                Some(info.full),
                Some(getDirectives(annotations))
              )
            } else {
              val impl         = subTypes.map(_._2.copy(interfaces = () => Some(List(toType(isInput, isSubscription)))))
              val commonFields = () =>
                impl
                  .flatMap(_.fields(__DeprecatedArgs(Some(true))))
                  .flatten
                  .groupBy(_.name)
                  .filter { case (name, list) => list.lengthCompare(impl.size) == 0 }
                  .collect { case (name, list) =>
                    Types
                      .unify(list.map(_.`type`()))
                      .flatMap(t => list.headOption.map(_.copy(`type` = () => t)))
                  }
                  .flatten
                  .toList

              makeInterface(
                Some(getName(annotations, info)),
                getDescription(annotations),
                commonFields,
                impl,
                Some(info.full),
                Some(getDirectives(annotations))
              )
            }

          def resolve(value: A): Step[R] = {
            val (label, _, schema, _) = members(m.ordinal(value))
            if (isEnum) PureStep(EnumValue(label)) else schema.resolve(value)
          }
        }
      case m: Mirror.ProductOf[A] =>
        lazy val annotations      = Macros.annotations[A]
        lazy val fields           = recurse[R, m.MirroredElemLabels, m.MirroredElemTypes]()
        lazy val info             = Macros.typeInfo[A]
        lazy val paramAnnotations = Macros.paramAnnotations[A].toMap

        new Schema[R, A] {
          def toType(isInput: Boolean, isSubscription: Boolean): __Type =
            inline if (Macros.isValueType[A] && Macros.hasFields[m.MirroredElemLabels])
              inline if (Macros.isScalarValueType[A])
                makeScalar(getName(annotations, info), getDescription(annotations))
              else fields.head._3.toType_(isInput, isSubscription)
            else if (isInput) {
              mkInputObject[R](annotations, fields, info, paramAnnotations)(isInput, isSubscription)
            } else {
              mkObject[R](annotations, fields, info, paramAnnotations)(isInput, isSubscription)
            }

          def resolve(value: A): Step[R] =
            inline if (!Macros.hasFields[m.MirroredElemLabels]) PureStep(EnumValue(getName(annotations, info)))
            else inline if (Macros.isValueType[A]) {
              val head = fields.head
              head._3.resolve(value.asInstanceOf[Product].productElement(head._4))
            } else {
              val fieldsBuilder = Map.newBuilder[String, Step[R]]
              fields.foreach { case (label, _, schema, index) =>
                val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
                fieldsBuilder += getName(fieldAnnotations, label) -> schema.resolve(
                  value.asInstanceOf[Product].productElement(index)
                )
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

  private def getInputName(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLInputName(suffix) => suffix }

  private def getDescription(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLDescription(desc) => desc }

  private def getDirectives(annotations: Seq[Any]): List[Directive] =
    annotations.collect { case GQLDirective(dir) => dir }.toList

  private def getDefaultValue(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLDefault(v) => v }

  private def getDeprecatedReason(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLDeprecated(reason) => reason }

  private def mkInputObject[R](
    annotations: List[Any],
    fields: List[(String, List[Any], Schema[R, Any], Int)],
    info: TypeInfo,
    paramAnnotations: Map[String, List[Any]]
  )(isInput: Boolean, isSubscription: Boolean) = makeInputObject(
    Some(getInputName(annotations).getOrElse(customizeInputTypeName(getName(annotations, info)))),
    getDescription(annotations),
    fields.map { (label, _, schema, _) =>
      val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
      __InputValue(
        getName(fieldAnnotations, label),
        getDescription(fieldAnnotations),
        () =>
          if (schema.optional) schema.toType_(isInput, isSubscription)
          else schema.toType_(isInput, isSubscription).nonNull,
        getDefaultValue(fieldAnnotations),
        Some(getDirectives(fieldAnnotations)).filter(_.nonEmpty)
      )
    },
    Some(info.full),
    Some(getDirectives(annotations))
  )

  private def mkObject[R](
    annotations: List[Any],
    fields: List[(String, List[Any], Schema[R, Any], Int)],
    info: TypeInfo,
    paramAnnotations: Map[String, List[Any]]
  )(isInput: Boolean, isSubscription: Boolean) = makeObject(
    Some(getName(annotations, info)),
    getDescription(annotations),
    fields.filterNot { case (label, _, _, _) =>
      paramAnnotations.getOrElse(label, Nil).contains(GQLExcluded())
    }.map { case (label, _, schema, _) =>
      val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
      val deprecatedReason = getDeprecatedReason(fieldAnnotations)

      __Field(
        getName(fieldAnnotations, label),
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

  inline given gen[R, A]: Schema[R, A] = derived[R, A]

  inline def genDebug[R, A]: Schema[R, A] = PrintDerived(derived[R, A])
}

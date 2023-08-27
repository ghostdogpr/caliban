package caliban.schema

import caliban.Value.EnumValue
import caliban.introspection.adt.*
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.*
import caliban.schema.Step.{ FunctionStep, ObjectStep }
import caliban.schema.Types.*
import caliban.schema.macros.{ Macros, TypeInfo }

import scala.compiletime.*
import scala.deriving.Mirror
import scala.util.NotGiven

object PrintDerived {
  import scala.quoted.*
  inline def apply[T](inline any: T): T                                = ${ printDerived('any) }
  def printDerived[T: Type](any: Expr[T])(using qctx: Quotes): Expr[T] = {
    import qctx.reflect.*
    println(Printer.TreeShortCode.show(any.asTerm))
    any
  }
}

trait CommonSchemaDerivation {

  /**
   * Default naming logic for input types.
   * This is needed to avoid a name clash between a type used as an input and the same type used as an output.
   * GraphQL needs 2 different types, and they can't have the same name.
   * By default, the "Input" suffix is added after the type name, given that it is not already present.
   */
  def customizeInputTypeName(name: String): String =
    name match {
      case s"${prefix}Input" => name
      case _                 => s"${name}Input"
    }

  inline def recurse[R, P, Label, A <: Tuple](
    inline values: List[(String, List[Any], Schema[R, Any], Int)] = Nil
  )(inline index: Int = 0): List[(String, List[Any], Schema[R, Any], Int)] =
    inline erasedValue[(Label, A)] match {
      case (_: EmptyTuple, _)                 => values.reverse
      case (_: (name *: names), _: (t *: ts)) =>
        recurse[R, P, names, ts] {
          inline if (Macros.isFieldExcluded[P, name]) values
          else
            (
              constValue[name].toString,
              Macros.annotations[t], {
                if (Macros.isEnumField[P, t])
                  if (!Macros.implicitExists[Schema[R, t]]) derived[R, t]
                  else summonInline[Schema[R, t]]
                else summonInline[Schema[R, t]]
              }.asInstanceOf[Schema[R, Any]],
              index
            ) :: values
        }(index + 1)
    }

  inline def derived[R, A]: Schema[R, A] =
    inline summonInline[Mirror.Of[A]] match {
      case m: Mirror.SumOf[A] =>
        makeSumSchema[R, A](
          recurse[R, A, m.MirroredElemLabels, m.MirroredElemTypes]()(),
          Macros.typeInfo[A],
          Macros.annotations[A]
        )(m.ordinal)

      case m: Mirror.ProductOf[A] =>
        makeProductSchema[R, A](
          recurse[R, A, m.MirroredElemLabels, m.MirroredElemTypes]()(),
          Macros.typeInfo[A],
          Macros.annotations[A],
          Macros.paramAnnotations[A].toMap
        )
    }

  private def makeSumSchema[R, A](
    _members: => List[(String, List[Any], Schema[R, Any], Int)],
    info: TypeInfo,
    annotations: List[Any]
  )(ordinal: A => Int): Schema[R, A] = new Schema[R, A] {

    // Vector has ~O(1) performance for `.apply` as opposed to List's O(n)
    private lazy val members = _members.map(v => (v._1, v._2, v._3)).toVector

    private lazy val membersOrdered = members.sortBy(_._1).toList

    private lazy val subTypes = membersOrdered.map { (label, subTypeAnnotations, schema) =>
      (label, schema.toType_(), subTypeAnnotations)
    }

    private lazy val isEnum = subTypes.forall { (_, t, _) =>
      t.fields(__DeprecatedArgs(Some(true))).forall(_.isEmpty)
      && t.inputFields.forall(_.isEmpty)
    }

    private lazy val isInterface = annotations.exists {
      case GQLInterface() => true
      case _              => false
    }

    private lazy val isUnion = annotations.exists {
      case GQLUnion() => true
      case _          => false
    }

    private lazy val isOneOfInput = annotations.contains(GQLOneOfInput())

    def toType(isInput: Boolean, isSubscription: Boolean): __Type =
      if (!isInterface && !isUnion && membersOrdered.nonEmpty && isEnum && !isOneOfInput)
        mkEnum(annotations, info, subTypes)
      else if (isOneOfInput && isInput) {
        mkOneOfInput(annotations, membersOrdered, info)
      } else if (!isInterface)
        makeUnion(
          Some(getName(annotations, info)),
          getDescription(annotations),
          subTypes.map((_, t, _) => fixEmptyUnionObject(t)),
          Some(info.full),
          Some(getDirectives(annotations))
        )
      else {
        val impl = subTypes.map(_._2.copy(interfaces = () => Some(List(toType(isInput, isSubscription)))))
        mkInterface(annotations, info, impl)
      }

    def resolve(value: A): Step[R] = {
      val (label, _, schema) = members(ordinal(value))
      if (isEnum) PureStep(EnumValue(label)) else schema.resolve(value)
    }
  }

  private def makeProductSchema[R, A](
    _fields: => List[(String, List[Any], Schema[R, Any], Int)],
    info: TypeInfo,
    annotations: List[Any],
    paramAnnotations: Map[String, List[Any]]
  ): Schema[R, A] = new Schema[R, A] {

    private lazy val fields = _fields

    private lazy val isValueType: Boolean =
      annotations.exists {
        case GQLValueType(_) => true
        case _               => false
      }

    private lazy val isScalarValueType: Boolean =
      annotations.exists {
        case GQLValueType(true) => true
        case _                  => false
      }

    private lazy val name = getName(annotations, info)

    def toType(isInput: Boolean, isSubscription: Boolean): __Type =
      if (isValueType && fields.nonEmpty)
        if (isScalarValueType) makeScalar(name, getDescription(annotations))
        else fields.head._3.toType_(isInput, isSubscription)
      else if (isInput) mkInputObject[R](annotations, fields, info, paramAnnotations)(isInput, isSubscription)
      else mkObject[R](annotations, fields, info, paramAnnotations)(isInput, isSubscription)

    override private[schema] lazy val resolveFieldLazily: Boolean = fields.nonEmpty

    def resolve(value: A): Step[R] =
      if (fields.isEmpty) PureStep(EnumValue(name))
      else if (isValueType) {
        val head = fields.head
        head._3.resolve(value.asInstanceOf[Product].productElement(head._4))
      } else {
        val fieldsBuilder = Map.newBuilder[String, Step[R]]
        fields.foreach { (label, _, schema, index) =>
          val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
          lazy val step        = schema.resolve(value.asInstanceOf[Product].productElement(index))
          fieldsBuilder += getName(fieldAnnotations, label) -> {
            if (schema.resolveFieldLazily) FunctionStep(_ => step)
            else step
          }
        }
        ObjectStep(name, fieldsBuilder.result())
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

  private def mkEnum(annotations: List[Any], info: TypeInfo, subTypes: List[(String, __Type, List[Any])]) =
    makeEnum(
      Some(getName(annotations, info)),
      getDescription(annotations),
      subTypes.collect { case (name, __Type(_, _, description, _, _, _, _, _, _, _, _, _, _), annotations) =>
        __EnumValue(
          name,
          description,
          getDeprecatedReason(annotations).isDefined,
          getDeprecatedReason(annotations),
          Some(annotations.collect { case GQLDirective(dir) => dir }).filter(_.nonEmpty)
        )
      },
      Some(info.full),
      Some(getDirectives(annotations))
    )

  private def mkInterface(
    annotations: List[Any],
    info: TypeInfo,
    impl: List[__Type]
  ) = {
    val commonFields = () =>
      impl
        .flatMap(_.allFields)
        .groupBy(_.name)
        .collect {
          case (_, list) if list.lengthCompare(impl.size) == 0 =>
            Types
              .unify(list)
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

  private def mkOneOfInput[R](
    annotations: List[Any],
    members: List[(String, List[Any], Schema[R, Any])],
    info: TypeInfo
  ) =
    makeInputObject(
      Some(getInputName(annotations).getOrElse(customizeInputTypeName(getName(annotations, info)))),
      getDescription(annotations),
      members.map { (label, annotations, schema) =>
        __InputValue(
          oneOfInputFieldName(label, annotations),
          getDescription(annotations),
          () => schema.toType_(isInput = true),
          None,
          None
        )
      },
      Some(info.full),
      Some(List(Directive("oneOf"))),
      isOneOf = true
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
}

trait SchemaDerivation[R] extends CommonSchemaDerivation {
  inline def gen[R, A]: Schema[R, A] = derived[R, A]

  inline def genDebug[R, A]: Schema[R, A] = PrintDerived(derived[R, A])

  final lazy val auto = new AutoSchemaDerivation[Any] {}

  sealed trait SemiAuto[A] extends Schema[R, A]
  object SemiAuto {
    inline def derived[A]: SemiAuto[A]                       = exported(Schema.derived[R, A])
    private def exported[A](impl: Schema[R, A]): SemiAuto[A] = new {
      export impl.*
    }
  }

  sealed trait Auto[A] extends Schema[R, A] {
    inline given genAuto[T](using NotGiven[Schema[R, T]]): Schema[R, T] = derived[R, T]
  }

  object Auto {
    inline def derived[A]: Auto[A] = new {
      private val impl = Schema.derived[R, A]
      export impl.*
    }
  }
}

trait AutoSchemaDerivation[R] extends GenericSchema[R] with LowPriorityDerivedSchema {
  // for cross-compililing with scala 2
  inline def genAll[R, A]: Schema[R, A] = derived[R, A]
}

private[schema] trait LowPriorityDerivedSchema extends CommonSchemaDerivation {
  inline implicit def genAuto[R, A]: Schema[R, A] = derived[R, A]
}

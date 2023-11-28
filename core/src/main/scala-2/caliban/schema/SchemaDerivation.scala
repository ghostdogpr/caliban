package caliban.schema

import caliban.Value._
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import caliban.schema.Annotations._
import caliban.schema.Step.{ PureStep => _ }
import caliban.schema.Types._
import magnolia1._

import scala.language.experimental.macros

trait CommonSchemaDerivation[R] {

  /**
   * Default naming logic for input types.
   * This is needed to avoid a name clash between a type used as an input and the same type used as an output.
   * GraphQL needs 2 different types, and they can't have the same name.
   * By default, the "Input" suffix is added after the type name, given that it is not already present.
   */
  def customizeInputTypeName(name: String): String =
    if (name.endsWith("Input")) name else s"${name}Input"

  type Typeclass[T] = Schema[R, T]

  def isValueType[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Boolean =
    ctx.annotations.exists {
      case GQLValueType(_) => true
      case _               => false
    }

  def isScalarValueType[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Boolean =
    ctx.annotations.exists {
      case GQLValueType(true) => true
      case _                  => false
    }

  def join[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    private lazy val objectResolver =
      new ObjectFieldResolver[R, T](
        getName(ctx),
        ctx.parameters.map { p =>
          getName(p) -> { (v: T) => p.typeclass.resolve(p.dereference(v)) }
        }
      )

    private lazy val _isValueType = (ctx.isValueClass || isValueType(ctx)) && ctx.parameters.nonEmpty

    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
      val _ = objectResolver // Initializes lazy val
      if (_isValueType) {
        if (isScalarValueType(ctx)) makeScalar(getName(ctx), getDescription(ctx))
        else ctx.parameters.head.typeclass.toType_(isInput, isSubscription)
      } else if (isInput)
        makeInputObject(
          Some(ctx.annotations.collectFirst { case GQLInputName(suffix) => suffix }
            .getOrElse(customizeInputTypeName(getName(ctx)))),
          getDescription(ctx),
          ctx.parameters
            .map(p =>
              __InputValue(
                getName(p),
                getDescription(p),
                () =>
                  if (p.typeclass.optional) p.typeclass.toType_(isInput, isSubscription)
                  else p.typeclass.toType_(isInput, isSubscription).nonNull,
                p.annotations.collectFirst { case GQLDefault(v) => v },
                p.annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
                p.annotations.collectFirst { case GQLDeprecated(reason) => reason },
                Some(p.annotations.collect { case GQLDirective(dir) => dir }.toList).filter(_.nonEmpty)
              )
            )
            .toList,
          Some(ctx.typeName.full),
          Some(getDirectives(ctx))
        )
      else
        makeObject(
          Some(getName(ctx)),
          getDescription(ctx),
          ctx.parameters
            .filterNot(_.annotations.exists(_ == GQLExcluded()))
            .map(p =>
              Types.makeField(
                getName(p),
                getDescription(p),
                p.typeclass.arguments,
                () =>
                  if (p.typeclass.optional) p.typeclass.toType_(isInput, isSubscription)
                  else p.typeclass.toType_(isInput, isSubscription).nonNull,
                p.annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
                p.annotations.collectFirst { case GQLDeprecated(reason) => reason },
                Option(p.annotations.collect { case GQLDirective(dir) => dir }.toList).filter(_.nonEmpty)
              )
            )
            .toList,
          getDirectives(ctx),
          Some(ctx.typeName.full)
        )
    }

    override def resolve(value: T): Step[R] =
      if (ctx.isObject) PureStep(EnumValue(getName(ctx)))
      else if (_isValueType) resolveValueType(value)
      else objectResolver.resolve(value)

    private def resolveValueType(value: T): Step[R] = {
      val head = ctx.parameters.head
      head.typeclass.resolve(head.dereference(value))
    }

  }

  def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
      val subtypes    =
        ctx.subtypes
          .map(s => s.typeclass.toType_() -> s.annotations)
          .toList
          .sortBy { case (tpe, _) =>
            tpe.name.getOrElse("")
          }
      val isEnum      = subtypes.forall {
        case (t, _) if t.allFields.isEmpty && t.allInputFields.isEmpty => true
        case _                                                         => false
      }
      val isInterface = ctx.annotations.exists {
        case GQLInterface() => true
        case _              => false
      }
      val isUnion     = ctx.annotations.exists {
        case GQLUnion() => true
        case _          => false
      }

      if (isEnum && subtypes.nonEmpty && !isInterface && !isUnion)
        makeEnum(
          Some(getName(ctx)),
          getDescription(ctx),
          subtypes.collect { case (__Type(_, Some(name), description, _, _, _, _, _, _, _, _, _), annotations) =>
            __EnumValue(
              name,
              description,
              annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
              annotations.collectFirst { case GQLDeprecated(reason) => reason },
              Some(annotations.collect { case GQLDirective(dir) => dir }.toList).filter(_.nonEmpty)
            )
          },
          Some(ctx.typeName.full),
          Some(getDirectives(ctx.annotations))
        )
      else if (!isInterface)
        makeUnion(
          Some(getName(ctx)),
          getDescription(ctx),
          subtypes.map { case (t, _) => fixEmptyUnionObject(t) },
          Some(ctx.typeName.full),
          Some(getDirectives(ctx.annotations))
        )
      else {
        val impl         = subtypes.map(_._1.copy(interfaces = () => Some(List(toType(isInput, isSubscription)))))
        val commonFields = () =>
          impl
            .flatMap(_.allFields)
            .groupBy(_.name)
            .filter { case (_, list) => list.lengthCompare(impl.size) == 0 }
            .collect { case (_, list) =>
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
          Some(getName(ctx)),
          getDescription(ctx),
          commonFields,
          impl,
          Some(ctx.typeName.full),
          Some(getDirectives(ctx.annotations))
        )
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
                    _ => Nil,
                    () => makeScalar("Boolean")
                  )
                )
              )
          )
        case _         => t
      }

    override def resolve(value: T): Step[R] =
      ctx.split(value)(subType => subType.typeclass.resolve(subType.cast(value)))
  }

  private def getDirectives(annotations: Seq[Any]): List[Directive] =
    annotations.collect { case GQLDirective(dir) => dir }.toList

  private def getDirectives[Typeclass[_], Type](ctx: ReadOnlyCaseClass[Typeclass, Type]): List[Directive] =
    getDirectives(ctx.annotations)

  private def getName(annotations: Seq[Any], typeName: TypeName): String =
    annotations.collectFirst { case GQLName(name) => name }.getOrElse {
      typeName.typeArguments match {
        case Nil  => typeName.short
        case args => typeName.short + args.map(getName(Nil, _)).mkString
      }
    }

  private def getName[Typeclass[_], Type](ctx: ReadOnlyCaseClass[Typeclass, Type]): String =
    getName(ctx.annotations, ctx.typeName)

  private def getName[Typeclass[_], Type](ctx: SealedTrait[Typeclass, Type]): String =
    getName(ctx.annotations, ctx.typeName)

  private def getName[Typeclass[_], Type](ctx: ReadOnlyParam[Typeclass, Type]): String =
    ctx.annotations.collectFirst { case GQLName(name) => name }.getOrElse(ctx.label)

  private def getDescription(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLDescription(desc) => desc }

  private def getDescription[Typeclass[_], Type](ctx: ReadOnlyCaseClass[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  private def getDescription[Typeclass[_], Type](ctx: SealedTrait[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  private def getDescription[Typeclass[_], Type](ctx: ReadOnlyParam[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)
}

trait SchemaDerivation[R] extends CommonSchemaDerivation[R] {
  def apply[A](implicit ev: Schema[R, A]): Schema[R, A] = ev

  /**
   * Returns an instance of `Schema` for the given type T.
   * This method requires a `Schema` for all types nested inside T.
   * It should be used only if T is a case class or a sealed trait.
   */
  def gen[R0, T]: Typeclass[T] = macro Magnolia.gen[T]

  object auto extends AutoSchemaDerivation[R]
}

trait AutoSchemaDerivation[R] extends GenericSchema[R] with LowPriorityDerivedSchema {
  implicit def genMacro[T]: Derived[Typeclass[T]] = macro DerivedMagnolia.derivedMagnolia[Typeclass, T]

  /**
   * Returns an instance of `Schema` for the given type T.
   * This method will automatically generate missing `Schema` for all types nested inside T that are case classes or sealed traits.
   */
  def genAll[R0, T](implicit derived: Derived[Schema[R0, T]]): Schema[R0, T] = derived.schema
}

private[schema] trait LowPriorityDerivedSchema {
  implicit def derivedSchema[R, T](implicit derived: Derived[Schema[R, T]]): Schema[R, T] = derived.schema
}

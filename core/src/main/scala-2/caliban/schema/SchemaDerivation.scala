package caliban.schema

import caliban.Value._
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import caliban.schema.Annotations._
import caliban.schema.Step._
import caliban.schema.Types._
import magnolia._

import scala.language.experimental.macros

trait SchemaDerivation[R] extends LowPriorityDerivedSchema {

  /**
   * Default naming logic for input types.
   * This is needed to avoid a name clash between a type used as an input and the same type used as an output.
   * GraphQL needs 2 different types, and they can't have the same name.
   * By default, we add the "Input" suffix after the type name.
   */
  def customizeInputTypeName(name: String): String = s"${name}Input"

  type Typeclass[T] = Schema[R, T]

  def combine[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
      if (ctx.isValueClass && ctx.parameters.nonEmpty) ctx.parameters.head.typeclass.toType_(isInput, isSubscription)
      else if (isInput)
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
                  else makeNonNull(p.typeclass.toType_(isInput, isSubscription)),
                None,
                Some(p.annotations.collect { case GQLDirective(dir) => dir }.toList).filter(_.nonEmpty)
              )
            )
            .toList,
          Some(ctx.typeName.full)
        )
      else
        makeObject(
          Some(getName(ctx)),
          getDescription(ctx),
          ctx.parameters
            .map(p =>
              __Field(
                getName(p),
                getDescription(p),
                p.typeclass.arguments,
                () =>
                  if (p.typeclass.optional) p.typeclass.toType_(isInput, isSubscription)
                  else makeNonNull(p.typeclass.toType_(isInput, isSubscription)),
                p.annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
                p.annotations.collectFirst { case GQLDeprecated(reason) => reason },
                Option(p.annotations.collect { case GQLDirective(dir) => dir }.toList).filter(_.nonEmpty)
              )
            )
            .toList,
          getDirectives(ctx),
          Some(ctx.typeName.full)
        )

    override def resolve(value: T): Step[R] =
      if (ctx.isObject) PureStep(EnumValue(getName(ctx)))
      else if (ctx.isValueClass && ctx.parameters.nonEmpty) {
        val head = ctx.parameters.head
        head.typeclass.resolve(head.dereference(value))
      } else {
        val fields = Map.newBuilder[String, Step[R]]
        ctx.parameters.foreach(p => fields += getName(p) -> p.typeclass.resolve(p.dereference(value)))
        ObjectStep(getName(ctx), fields.result())
      }
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
      val subtypes =
        ctx.subtypes
          .map(s => s.typeclass.toType_() -> s.annotations)
          .toList
          .sortBy { case (tpe, _) =>
            tpe.name.getOrElse("")
          }
      val isEnum   = subtypes.forall {
        case (t, _)
            if t.fields(__DeprecatedArgs(Some(true))).forall(_.isEmpty)
              && t.inputFields.forall(_.isEmpty) =>
          true
        case _ => false
      }
      if (isEnum && subtypes.nonEmpty)
        makeEnum(
          Some(getName(ctx)),
          getDescription(ctx),
          subtypes.collect { case (__Type(_, Some(name), description, _, _, _, _, _, _, _, _), annotations) =>
            __EnumValue(
              name,
              description,
              annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
              annotations.collectFirst { case GQLDeprecated(reason) => reason }
            )
          },
          Some(ctx.typeName.full)
        )
      else {
        ctx.annotations.collectFirst { case GQLInterface() =>
          ()
        }.fold(
          makeUnion(
            Some(getName(ctx)),
            getDescription(ctx),
            subtypes.map { case (t, _) => fixEmptyUnionObject(t) },
            Some(ctx.typeName.full)
          )
        ) { _ =>
          val impl         = subtypes.map(_._1.copy(interfaces = () => Some(List(toType(isInput, isSubscription)))))
          val commonFields = () =>
            impl
              .flatMap(_.fields(__DeprecatedArgs(Some(true))))
              .flatten
              .groupBy(_.name)
              .filter({ case (_, list) => list.lengthCompare(impl.size) == 0 })
              .collect { case (_, list) =>
                Types
                  .unify(list.map(_.`type`()))
                  .flatMap(t => list.headOption.map(_.copy(`type` = () => t)))
              }
              .flatten
              .toList

          makeInterface(Some(getName(ctx)), getDescription(ctx), commonFields, impl, Some(ctx.typeName.full))
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

    override def resolve(value: T): Step[R] =
      ctx.dispatch(value)(subType => subType.typeclass.resolve(subType.cast(value)))
  }

  private def getDirectives(annotations: Seq[Any]): List[Directive]                                       =
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

  private def getName[Typeclass[_], Type](ctx: ReadOnlyParam[Typeclass, Type]): String                    =
    ctx.annotations.collectFirst { case GQLName(name) => name }.getOrElse(ctx.label)

  private def getDescription(annotations: Seq[Any]): Option[String]                                       =
    annotations.collectFirst { case GQLDescription(desc) => desc }

  private def getDescription[Typeclass[_], Type](ctx: ReadOnlyCaseClass[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  private def getDescription[Typeclass[_], Type](ctx: SealedTrait[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  private def getDescription[Typeclass[_], Type](ctx: ReadOnlyParam[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  /**
   * Generates an instance of `Schema` for the given type T.
   * This should be used only if T is a case class or a sealed trait.
   */
  implicit def genMacro[T]: Derived[Typeclass[T]] = macro DerivedMagnolia.derivedMagnolia[Typeclass, T]

  /**
   * Returns an instance of `Schema` for the given type T.
   * For a case class or sealed trait, you can call `genMacro[T].schema` instead to get more details if the
   * schema can't be derived.
   */
  def gen[T](implicit derived: Derived[Schema[R, T]]): Schema[R, T] = derived.schema

}

private[schema] trait LowPriorityDerivedSchema {
  implicit def derivedSchema[R, T](implicit derived: Derived[Schema[R, T]]): Schema[R, T] = derived.schema
}

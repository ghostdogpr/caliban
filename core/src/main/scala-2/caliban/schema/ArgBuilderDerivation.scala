package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.Value._
import caliban.schema.Annotations.{ GQLDefault, GQLName, GQLOneOfInput, GQLOneOfInputName, GQLValueType }
import magnolia._
import mercator.Monadic

import scala.language.experimental.macros

trait CommonArgBuilderDerivation {

  type Typeclass[T] = ArgBuilder[T]

  type EitherExecutionError[A] = Either[ExecutionError, A]

  implicit val eitherMonadic: Monadic[EitherExecutionError] = new Monadic[EitherExecutionError] {
    override def flatMap[A, B](from: EitherExecutionError[A])(
      fn: A => EitherExecutionError[B]
    ): EitherExecutionError[B] = from.flatMap(fn)

    override def point[A](value: A): EitherExecutionError[A] = Right(value)

    override def map[A, B](from: EitherExecutionError[A])(fn: A => B): EitherExecutionError[B] = from.map(fn)
  }

  def combine[T](ctx: CaseClass[ArgBuilder, T]): ArgBuilder[T] =
    (input: InputValue) =>
      ctx.constructMonadic { p =>
        input match {
          case InputValue.ObjectValue(fields) =>
            val label   = p.annotations.collectFirst { case GQLName(name) => name }.getOrElse(p.label)
            val default = p.annotations.collectFirst { case GQLDefault(v) => v }
            fields.get(label).fold(p.typeclass.buildMissing(default))(p.typeclass.build)
          case value                          => p.typeclass.build(value)
        }
      }

  def dispatch[T](ctx: SealedTrait[ArgBuilder, T]): ArgBuilder[T] =
    if (ctx.annotations.contains(GQLOneOfInput())) makeOneOfBuilder(ctx)
    else makeSumBuilder(ctx)

  private def makeSumBuilder[T](ctx: SealedTrait[ArgBuilder, T]): ArgBuilder[T] = input =>
    (input match {
      case EnumValue(value)   => Some(value)
      case StringValue(value) => Some(value)
      case _                  => None
    }) match {
      case Some(value) =>
        ctx.subtypes
          .find(t =>
            t.annotations.collectFirst { case GQLName(name) => name }.contains(value) || t.typeName.short == value
          ) match {
          case Some(subtype) => subtype.typeclass.build(InputValue.ObjectValue(Map()))
          case None          => Left(ExecutionError(s"Invalid value $value for trait ${ctx.typeName.short}"))
        }
      case None        => Left(ExecutionError(s"Can't build a trait from input $input"))
    }

  private def makeOneOfBuilder[A](ctx: SealedTrait[ArgBuilder, A]): ArgBuilder[A] =
    new ArgBuilder[A] {

      private val builders =
        ctx.subtypes.map { p =>
          val name        = Types.oneOfInputFieldName(p.typeName.short, p.annotations)
          val isValueType = p.annotations.exists { case GQLValueType(_) => true; case _ => false }
          name -> (p.typeclass, isValueType)
        }.toMap

      private def error(input: InputValue) =
        ExecutionError(s"Invalid oneOf input $input for trait ${ctx.typeName.short}")

      def build(input: InputValue): Either[ExecutionError, A] = input match {
        case InputValue.ObjectValue(fields) =>
          fields.toList match {
            case (key, innerValue) :: Nil =>
              builders.get(key).toRight(error(input)).flatMap { case (builder, isValueType) =>
                innerValue match {
                  case v: InputValue.ObjectValue if !isValueType => builder.build(v)
                  case v if isValueType                          => builder.build(v)
                  case _                                         => Left(error(input))
                }
              }
            case _                        =>
              Left(ExecutionError("Exactly one key must be specified for oneOf inputs"))
          }
        case _                              => Left(error(input))
      }
    }

}

trait ArgBuilderDerivation extends CommonArgBuilderDerivation {
  def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}

trait AutoArgBuilderDerivation extends ArgBuilderInstances with LowPriorityDerivedArgBuilder

private[schema] trait LowPriorityDerivedArgBuilder extends CommonArgBuilderDerivation {
  implicit def genAuto[T]: Typeclass[T] = macro Magnolia.gen[T]
}

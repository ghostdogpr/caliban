package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.Value._
import caliban.schema.Annotations.{ GQLDefault, GQLName, GQLOneOfInput }
import magnolia1._

import scala.collection.compat.immutable.ArraySeq
import scala.language.experimental.macros

trait CommonArgBuilderDerivation {
  import caliban.syntax._

  type Typeclass[T] = ArgBuilder[T]

  type EitherExecutionError[A] = Either[ExecutionError, A]

  def join[T](ctx: CaseClass[ArgBuilder, T]): ArgBuilder[T] = new ArgBuilder[T] {
    private lazy val params = {
      val arr = Array.ofDim[(String, EitherExecutionError[Any], ArgBuilder[Any])](ctx.parameters.length)
      ctx.parameters.zipWithIndex.foreach { case (p, i) =>
        val label   = p.annotations.collectFirst { case GQLName(name) => name }.getOrElse(p.label)
        val default = p.typeclass.buildMissing(p.annotations.collectFirst { case GQLDefault(v) => v })
        arr(i) = (label, default, p.typeclass.asInstanceOf[ArgBuilder[Any]])
      }
      arr
    }

    private lazy val required = params.collect { case (label, default, _) if default.isLeft => label }

    private val isValueType = DerivationUtils.isValueType(ctx)

    override private[schema] val partial: PartialFunction[InputValue, Either[ExecutionError, T]] = {
      case InputValue.ObjectValue(fields) if required.forall(fields.contains) => fromFields(fields)
    }

    def build(input: InputValue): Either[ExecutionError, T] =
      input match {
        case InputValue.ObjectValue(fields) if !isValueType => fromFields(fields)
        case value if isValueType                           => fromValue(value)
        case _                                              => Left(ExecutionError("Expected an input object"))
      }

    private def fromFields(fields: Map[String, InputValue]): Either[ExecutionError, T] = {
      val params = this.params
      var i      = 0
      val l      = params.length
      val arr    = Array.ofDim[Any](l)
      while (i < l) {
        val (label, default, builder) = params(i)
        val field                     = fields.getOrElseNull(label)
        val value                     = if (field ne null) builder.build(field) else default
        value match {
          case Right(v) => arr(i) = v
          case l        => return l.asInstanceOf[Either[ExecutionError, T]]
        }
        i += 1
      }
      val args   = ArraySeq.unsafeWrapArray(arr)
      Right(ctx.rawConstruct(args))
    }

    private def fromValue(input: InputValue): Either[ExecutionError, T] =
      params(0)._3
        .build(input)
        .map(v => ctx.rawConstruct(List(v)))
  }

  def split[T](ctx: SealedTrait[ArgBuilder, T]): ArgBuilder[T] =
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

  private def makeOneOfBuilder[A](ctx: SealedTrait[ArgBuilder, A]): ArgBuilder[A] = new ArgBuilder[A] {

    private def inputError(input: InputValue) =
      ExecutionError(s"Invalid oneOf input $input for trait ${ctx.typeName.short}")

    override val partial: PartialFunction[InputValue, Either[ExecutionError, A]] = {
      val xs = ctx.subtypes.map(_.typeclass).toList.asInstanceOf[List[ArgBuilder[A]]]

      val checkSize: PartialFunction[InputValue, Either[ExecutionError, A]] = {
        case InputValue.ObjectValue(f) if f.size != 1 =>
          Left(ExecutionError("Exactly one key must be specified for oneOf inputs"))
      }
      xs.foldLeft(checkSize)(_ orElse _.partial)
    }

    def build(input: InputValue): Either[ExecutionError, A] =
      partial.applyOrElse(input, (in: InputValue) => Left(inputError(in)))
  }

}

trait ArgBuilderDerivation extends CommonArgBuilderDerivation {
  def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}

trait AutoArgBuilderDerivation extends ArgBuilderInstances with LowPriorityDerivedArgBuilder

private[schema] trait LowPriorityDerivedArgBuilder extends CommonArgBuilderDerivation {
  implicit def genAuto[T]: Typeclass[T] = macro Magnolia.gen[T]
}

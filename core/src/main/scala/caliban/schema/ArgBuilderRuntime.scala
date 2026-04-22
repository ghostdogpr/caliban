package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.Value.{ EnumValue, StringValue }
import caliban.schema.Annotations.{ GQLDefault, GQLName }
import caliban.syntax._

private[schema] final class SingletonArgBuilder[A](
  value: => A,
  label: String,
  annotations: List[Any]
) extends ArgBuilder[A] {
  def build(input: InputValue): Either[ExecutionError, A] =
    input match {
      case InputValue.ObjectValue(fields) if fields.isEmpty => Right(value)
      case EnumValue(value0) if value0 == label || annotations.contains(GQLName(value0)) =>
        Right(value)
      case StringValue(value0) if value0 == label || annotations.contains(GQLName(value0)) =>
        Right(value)
      case _                                                                              =>
        Left(ExecutionError("Expected an input object"))
    }
}

private[schema] final class SumArgBuilder[A](
  _subTypes: => List[(String, List[Any], ArgBuilder[Any])],
  traitLabel: String
) extends ArgBuilder[A] {
  private lazy val subTypes = _subTypes

  def build(input: InputValue): Either[ExecutionError, A] =
    (input match {
      case EnumValue(value)   => Right(value)
      case StringValue(value) => Right(value)
      case _                  => Left(ExecutionError(s"Can't build a trait from input $input"))
    })
      .flatMap { value =>
        subTypes.collectFirst {
          case (label, annotations, builder) if label == value || annotations.contains(GQLName(value)) =>
            builder.asInstanceOf[ArgBuilder[A]]
        }
          .toRight(ExecutionError(s"Invalid value $value for trait $traitLabel"))
          .flatMap(_.build(SumArgBuilder.emptyInput))
      }
}

private[schema] object SumArgBuilder {
  private val emptyInput = InputValue.ObjectValue(Map.empty)
}

private[schema] final class OneOfArgBuilder[A](
  _subTypes: => List[(String, List[Any], ArgBuilder[Any])],
  traitLabel: String
) extends ArgBuilder[A] {

  override lazy val partial: PartialFunction[InputValue, Either[ExecutionError, A]] = {
    val xs = _subTypes.map(_._3).asInstanceOf[List[ArgBuilder[A]]]

    val checkSize: PartialFunction[InputValue, Either[ExecutionError, A]] = {
      case InputValue.ObjectValue(f) if f.size != 1 =>
        Left(ExecutionError("Exactly one key must be specified for oneOf inputs"))
    }
    xs.foldLeft(checkSize)(_ orElse _.partial)
  }

  def build(input: InputValue): Either[ExecutionError, A] =
    partial.applyOrElse(input, (in: InputValue) => Left(inputError(in)))

  private def inputError(input: InputValue) =
    ExecutionError(s"Invalid oneOf input $input for trait $traitLabel")
}

private[schema] final class ProductArgBuilder[A](
  _fields: => List[(String, List[Any], ArgBuilder[Any])],
  isValueType: Boolean,
  construct: Array[Any] => A
) extends ArgBuilder[A] {

  private lazy val params = Array.from(_fields.map { case (label, annotations, builder) =>
    val default    = builder.buildMissing(annotations.collectFirst { case GQLDefault(v) => v })
    val finalLabel = annotations.collectFirst { case GQLName(name) => name }.getOrElse(label)
    (finalLabel, default, builder)
  })

  private lazy val required = params.collect { case (label, default, _) if default.isLeft => label }

  override private[schema] val partial: PartialFunction[InputValue, Either[ExecutionError, A]] = {
    case InputValue.ObjectValue(fields) if required.forall(fields.contains) => fromFields(fields)
  }

  def build(input: InputValue): Either[ExecutionError, A] =
    if (isValueType) fromValue(input)
    else
      input match {
        case InputValue.ObjectValue(fields) => fromFields(fields)
        case _                              => Left(ExecutionError("Expected an input object"))
      }

  private def fromFields(fields: Map[String, InputValue]): Either[ExecutionError, A] = {
    val currentParams = params
    var i             = 0
    val length        = currentParams.length
    val arr           = Array.ofDim[Any](length)
    while (i < length) {
      val (label, default, builder) = currentParams(i)
      val field                     = fields.getOrElseNull(label)
      val value                     = if (field ne null) builder.build(field) else default
      value match {
        case Right(v) => arr(i) = v
        case l        => return l.asInstanceOf[Either[ExecutionError, A]]
      }
      i += 1
    }
    Right(construct(arr))
  }

  private def fromValue(input: InputValue): Either[ExecutionError, A] =
    params(0)._3
      .build(input)
      .map(v => construct(Array[Any](v)))
}

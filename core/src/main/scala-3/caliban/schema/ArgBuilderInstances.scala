package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.Value.{ EnumValue, StringValue }
import caliban.schema.Annotations.{ GQLDefault, GQLName }
import caliban.syntax.*

private final class SumArgBuilder[A](
  _subTypes: => List[(String, List[Any], ArgBuilder[Any])],
  traitLabel: String
) extends ArgBuilder[A] {
  private lazy val subTypes = _subTypes

  def build(input: InputValue): Either[ExecutionError, A] =
    input.match {
      case EnumValue(value)   => Right(value)
      case StringValue(value) => Right(value)
      case _                  => Left(ExecutionError(s"Can't build a trait from input $input"))
    }.flatMap { value =>
      subTypes.collectFirst {
        case (
              label,
              annotations,
              builder: ArgBuilder[A @unchecked]
            ) if label == value || annotations.contains(GQLName(value)) =>
          builder
      }
        .toRight(ExecutionError(s"Invalid value $value for trait $traitLabel"))
        .flatMap(_.build(SumArgBuilder.emptyInput))
    }
}

private object SumArgBuilder {
  private val emptyInput = InputValue.ObjectValue(Map.empty)
}

private final class OneOfArgBuilder[A](
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

private final class ProductArgBuilder[A](
  _fields: => List[(String, ArgBuilder[Any])],
  annotations: Map[String, List[Any]],
  isValueType: Boolean
)(fromProduct: Product => A)
    extends ArgBuilder[A] {
  import ProductArgBuilder.ArrayProductWrapper

  private lazy val params = Array.from(_fields.map { (label, builder) =>
    val labelList  = annotations.get(label)
    val default    = builder.buildMissing(labelList.flatMap(_.collectFirst { case GQLDefault(v) => v }))
    val finalLabel = labelList.flatMap(_.collectFirst { case GQLName(name) => name }).getOrElse(label)
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
        case l        => return l.asInstanceOf[Either[ExecutionError, A]]
      }
      i += 1
    }
    Right(fromProduct(new ArrayProductWrapper(arr)))
  }

  private def fromValue(input: InputValue): Either[ExecutionError, A] =
    params(0)._3
      .build(input)
      .map(v => fromProduct(Tuple1(v)))
}

private object ProductArgBuilder {
  private final class ArrayProductWrapper(arr: Array[Any]) extends Product {
    def canEqual(that: Any): Boolean = false // Not used by the `fromProduct` method so we just stub it
    def productArity: Int            = arr.length
    def productElement(n: Int): Any  = arr(n)
  }
}

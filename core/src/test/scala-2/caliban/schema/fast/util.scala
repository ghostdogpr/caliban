package caliban.schema.fast

import caliban.{ CalibanError, InputValue, Value }
import caliban.schema.ArgBuilder

object util {
  def invokeArgBuilders[T](
    input: InputValue
  )(implicit
    argBuilder: ArgBuilder[T],
    fastArgBuilder: FastArgBuilderWrapper[
      T
    ]
  ): (Either[CalibanError.ExecutionError, T], Either[CalibanError.ExecutionError, T]) =
    (invokeArgBuilder[T](input), invokeFastArgBuilder[T](input))

  def invokeArgBuilder[T](
    input: InputValue
  )(implicit argBuilder: ArgBuilder[T]): Either[CalibanError.ExecutionError, T] =
    argBuilder.build(input)

  def invokeFastArgBuilder[T](
    input: InputValue
  )(implicit fastArgBuilder: FastArgBuilderWrapper[T]): Either[CalibanError.ExecutionError, T] =
    fastArgBuilder.value.build(input)

  object syntax {
    import scala.language.implicitConversions

    val gqlNull: Value = Value.NullValue

    def gqlObject(kvs: (String, InputValue)*) = InputValue.ObjectValue(kvs.toMap)
    def gqlList(lst: InputValue*)             = InputValue.ListValue(lst.toList)
    def gqlEnum(s: String): Value.EnumValue   = Value.EnumValue(s)

    implicit def boolean2GqlBoolean(x: Boolean): Value.BooleanValue = Value.BooleanValue(x)
    implicit def int2GqlInt(x: Int): Value.IntValue.IntNumber       = Value.IntValue.IntNumber(x)
    implicit def long2GqlLong(x: Long): Value.IntValue.LongNumber   = Value.IntValue.LongNumber(x)
    implicit def string2GqlString(x: String): Value.StringValue     = Value.StringValue(x)
  }

}

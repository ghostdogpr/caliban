package caliban.interop.zio

import caliban.Value._
import caliban._
import caliban.introspection.adt.__Type
import caliban.schema.Types.makeScalar
import caliban.schema.{ ArgBuilder, PureStep, Schema, Step }
import zio.Chunk
import zio.json.ast.Json

object json {
  implicit val jsonSchema: Schema[Any, Json] = new Schema[Any, Json] {
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = makeScalar("Json")

    override def resolve(value: Json): Step[Any] =
      PureStep(toResponseValue(value))
  }

  implicit val jsonArgBuilder: ArgBuilder[Json] = (input: InputValue) => Right(fromInputValue(input))

  private def fromInputValue(input: InputValue): Json =
    input match {
      case InputValue.ListValue(values)   =>
        Json.Arr(Chunk.fromIterable(values.map(fromInputValue)))
      case InputValue.ObjectValue(fields) =>
        Json.Obj(Chunk.fromIterable(fields.map { case (k, v) => k -> fromInputValue(v) }))
      case InputValue.VariableValue(name) => Json.Str(name)
      case EnumValue(value)               => Json.Str(value)
      case BooleanValue(value)            => Json.Bool(value)
      case StringValue(value)             => Json.Str(value)
      case x: IntValue                    => Json.Num(BigDecimal(x.toBigInt))
      case x: FloatValue                  => Json.Num(x.toBigDecimal)
      case Value.NullValue                => Json.Null
    }

  private def toResponseValue(input: Json): ResponseValue =
    input match {
      case Json.Str(value)    => Value.StringValue(value)
      case Json.Obj(fields)   => ResponseValue.ObjectValue(fields.map { case (k, v) => k -> toResponseValue(v) }.toList)
      case Json.Arr(elements) => ResponseValue.ListValue(elements.map(toResponseValue).toList)
      case Json.Bool(value)   => Value.BooleanValue(value)
      case Json.Num(value)    =>
        try IntValue(value.intValueExact)
        catch {
          case _: ArithmeticException => Value.FloatValue(value)
        }
      case Json.Null          => Value.NullValue
    }
}

/**
 * Mix-in trait that provides implicit [[caliban.schema.Schema]] and [[caliban.schema.ArgBuilder]] instances for ZIO JSON support.
 */
trait ZioJson {
  implicit val jsonSchema: Schema[Any, Json]    = json.jsonSchema
  implicit val jsonArgBuilder: ArgBuilder[Json] = json.jsonArgBuilder
}

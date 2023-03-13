package caliban.parsing.parsers

import caliban.InputValue
import caliban.InputValue._
import caliban.Value._
import fastparse._

import scala.annotation.nowarn

@nowarn("msg=NoWhitespace") // False positive warning in Scala 2.x
private[caliban] trait ValueParsers extends NumberParsers {
  def booleanValue(implicit ev: P[Any]): P[BooleanValue] =
    P("true").map(_ => BooleanValue(true)) | P("false").map(_ => BooleanValue(false))

  def nullValue(implicit ev: P[Any]): P[InputValue] = P("null").map(_ => NullValue)
  def enumValue(implicit ev: P[Any]): P[InputValue] = P(name).map(EnumValue.apply)
  def listValue(implicit ev: P[Any]): P[ListValue]  = P("[" ~/ value.rep ~ "]").map(values => ListValue(values.toList))

  def objectField(implicit ev: P[Any]): P[(String, InputValue)] = P(name ~ ":" ~/ value)
  def objectValue(implicit ev: P[Any]): P[ObjectValue]          =
    P("{" ~ objectField.rep ~ "}").map(values => ObjectValue(values.toMap))

  def variableValue(implicit ev: P[Any]): P[VariableValue] = P("$" ~/ name).map(VariableValue.apply)

  def value(implicit ev: P[Any]): P[InputValue] =
    P(
      floatValue | intValue | booleanValue | stringValue | nullValue | enumValue | listValue | objectValue | variableValue
    )
}

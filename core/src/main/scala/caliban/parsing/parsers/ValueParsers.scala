package caliban.parsing.parsers

import caliban.InputValue
import caliban.InputValue._
import caliban.Value._
import fastparse._

import scala.annotation.nowarn

@nowarn("msg=NoWhitespace") // False positive warning in Scala 2.x
private[caliban] trait ValueParsers extends NumberParsers {
  def booleanValue(implicit ev: P[Any]): P[BooleanValue] =
    StringIn("true", "false").!.map(v => BooleanValue(v.toBoolean))

  def nullValue(implicit ev: P[Any]): P[InputValue] = LiteralStr("null").map(_ => NullValue)
  def enumValue(implicit ev: P[Any]): P[InputValue] = name.map(EnumValue.apply)
  def listValue(implicit ev: P[Any]): P[ListValue]  = ("[" ~/ value.rep ~ "]").map(values => ListValue(values.toList))

  def objectField(implicit ev: P[Any]): P[(String, InputValue)] = name ~ ":" ~/ value
  def objectValue(implicit ev: P[Any]): P[ObjectValue]          =
    ("{" ~/ objectField.rep ~ "}").map(values => ObjectValue(values.toMap))

  def variableValue(implicit ev: P[Any]): P[VariableValue] = ("$" ~/ name).map(VariableValue.apply)

  def value(implicit ev: P[Any]): P[InputValue] =
    floatValue | intValue | booleanValue | stringValue | nullValue | enumValue | listValue | objectValue | variableValue

}

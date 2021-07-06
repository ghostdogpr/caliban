package caliban.parsing.parsers

import caliban.InputValue
import caliban.InputValue._
import caliban.Value._
import fastparse._

private[caliban] trait ValueParsers extends NumberParsers {
  def booleanValue[_: P]: P[BooleanValue] =
    P("true").map(_ => BooleanValue(true)) | P("false").map(_ => BooleanValue(false))

  def nullValue[_: P]: P[InputValue] = P("null").map(_ => NullValue)
  def enumValue[_: P]: P[InputValue] = P(name).map(EnumValue)
  def listValue[_: P]: P[ListValue]  = P("[" ~/ value.rep ~ "]").map(values => ListValue(values.toList))

  def objectField[_: P]: P[(String, InputValue)] = P(name ~ ":" ~/ value)
  def objectValue[_: P]: P[ObjectValue]          =
    P("{" ~ objectField.rep ~ "}").map(values => ObjectValue(values.toMap))

  def variableValue[_: P]: P[VariableValue] = P("$" ~/ name).map(VariableValue)

  def value[_: P]: P[InputValue] =
    P(
      floatValue | intValue | booleanValue | stringValue | nullValue | enumValue | listValue | objectValue | variableValue
    )
}

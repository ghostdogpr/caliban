package caliban.parsing.parsers

import caliban.Value._
import fastparse._

private[caliban] trait NumberParsers extends StringParsers {
  def negativeSign(implicit ev: P[Any]): P[Unit] = StringIn("-")
  def nonZeroDigit(implicit ev: P[Any]): P[Unit] = CharIn("1-9")
  def digit(implicit ev: P[Any]): P[Unit]        = CharIn("0-9")
  def integerPart(implicit ev: P[Any]): P[Unit]  =
    negativeSign.? ~~ ("0" | (nonZeroDigit ~~ digit.repX))

  def intValue(implicit ev: P[Any]): P[IntValue] = integerPart.!.map(IntValue.fromStringUnsafe)

  def sign(implicit ev: P[Any]): P[Unit]              = StringIn("-", "+")
  def exponentIndicator(implicit ev: P[Any]): P[Unit] = CharIn("eE")
  def exponentPart(implicit ev: P[Any]): P[Unit]      = exponentIndicator ~~ sign.? ~~ digit.repX(1)
  def fractionalPart(implicit ev: P[Any]): P[Unit]    = "." ~~ digit.repX(1)
  def floatValue(implicit ev: P[Any]): P[FloatValue]  =
    (
      integerPart ~~ (fractionalPart | exponentPart | (fractionalPart ~~ exponentPart))
    ).!.map(FloatValue.fromStringUnsafe)
}

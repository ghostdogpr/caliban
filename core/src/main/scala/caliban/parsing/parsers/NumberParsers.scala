package caliban.parsing.parsers

import caliban.Value._
import fastparse._

private[caliban] trait NumberParsers extends StringParsers {
  def negativeSign(implicit ev: P[Any]): P[Unit] = P("-")
  def nonZeroDigit(implicit ev: P[Any]): P[Unit] = P(CharIn("1-9"))
  def digit(implicit ev: P[Any]): P[Unit]        = P("0" | nonZeroDigit)
  def integerPart(implicit ev: P[Any]): P[Unit]  = P(
    (negativeSign.? ~~ "0") | (negativeSign.? ~~ nonZeroDigit ~~ digit.repX)
  )
  def intValue(implicit ev: P[Any]): P[IntValue] = integerPart.!.map(IntValue(_))

  def sign(implicit ev: P[Any]): P[Unit]              = P("-" | "+")
  def exponentIndicator(implicit ev: P[Any]): P[Unit] = P(CharIn("eE"))
  def exponentPart(implicit ev: P[Any]): P[Unit]      = P(exponentIndicator ~~ sign.? ~~ digit.repX(1))
  def fractionalPart(implicit ev: P[Any]): P[Unit]    = P("." ~~ digit.repX(1))
  def floatValue(implicit ev: P[Any]): P[FloatValue]  =
    P(
      (integerPart ~~ fractionalPart) | (integerPart ~~ exponentPart) | (integerPart ~~ fractionalPart ~~ exponentPart)
    ).!.map(FloatValue(_))
}

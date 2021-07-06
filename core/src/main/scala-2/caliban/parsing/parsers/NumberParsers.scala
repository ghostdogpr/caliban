package caliban.parsing.parsers

import caliban.Value._
import fastparse._

private[caliban] trait NumberParsers extends StringParsers {
  def negativeSign[_: P]: P[Unit] = P("-")
  def nonZeroDigit[_: P]: P[Unit] = P(CharIn("1-9"))
  def digit[_: P]: P[Unit]        = P("0" | nonZeroDigit)
  def integerPart[_: P]: P[Unit]  = P((negativeSign.? ~~ "0") | (negativeSign.? ~~ nonZeroDigit ~~ digit.repX))
  def intValue[_: P]: P[IntValue] = integerPart.!.map(IntValue(_))

  def sign[_: P]: P[Unit]              = P("-" | "+")
  def exponentIndicator[_: P]: P[Unit] = P(CharIn("eE"))
  def exponentPart[_: P]: P[Unit]      = P(exponentIndicator ~~ sign.? ~~ digit.repX(1))
  def fractionalPart[_: P]: P[Unit]    = P("." ~~ digit.repX(1))
  def floatValue[_: P]: P[FloatValue]  =
    P(
      (integerPart ~~ fractionalPart) | (integerPart ~~ exponentPart) | (integerPart ~~ fractionalPart ~~ exponentPart)
    ).!.map(FloatValue(_))
}

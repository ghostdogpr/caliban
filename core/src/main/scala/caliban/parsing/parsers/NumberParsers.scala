package caliban.parsing.parsers

import caliban.Value._
import fastparse._

private[caliban] trait NumberParsers extends StringParsers {
  def negativeSign(implicit ev: P[Any]): P[Unit] = StringIn("-")
  def nonZeroDigit(implicit ev: P[Any]): P[Unit] = CharIn("1-9")
  def digit(implicit ev: P[Any]): P[Unit]        = CharIn("0-9")
  def integerPart(implicit ev: P[Any]): P[Unit]  =
    negativeSign.? ~~ ("0" | (nonZeroDigit ~~ digit.repX))

  def numberLookahead(implicit ev: P[Any]): P[Unit] = {
    val index = ev.index
    val input = ev.input
    if (input.isReachable(index)) {
      val c = input(index)
      if (c == '.' || c == '_' || (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
        ev.freshFailure()
      else ev.freshSuccessUnit()
    } else ev.freshSuccessUnit()
  }

  def intValue(implicit ev: P[Any]): P[IntValue] =
    (integerPart.! ~~ numberLookahead).map(IntValue.fromStringUnsafe)

  def sign(implicit ev: P[Any]): P[Unit]              = StringIn("-", "+")
  def exponentIndicator(implicit ev: P[Any]): P[Unit] = CharIn("eE")
  def exponentPart(implicit ev: P[Any]): P[Unit]      = exponentIndicator ~~ sign.? ~~ digit.repX(1)
  def fractionalPart(implicit ev: P[Any]): P[Unit]    = "." ~~ digit.repX(1)
  def floatValue(implicit ev: P[Any]): P[FloatValue]  =
    (
      (integerPart ~~ ((fractionalPart ~~ exponentPart) | fractionalPart | exponentPart)).! ~~ numberLookahead
    ).map(FloatValue.fromStringUnsafe)
}

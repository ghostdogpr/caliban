package caliban.parsing.parsers

import caliban.Value.StringValue
import fastparse._

import scala.annotation.{ switch, tailrec }

private[caliban] trait StringParsers {
  implicit object whitespace extends Whitespace {

    type State = Int

    // statuses
    final val Normal                  = 0
    final val InsideLineComment       = 1
    final val DetermineLineBreakStart = 2

    final val UnicodeBOM   = '\uFEFF'
    final val Tab          = '\u0009'
    final val Space        = '\u0020'
    final val LF           = '\u000A'
    final val CR           = '\u000D'
    final val Comma        = ','
    final val CommentStart = '#'

    override def apply(p: P[_]): P[Unit] =
      loop(p.index, state = Normal)(p, p.input)

    @tailrec def loop(index: Int, state: State)(implicit ctx: P[_], input: ParserInput): ParsingRun[Unit] =
      if (input.isReachable(index)) {
        val currentChar = input(index)
        (state: @switch) match {
          case Normal                  =>
            (currentChar: @switch) match {
              case CR                                    => loop(index + 1, state = DetermineLineBreakStart)
              case Space | LF | Comma | Tab | UnicodeBOM => loop(index + 1, state = Normal)
              case CommentStart                          => loop(index + 1, state = InsideLineComment)
              case _                                     => ctx.freshSuccessUnit(index)
            }
          case InsideLineComment       =>
            loop(
              index + 1,
              state = (currentChar: @switch) match {
                case CR => DetermineLineBreakStart
                case LF => Normal
                case _  => InsideLineComment
              }
            )
          case DetermineLineBreakStart =>
            (currentChar: @switch) match {
              case LF => loop(index + 1, state = Normal)
              case _  => loop(index, state = Normal)
            }
        }
      } else ctx.freshSuccessUnit(index)
  }

  def sourceCharacter(implicit ev: P[Any]): P[Unit]                      = CharIn("\u0009\u000A\u000D\u0020-\uFFFF")
  def sourceCharacterWithoutLineTerminator(implicit ev: P[Any]): P[Unit] = CharIn("\u0009\u0020-\uFFFF")
  def name(implicit ev: P[Any]): P[String]                               =
    CharsWhileIn("_0-9A-Za-z", 1).!.flatMap { s =>
      // Less efficient in case of an error, but more efficient in case of success (happy path)
      if (s.charAt(0) <= '9') ev.freshFailure()
      else ev.freshSuccess(s)
    }
  def nameOnly(implicit ev: P[Any]): P[String]                           = Start ~ name ~ End

  def hexDigit(implicit ev: P[Any]): P[Unit]         = CharIn("0-9a-fA-F")
  def escapedUnicode(implicit ev: P[Any]): P[String] =
    (hexDigit ~~ hexDigit ~~ hexDigit ~~ hexDigit).!.map(Integer.parseInt(_, 16).toChar.toString)

  def escapedCharacter(implicit ev: P[Any]): P[String] = CharIn("\"\\\\/bfnrt").!.map {
    case "b"   => "\b"
    case "n"   => "\n"
    case "f"   => "\f"
    case "r"   => "\r"
    case "t"   => "\t"
    case other => other
  }

  def stringCharacter(implicit ev: P[Any]): P[String] =
    sourceCharacterWithoutLineTerminator.!.filter(c =>
      c != "\"" && c != "\\"
    ) | "\\u" ~~ escapedUnicode | "\\" ~~ escapedCharacter

  def blockStringCharacter(implicit ev: P[Any]): P[String] = "\\\"\"\"".!.map(_ => "\"\"\"") | sourceCharacter.!

  def stringValue(implicit ev: P[Any]): P[StringValue] =
    (
      ("\"\"\"" ~~ ((!"\"\"\"") ~~ blockStringCharacter).repX.map(s => blockStringValue(s.mkString)) ~~ "\"\"\"") |
        ("\"" ~~ stringCharacter.repX.map(_.mkString) ~~ "\"")
    ).map(v => StringValue(v))

  def blockStringValue(rawValue: String): String = {
    val l1           = rawValue.split("\r?\n").toList
    val commonIndent = l1 match {
      case Nil       => None
      case _ :: tail =>
        tail.foldLeft(Option.empty[Int]) { case (commonIndent, line) =>
          val indent = "[ \t]*".r.findPrefixOf(line).fold(0)(_.length)
          if (indent < line.length && commonIndent.fold(true)(_ > indent)) Some(indent) else commonIndent
        }
    }
    // remove indentation
    val l2           = (commonIndent, l1) match {
      case (Some(value), head :: tail) => head :: tail.map(_.drop(value))
      case _                           => l1
    }
    // NOTE: @noinline annotation required as Scala 2.12 inlines `.dropWhile` which results in compilation error since these methods are used within fastparse's macros
    // remove start lines that are only whitespaces
    val l3           = l2.dropWhile("[ \t]*".r.replaceAllIn(_, "").isEmpty): @noinline
    // remove end lines that are only whitespaces
    val l4           = (l3.reverse.dropWhile("[ \t]*".r.replaceAllIn(_, "").isEmpty): @noinline).reverse
    l4.mkString("\n")
  }
}

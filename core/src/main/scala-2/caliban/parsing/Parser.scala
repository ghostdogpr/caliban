package caliban.parsing

import caliban.CalibanError.ParsingError
import caliban.parsing.adt._
import fastparse._
import zio.{ IO, Task }

object Parser {
  import caliban.parsing.parsers.Parsers._

  /**
   * Parses the given string into a [[caliban.parsing.adt.Document]] object or fails with a [[caliban.CalibanError.ParsingError]].
   */
  def parseQuery(query: String): IO[ParsingError, Document] = {
    val sm = SourceMapper(query)
    Task(parse(query, document(_)))
      .mapError(ex => ParsingError(s"Internal parsing error", innerThrowable = Some(ex)))
      .flatMap {
        case Parsed.Success(value, _) => IO.succeed(Document(value.definitions, sm))
        case f: Parsed.Failure        => IO.fail(ParsingError(f.msg, Some(sm.getLocation(f.index))))
      }
  }

  /**
   * Checks if the query is valid, if not returns an error string.
   */
  def check(query: String): Option[String] = parse(query, document(_)) match {
    case Parsed.Success(_, _) => None
    case f: Parsed.Failure    => Some(f.msg)
  }
}

case class ParsedDocument(definitions: List[Definition], index: Int = 0)

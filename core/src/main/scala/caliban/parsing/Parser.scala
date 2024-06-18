package caliban.parsing

import caliban.CalibanError.ParsingError
import caliban.InputValue
import caliban.parsing.adt._
import fastparse._

import scala.util.Try
import scala.util.control.NonFatal

object Parser {
  import caliban.parsing.parsers.Parsers._

  /**
   * Parses the given string into a [[caliban.parsing.adt.Document]] object or returns a [[caliban.CalibanError.ParsingError]]
   * if the string is not a valid GraphQL document.
   */
  def parseQuery(query: String): Either[ParsingError, Document] = {
    val sm = SourceMapper(query)
    try
      parse(query, document(_)) match {
        case Parsed.Success(value, _) => Right(Document(value.definitions, sm))
        case f: Parsed.Failure        => Left(ParsingError(f.msg, Some(sm.getLocation(f.index))))
      }
    catch {
      case NonFatal(ex) => Left(ParsingError(s"Internal parsing error", innerThrowable = Some(ex)))
    }
  }

  def parseInputValue(rawValue: String): Either[ParsingError, InputValue] = {
    val sm = SourceMapper(rawValue)
    Try(parse(rawValue, value(_))).toEither.left
      .map(ex => ParsingError(s"Internal parsing error", innerThrowable = Some(ex)))
      .flatMap {
        case Parsed.Success(value, _) => Right(value)
        case f: Parsed.Failure        => Left(ParsingError(f.msg, Some(sm.getLocation(f.index))))
      }
  }

  def parseName(rawValue: String): Either[ParsingError, String] = {
    val sm = SourceMapper(rawValue)
    Try(parse(rawValue, nameOnly(_))).toEither.left
      .map(ex => ParsingError(s"Internal parsing error", innerThrowable = Some(ex)))
      .flatMap {
        case Parsed.Success(value, _) => Right(value)
        case f: Parsed.Failure        => Left(ParsingError(f.msg, Some(sm.getLocation(f.index))))
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

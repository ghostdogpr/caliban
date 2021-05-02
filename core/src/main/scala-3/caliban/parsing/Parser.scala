package caliban.parsing

import caliban.CalibanError.ParsingError
import caliban.parsing.adt._
import zio.IO

object Parser {

  /**
   * Parses the given string into a [[caliban.parsing.adt.Document]] object or fails with a [[caliban.CalibanError.ParsingError]].
   */
  def parseQuery(query: String): IO[ParsingError, Document] = ???

  /**
   * Checks if the query is valid, if not returns an error string.
   */
  def check(query: String): Option[String] = None
}

case class ParsedDocument(definitions: List[Definition], index: Int = 0)

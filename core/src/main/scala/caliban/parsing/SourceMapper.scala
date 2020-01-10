package caliban.parsing

import fastparse.internal.Instrument

trait SourceMapper {

  def getLocation(index: Int): Location

}

object SourceMapper {
  private[parsing] class Builder() extends Instrument {
    private val builder                               = Array.newBuilder[Int]
    def beforeParse(parser: String, index: Int): Unit = ()

    def afterParse(parser: String, index: Int, success: Boolean): Unit = parser match {
      case "lineTerminator" => builder += index
    }

    def result(): SourceMapper = new SourceMapper {
      private val newLines = builder.result()
      def getLocation(index: Int): Location = {
        // We can compute the relative position of each component by computing the position of the last new line
        val indexOfNextNewline = newLines.indexWhere(index < _)
        // Once we have found the next we know two things, first that there have been n - 1 new lines before the current index
        // And 2 that the current column is derived from taking index - indexOf(n - 1)

        val indexOfN = newLines(indexOfNextNewline - 1)

        Location(column = index - indexOfN, indexOfNextNewline)
      }
    }
  }

  val empty = new SourceMapper {
    def getLocation(index: Int): Location = Location.empty
  }
}

case class Location(column: Int, line: Int)

object Location {
  val empty: Location = Location(0, 0)
}

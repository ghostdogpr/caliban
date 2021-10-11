package caliban.parsing

import caliban.parsing.adt.LocationInfo
import fastparse.internal.Util

/**
 * Maps an index to the "friendly" version of an index based on the underlying source.
 */
trait SourceMapper {

  def getLocation(index: Int): LocationInfo

}

object SourceMapper {

  /**
   * Implementation taken from https://github.com/lihaoyi/fastparse/blob/e334ca88b747fb3b6637ef6d76715ad66e048a6c/fastparse/src/fastparse/ParserInput.scala#L123-L131
   *
   * It is used to look up a line/column number pair given a raw index into a source string. The numbers are determined by
   * computing the number of newlines occurring between 0 and the current index.
   */
  final private[parsing] case class DefaultSourceMapper(source: String) extends SourceMapper {
    private[this] lazy val lineNumberLookup = Util.lineNumberLookup(source)

    def getLocation(index: Int): LocationInfo = {
      val line = lineNumberLookup.indexWhere(_ > index) match {
        case -1 => lineNumberLookup.length - 1
        case n  => 0 max (n - 1)
      }

      val col = index - lineNumberLookup(line)
      LocationInfo(column = col + 1, line = line + 1)
    }
  }

  def apply(source: String): SourceMapper = DefaultSourceMapper(source)

  private case object EmptySourceMapper extends SourceMapper {
    def getLocation(index: Int): LocationInfo = LocationInfo.origin
  }

  val empty: SourceMapper = EmptySourceMapper
}

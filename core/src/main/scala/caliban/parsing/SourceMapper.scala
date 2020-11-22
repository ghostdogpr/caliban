package caliban.parsing

import caliban.parsing.adt.LocationInfo
import scala.collection.mutable.ArrayBuffer

/**
 * Maps an index to the "friendly" version of an index based on the underlying source.
 */
trait SourceMapper {

  def getLocation(index: Int): LocationInfo

}

object SourceMapper {

  /**
    * Implementation taken from https://github.com/lihaoyi/fastparse/blob/dd74612224846d3743e19419b3f1191554b973f5/fastparse/src/fastparse/internal/Util.scala#L48
    */
  private def lineNumberLookup(data: String): Array[Int] = {
    val lineStarts = new ArrayBuffer[Int]()
    var i = 0
    var col = 1
    var cr = false
    var prev: Character = null
    while (i < data.length){
      val char = data(i)
      if (char == '\r') {
        if (prev != '\n' && col == 1) lineStarts.append(i)
        col = 1
        cr = true
      }else if (char == '\n') {
        if (prev != '\r' && col == 1) lineStarts.append(i)
        col = 1
        cr = false
      }else{
        if (col == 1) lineStarts.append(i)
        col += 1
        cr = false
      }
      prev = char
      i += 1
    }
    if (col == 1) lineStarts.append(i)

    lineStarts.toArray
  }

  /**
   * Implementation taken from https://github.com/lihaoyi/fastparse/blob/e334ca88b747fb3b6637ef6d76715ad66e048a6c/fastparse/src/fastparse/ParserInput.scala#L123-L131
   *
   * It is used to look up a line/column number pair given a raw index into a source string. The numbers are determined by
   * computing the number of newlines occurring between 0 and the current index.
   */
  private[parsing] case class DefaultSourceMapper(source: String) extends SourceMapper {
    private[this] lazy val lineNumberLookup = SourceMapper.lineNumberLookup(source)

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

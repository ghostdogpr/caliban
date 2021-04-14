package caliban.parsing

import caliban.parsing.adt.LocationInfo

/**
 * Maps an index to the "friendly" version of an index based on the underlying source.
 */
trait SourceMapper {

  def getLocation(index: Int): LocationInfo

}

object SourceMapper {

  def apply(source: String): SourceMapper = ???

  private case object EmptySourceMapper extends SourceMapper {
    def getLocation(index: Int): LocationInfo = LocationInfo.origin
  }

  val empty: SourceMapper = EmptySourceMapper
}

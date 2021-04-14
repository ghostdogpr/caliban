package caliban.parsing.adt

case class LocationInfo(column: Int, line: Int)

object LocationInfo {
  val origin: LocationInfo = LocationInfo(0, 0)
}

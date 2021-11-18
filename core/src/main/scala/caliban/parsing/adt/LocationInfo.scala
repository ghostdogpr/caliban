package caliban.parsing.adt

import caliban.ResponseValue
import caliban.ResponseValue.ObjectValue
import caliban.Value.IntValue

case class LocationInfo(column: Int, line: Int) {
  def toResponseValue: ResponseValue =
    ObjectValue(List("line" -> IntValue(line), "column" -> IntValue(column)))
}

object LocationInfo {
  val origin: LocationInfo = LocationInfo(0, 0)
}

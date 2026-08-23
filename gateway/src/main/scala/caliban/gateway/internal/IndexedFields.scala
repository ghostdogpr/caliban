package caliban.gateway.internal

import caliban.ResponseValue
import caliban.ResponseValue.ObjectValue
import caliban.Value.NullValue

private[internal] final class IndexedFields private (
  value: ObjectValue,
  index: java.util.HashMap[String, ResponseValue]
) {

  def get(name: String): Option[ResponseValue] =
    if (index eq null) value.get(name) else Option(index.get(name))

  def getOrNullValue(name: String): ResponseValue =
    if (index eq null) value.getOrNullValue(name)
    else {
      val found = index.get(name)
      if (found eq null) NullValue else found
    }
}

private[internal] object IndexedFields {

  val WideObjectFields = 16

  def apply(value: ObjectValue): IndexedFields = {
    val fields                                          = value.fields
    var count                                           = 0
    var scan                                            = fields
    while ((scan ne Nil) && count < WideObjectFields) {
      count += 1
      scan = scan.tail
    }
    var index: java.util.HashMap[String, ResponseValue] = null
    if (count >= WideObjectFields) {
      index = new java.util.HashMap
      scan = fields
      while (scan ne Nil) {
        index.put(scan.head._1, scan.head._2)
        scan = scan.tail
      }
    }
    new IndexedFields(value, index)
  }
}

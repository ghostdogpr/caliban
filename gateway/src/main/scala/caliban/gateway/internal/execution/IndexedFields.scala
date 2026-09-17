package caliban.gateway.internal.execution

import caliban.ResponseValue
import caliban.ResponseValue.ObjectValue

/**
 * Uses linear lookup for small objects and an index for wider ones, preserving the first duplicate field.
 */
private[execution] final class IndexedFields private (
  value: ObjectValue,
  index: java.util.HashMap[String, ResponseValue]
) {

  def get(name: String): Option[ResponseValue] =
    Option(getOrNull(name))

  def getOrNull(name: String): ResponseValue =
    if (index eq null) value.getOrNull(name) else index.get(name)

}

private[execution] object IndexedFields {

  def apply(value: ObjectValue): IndexedFields = {
    val fields                                          = value.fields
    val size                                            = fields.size
    var index: java.util.HashMap[String, ResponseValue] = null
    if (size >= IndexThreshold) {
      index = new java.util.HashMap(math.ceil(size / 0.75d).toInt)
      var scan = fields
      while (scan ne Nil) {
        index.putIfAbsent(scan.head._1, scan.head._2)
        scan = scan.tail
      }
    }
    new IndexedFields(value, index)
  }

  val IndexThreshold = 16
}

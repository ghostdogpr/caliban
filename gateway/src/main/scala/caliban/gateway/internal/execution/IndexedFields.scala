package caliban.gateway.internal.execution

import caliban.ResponseValue
import caliban.ResponseValue.ObjectValue
import caliban.PathValue

import scala.collection.mutable

private[internal] final class IndexedFields private (
  value: ObjectValue,
  index: java.util.HashMap[String, ResponseValue]
) {

  def get(name: String): Option[ResponseValue] =
    Option(getOrNull(name))

  def getOrNull(name: String): ResponseValue =
    if (index eq null) value.getOrNull(name) else index.get(name)

}

private[internal] object IndexedFields {

  val WideObjectFields = 16

  def apply(value: ObjectValue): IndexedFields = {
    val fields                                          = value.fields
    val size                                            = fields.size
    var index: java.util.HashMap[String, ResponseValue] = null
    if (size >= WideObjectFields) {
      index = new java.util.HashMap(math.ceil(size / 0.75d).toInt)
      var scan = fields
      while (scan ne Nil) {
        index.putIfAbsent(scan.head._1, scan.head._2)
        scan = scan.tail
      }
    }
    new IndexedFields(value, index)
  }
}

private[internal] final class PathIndex private (root: PathIndex.Node, linear: List[List[PathValue]]) {

  def containsPrefixOf(path: List[PathValue]): Boolean = {
    var paths = linear
    while (paths ne Nil) {
      if (path.startsWith(paths.head)) return true
      paths = paths.tail
    }
    find(path, overlap = false)
  }

  def overlaps(path: List[PathValue]): Boolean = {
    var paths = linear
    while (paths ne Nil) {
      val indexed = paths.head
      if (indexed.startsWith(path) || path.startsWith(indexed)) return true
      paths = paths.tail
    }
    find(path, overlap = true)
  }

  private def find(path: List[PathValue], overlap: Boolean): Boolean = {
    var current   = root
    var remaining = path
    while (current ne null) {
      if (current.terminal) return true
      if (remaining eq Nil) return overlap && !current.children.isEmpty
      current = current.children.get(remaining.head)
      remaining = remaining.tail
    }
    false
  }
}

private[internal] object PathIndex {
  private val LinearLimit = 4
  private val Empty       = new PathIndex(null, Nil)

  private final class Node {
    val children = new java.util.HashMap[PathValue, Node]
    var terminal = false
  }

  def apply(paths: Iterator[List[PathValue]]): PathIndex = {
    val initial = new mutable.ListBuffer[List[PathValue]]
    while (initial.size <= LinearLimit && paths.hasNext) initial += paths.next()
    if (initial.size <= LinearLimit)
      if (initial.isEmpty) Empty else new PathIndex(null, initial.toList)
    else {
      val root     = new Node
      var buffered = initial.toList
      while (buffered ne Nil) {
        add(root, buffered.head)
        buffered = buffered.tail
      }
      while (paths.hasNext) add(root, paths.next())
      new PathIndex(root, Nil)
    }
  }

  private def add(root: Node, path: List[PathValue]): Unit = {
    var current   = root
    var remaining = path
    while (remaining ne Nil) {
      val segment = remaining.head
      var child   = current.children.get(segment)
      if (child eq null) {
        child = new Node
        current.children.put(segment, child)
      }
      current = child
      remaining = remaining.tail
    }
    current.terminal = true
  }
}

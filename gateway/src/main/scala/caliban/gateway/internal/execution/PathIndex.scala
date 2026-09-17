package caliban.gateway.internal.execution

import caliban.PathValue

import scala.collection.mutable

/**
 * Scans small path sets directly and uses a prefix tree for larger sets.
 */
private[execution] final class PathIndex private (root: PathIndex.Node, linear: List[List[PathValue]]) {

  /**
   * Whether an indexed path is an ancestor of, or equal to, the given path.
   */
  def containsPrefixOf(path: List[PathValue]): Boolean = {
    var paths = linear
    while (paths ne Nil) {
      if (path.startsWith(paths.head)) return true
      paths = paths.tail
    }
    find(path, overlap = false)
  }

  /**
   * Whether either path is an ancestor of, or equal to, the other.
   */
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

private[execution] object PathIndex {
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

  private val LinearLimit = 4
  private val Empty       = new PathIndex(null, Nil)

  private final class Node {
    val children = new java.util.HashMap[PathValue, Node]
    var terminal = false
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

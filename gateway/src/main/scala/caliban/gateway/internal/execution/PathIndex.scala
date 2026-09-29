package caliban.gateway.internal.execution

import caliban.PathValue

/**
 * Prefix tree over a set of paths.
 */
private[execution] final class PathIndex private (root: PathIndex.Node) {

  /**
   * Whether an indexed path is an ancestor of, or equal to, the given path.
   */
  def containsPrefixOf(path: List[PathValue]): Boolean = find(path, overlap = false)

  /**
   * Whether either path is an ancestor of, or equal to, the other.
   */
  def overlaps(path: List[PathValue]): Boolean = find(path, overlap = true)

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
  def apply(paths: Iterator[List[PathValue]]): PathIndex =
    if (!paths.hasNext) empty
    else {
      val root = new Node
      paths.foreach(add(root, _))
      new PathIndex(root)
    }

  val empty: PathIndex = new PathIndex(null)

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

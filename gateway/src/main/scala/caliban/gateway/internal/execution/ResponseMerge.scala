package caliban.gateway.internal.execution

import caliban.{ PathValue, ResponseValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, NullValue, StringValue }

import scala.collection.mutable

/**
 * Ordered response merging. Root merges retain independent non-null results;
 * entity patches overwrite fetched values, while blocked patches only fill missing values.
 */
private[gateway] object ResponseMerge {
  type Patch = (List[PathValue], Edit)

  sealed trait Edit
  final case class Overwrite(value: ResponseValue) extends Edit
  final case class Fill(value: ResponseValue)      extends Edit

  def applyPatches(value: ResponseValue, patches: List[Patch]): ResponseValue =
    patches match {
      case (path, edit) :: Nil => editAt(value, path, edit)
      case _                   =>
        val root = new PatchNode
        patches.foreach { case (path, edit) => root.add(path, edit) }
        root.patch(value)
    }

  def mergeRootValue(left: ResponseValue, right: ResponseValue): ResponseValue =
    merge(left, right, retainNonNull = true)

  private def applyEdit(value: ResponseValue, edit: Edit): ResponseValue =
    edit match {
      case Overwrite(patch)  => merge(value, patch, retainNonNull = false)
      case Fill(patch)       => merge(patch, value, retainNonNull = false)
      case group: PatchGroup => group.patch(value)
    }

  private final class PatchNode {
    private var entries: List[Edit] = Nil

    def add(path: List[PathValue], edit: Edit): Unit =
      path match {
        case Nil             => entries = edit :: entries
        case segment :: rest =>
          val group = entries match {
            case (group: PatchGroup) :: _ => group
            case _                        =>
              val created = new PatchGroup
              entries = created :: entries
              created
          }
          group.nodeAt(segment).add(rest, edit)
      }

    def patch(value: ResponseValue): ResponseValue =
      entries.foldRight(value)((edit, current) => applyEdit(current, edit))
  }

  private final class PatchGroup extends Edit {
    private var keys: java.util.HashMap[String, PatchNode] = null
    private var indices: mutable.LongMap[PatchNode]        = null

    def nodeAt(segment: PathValue): PatchNode =
      segment match {
        case StringValue(key)          =>
          if (keys eq null) keys = new java.util.HashMap[String, PatchNode]
          var node = keys.get(key)
          if (node eq null) {
            node = new PatchNode
            keys.put(key, node)
          }
          node
        case IntValue.IntNumber(index) =>
          if (indices eq null) indices = mutable.LongMap.empty
          indices.getOrElseUpdate(index.toLong, new PatchNode)
      }

    def patch(value: ResponseValue): ResponseValue =
      value match {
        case ObjectValue(fields) if keys ne null  =>
          ObjectValue(fields.map { field =>
            val node = keys.get(field._1)
            if (node eq null) field else (field._1, node.patch(field._2))
          })
        case ListValue(values) if indices ne null =>
          var index = 0
          ListValue(values.map { nested =>
            val node = indices.getOrNull(index.toLong)
            index += 1
            if (node eq null) nested else node.patch(nested)
          })
        case other                                => other
      }
  }

  private def editAt(value: ResponseValue, path: List[PathValue], edit: Edit): ResponseValue =
    path match {
      case Nil                               => applyEdit(value, edit)
      case StringValue(key) :: tail          =>
        value match {
          case ObjectValue(fields) =>
            ObjectValue(fields.map(field => if (field._1 == key) (key, editAt(field._2, tail, edit)) else field))
          case other               => other
        }
      case IntValue.IntNumber(index) :: tail =>
        value match {
          case ListValue(values) if index >= 0 => ListValue(editValueAt(values, index, tail, edit))
          case other                           => other
        }
    }

  private def editValueAt(
    values: List[ResponseValue],
    index: Int,
    path: List[PathValue],
    edit: Edit
  ): List[ResponseValue] =
    values.splitAt(index) match {
      case (prefix, nested :: tail) => prefix ::: (editAt(nested, path, edit) :: tail)
      case _                        => values
    }

  private def merge(left: ResponseValue, right: ResponseValue, retainNonNull: Boolean): ResponseValue =
    (left, right) match {
      case (leftObject: ObjectValue, rightObject: ObjectValue)                                    =>
        ObjectValue(mergeFields(leftObject.fields, rightObject.fields, retainNonNull))
      case (value, NullValue) if retainNonNull                                                    => value
      case (ListValue(leftValues), ListValue(rightValues)) if leftValues.size == rightValues.size =>
        ListValue(leftValues.zip(rightValues).map { case (leftValue, rightValue) =>
          merge(leftValue, rightValue, retainNonNull)
        })
      case (_, value)                                                                             => value
    }

  private def mergeFields(
    left: List[(String, ResponseValue)],
    right: List[(String, ResponseValue)],
    retainNonNull: Boolean
  ): List[(String, ResponseValue)] = {
    val leftSize                                            = left.size
    val positions                                           = indexPositions(left, leftSize)
    val matches                                             = new Array[ResponseValue](leftSize)
    var extras: mutable.ListBuffer[(String, ResponseValue)] = null
    var remaining                                           = right
    while (remaining ne Nil) {
      val field    = remaining.head
      val position =
        if (positions ne null) indexedPositionOf(positions, field._1) else firstPositionOf(left, field._1)
      if (position < 0) {
        if (extras eq null) extras = new mutable.ListBuffer
        extras += field
      } else if (matches(position) eq null) matches(position) = field._2
      remaining = remaining.tail
    }
    val merged                                              = new mutable.ListBuffer[(String, ResponseValue)]
    var position                                            = 0
    remaining = left
    while (remaining ne Nil) {
      val field   = remaining.head
      val matched = matches(position)
      merged += (if (matched eq null) field else (field._1, merge(field._2, matched, retainNonNull)))
      position += 1
      remaining = remaining.tail
    }
    if (extras ne null) merged ++= extras
    merged.toList
  }

  /**
   * Returns null for objects below the index threshold, which `firstPositionOf` scans instead.
   */
  private def indexPositions(fields: List[(String, ResponseValue)], size: Int): java.util.HashMap[String, Integer] =
    if (size < IndexedFields.IndexThreshold) null
    else {
      val positions = new java.util.HashMap[String, Integer](size * 2)
      var position  = 0
      var remaining = fields
      while (remaining ne Nil) {
        positions.putIfAbsent(remaining.head._1, Integer.valueOf(position))
        position += 1
        remaining = remaining.tail
      }
      positions
    }

  private def indexedPositionOf(positions: java.util.HashMap[String, Integer], name: String): Int = {
    val position = positions.get(name)
    if (position eq null) -1 else position.intValue
  }

  private def firstPositionOf(fields: List[(String, ResponseValue)], name: String): Int = {
    var position  = 0
    var remaining = fields
    while ((remaining ne Nil) && !remaining.head._1.equals(name)) {
      position += 1
      remaining = remaining.tail
    }
    if (remaining eq Nil) -1 else position
  }
}

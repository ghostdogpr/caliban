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
  type Patch = (List[PathValue], ResponseValue)

  def applyPatches(value: ResponseValue, patches: List[Patch]): ResponseValue =
    patches match {
      case Nil                  => value
      case (path, patch) :: Nil => mergeAt(value, path, patch, onlyMissing = false)
      case _                    =>
        val root = new PatchNode
        patches.foreach { case (path, patch) => root.add(path, patch) }
        root.patch(value)
    }

  def fillMissing(value: ResponseValue, patches: List[Patch]): ResponseValue = {
    var filled    = value
    var remaining = patches
    while (remaining ne Nil) {
      val patch = remaining.head
      filled = mergeAt(filled, patch._1, patch._2, onlyMissing = true)
      remaining = remaining.tail
    }
    filled
  }

  def mergeObject(left: ResponseValue, right: ResponseValue): ResponseValue =
    mergeValues(left, right) {
      case (ListValue(leftValues), ListValue(rightValues)) if leftValues.size == rightValues.size =>
        ListValue(leftValues.zip(rightValues).map { case (leftValue, rightValue) =>
          mergeObject(leftValue, rightValue)
        })
      case (_, value)                                                                             => value
    }

  def mergeRootValue(left: ResponseValue, right: ResponseValue): ResponseValue =
    mergeValues(left, right) {
      case (NullValue, value)                                                                     => value
      case (value, NullValue)                                                                     => value
      case (ListValue(leftValues), ListValue(rightValues)) if leftValues.size == rightValues.size =>
        ListValue(leftValues.zip(rightValues).map { case (leftValue, rightValue) =>
          mergeRootValue(leftValue, rightValue)
        })
      case (_, value)                                                                             => value
    }

  private sealed trait PatchEntry

  private final case class PatchValue(value: ResponseValue) extends PatchEntry

  private final class PatchNode {
    private var entries: List[PatchEntry] = Nil

    def add(path: List[PathValue], patch: ResponseValue): Unit =
      path match {
        case Nil             => entries = PatchValue(patch) :: entries
        case segment :: rest =>
          val group = entries match {
            case (group: PatchGroup) :: _ => group
            case _                        =>
              val created = new PatchGroup
              entries = created :: entries
              created
          }
          group.nodeAt(segment).add(rest, patch)
      }

    def patch(value: ResponseValue): ResponseValue =
      entries.foldRight(value) {
        case (group: PatchGroup, current) => group.patch(current)
        case (PatchValue(patch), current) => mergeObject(current, patch)
      }
  }

  private final class PatchGroup extends PatchEntry {
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

  private def mergeAt(
    value: ResponseValue,
    path: List[PathValue],
    patch: ResponseValue,
    onlyMissing: Boolean
  ): ResponseValue =
    path match {
      case Nil                               => if (onlyMissing) mergeObject(patch, value) else mergeObject(value, patch)
      case StringValue(key) :: tail          =>
        value match {
          case ObjectValue(fields) => ObjectValue(updateFieldAt(fields, key, tail, patch, onlyMissing))
          case other               => other
        }
      case IntValue.IntNumber(index) :: tail =>
        value match {
          case ListValue(values) if index >= 0 => ListValue(updateValueAt(values, index, tail, patch, onlyMissing))
          case other                           => other
        }
    }

  private def updateFieldAt(
    fields: List[(String, ResponseValue)],
    key: String,
    path: List[PathValue],
    patch: ResponseValue,
    onlyMissing: Boolean
  ): List[(String, ResponseValue)] = {
    val updated   = new mutable.ListBuffer[(String, ResponseValue)]
    var found     = false
    var remaining = fields
    while (remaining ne Nil) {
      val field = remaining.head
      if (field._1.equals(key)) {
        found = true
        updated += ((key, mergeAt(field._2, path, patch, onlyMissing)))
      } else updated += field
      remaining = remaining.tail
    }
    if (found) updated.toList else fields
  }

  private def updateValueAt(
    values: List[ResponseValue],
    index: Int,
    path: List[PathValue],
    patch: ResponseValue,
    onlyMissing: Boolean
  ): List[ResponseValue] = {
    var reversedPrefix: List[ResponseValue] = Nil
    var remaining                           = values
    var position                            = 0
    while (position < index && (remaining ne Nil)) {
      reversedPrefix = remaining.head :: reversedPrefix
      remaining = remaining.tail
      position += 1
    }
    remaining match {
      case nested :: tail => reversedPrefix reverse_::: (mergeAt(nested, path, patch, onlyMissing) :: tail)
      case Nil            => values
    }
  }

  private def mergeValues(left: ResponseValue, right: ResponseValue)(
    mergeLeaf: (ResponseValue, ResponseValue) => ResponseValue
  ): ResponseValue =
    (left, right) match {
      case (leftObject: ObjectValue, rightObject: ObjectValue) =>
        ObjectValue(mergeFields(leftObject.fields, rightObject.fields)(mergeLeaf))
      case _                                                   => mergeLeaf(left, right)
    }

  private def mergeFields(left: List[(String, ResponseValue)], right: List[(String, ResponseValue)])(
    mergeLeaf: (ResponseValue, ResponseValue) => ResponseValue
  ): List[(String, ResponseValue)] = {
    val leftSize                                            = left.size
    val positions                                           = indexPositions(left, leftSize)
    val matches                                             = new Array[ResponseValue](leftSize)
    var extras: mutable.ListBuffer[(String, ResponseValue)] = null
    var remaining                                           = right
    while (remaining ne Nil) {
      val field    = remaining.head
      val position =
        if (positions ne null) indexedPositionOf(positions, field._1) else lastPositionOf(left, field._1)
      if (position >= 0) matches(position) = field._2
      else {
        if (extras eq null) extras = new mutable.ListBuffer
        extras += field
      }
      remaining = remaining.tail
    }
    val merged                                              = new mutable.ListBuffer[(String, ResponseValue)]
    var position                                            = 0
    remaining = left
    while (remaining ne Nil) {
      val field   = remaining.head
      val matched = matches(position)
      merged += (if (matched eq null) field else (field._1, mergeValues(field._2, matched)(mergeLeaf)))
      position += 1
      remaining = remaining.tail
    }
    if (extras ne null) merged ++= extras
    merged.toList
  }

  /**
   * Returns null for objects below the index threshold, which `lastPositionOf` scans instead.
   */
  private def indexPositions(fields: List[(String, ResponseValue)], size: Int): java.util.HashMap[String, Integer] =
    if (size < IndexedFields.IndexThreshold) null
    else {
      val positions = new java.util.HashMap[String, Integer](size * 2)
      var position  = 0
      var remaining = fields
      while (remaining ne Nil) {
        positions.put(remaining.head._1, Integer.valueOf(position))
        position += 1
        remaining = remaining.tail
      }
      positions
    }

  private def indexedPositionOf(positions: java.util.HashMap[String, Integer], name: String): Int = {
    val position = positions.get(name)
    if (position eq null) -1 else position.intValue
  }

  private def lastPositionOf(fields: List[(String, ResponseValue)], name: String): Int = {
    var position  = 0
    var found     = -1
    var remaining = fields
    while (remaining ne Nil) {
      if (remaining.head._1.equals(name)) found = position
      position += 1
      remaining = remaining.tail
    }
    found
  }
}

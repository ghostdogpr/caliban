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

  def applyPatches(value: ResponseValue, patches: List[Patch]): ResponseValue = {
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

  private def merge(left: ResponseValue, right: ResponseValue, retainNonNull: Boolean): ResponseValue =
    (left, right) match {
      case (leftObject: ObjectValue, rightObject: ObjectValue)                                    =>
        ObjectValue(mergeFields(leftObject, rightObject, retainNonNull))
      case (value, NullValue) if retainNonNull                                                    => value
      case (ListValue(leftValues), ListValue(rightValues)) if leftValues.size == rightValues.size =>
        ListValue(leftValues.zip(rightValues).map { case (leftValue, rightValue) =>
          merge(leftValue, rightValue, retainNonNull)
        })
      case (_, value)                                                                             => value
    }

  private def mergeFields(
    left: ObjectValue,
    right: ObjectValue,
    retainNonNull: Boolean
  ): List[(String, ResponseValue)] = {
    val leftFields        = IndexedFields(left)
    val (matched, extras) = right.fields.partition(field => leftFields.getOrNull(field._1) ne null)
    val merged            =
      if (matched.isEmpty) left.fields
      else {
        val matches = IndexedFields(ObjectValue(matched))
        left.fields.map { field =>
          val value = matches.getOrNull(field._1)
          if (value eq null) field else field._1 -> merge(field._2, value, retainNonNull)
        }
      }
    merged ::: extras
  }
}

package caliban.gateway.internal.execution

import caliban.{ CalibanError, PathValue, ResponseValue }
import caliban.execution.Field
import caliban.gateway.internal.execution.ResponseCompletion._
import caliban.gateway.internal.planning.OperationPlan.TypenameSelection
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue }

import scala.collection.mutable

/**
 * Completes fetched values against the client selections, including GraphQL null propagation.
 */
private[gateway] final class ResponseCompletion(typenameSelections: List[TypenameSelection]) {
  def complete(fields: List[Field], value: ResponseValue, errors: List[CalibanError]): Completion =
    completeObject(fields, value, Nil, ErrorPathIndex(errors))

  private def completeObject(
    fields: List[Field],
    value: ResponseValue,
    path: List[PathValue],
    sourceErrors: ErrorPathIndex
  ): Completion =
    value match {
      case obj: ObjectValue =>
        val completed = new mutable.ListBuffer[(String, ResponseValue)]
        val errors    = new mutable.ListBuffer[CalibanError.ExecutionError]
        var missing   = false
        var remaining = fields

        val lookup = IndexedFields(obj)

        while (remaining ne Nil) {
          val field     = remaining.head
          val name      = field.aliasedName
          val fieldPath = PathValue.Key(name) :: path
          val value     = lookup.getOrNull(name)
          val result    =
            if (value ne null)
              completeValue(field.fieldType, field, value, fieldPath, sourceErrors)
            else {
              val invalid = Completed(NullValue, invalidSourceValueErrors(fieldPath.reverse, sourceErrors))
              if (field.fieldType.kind == __TypeKind.NON_NULL)
                enforceNonNull(invalid, field, fieldPath, sourceErrors)
              else invalid
            }
          if (result.errors ne Nil) errors ++= result.errors
          result match {
            case Completed(completedValue, _) => completed += (name -> completedValue)
            case _: BubbleNull                => missing = true
          }
          remaining = remaining.tail
        }
        if (missing) BubbleNull(errors.toList)
        else Completed(ObjectValue(completed.toList), errors.toList)
      case _                => Completed(NullValue, invalidSourceValueErrors(path.reverse, sourceErrors))
    }

  private def completeValue(
    fieldType: __Type,
    field: Field,
    value: ResponseValue,
    path: List[PathValue],
    sourceErrors: ErrorPathIndex
  ): Completion =
    fieldType.kind match {
      case __TypeKind.NON_NULL                     =>
        val completed = fieldType.ofType
          .map(completeValue(_, field, value, path, sourceErrors))
          .getOrElse(Completed(NullValue, Nil))
        enforceNonNull(completed, field, path, sourceErrors)
      case _ if value == NullValue                 => Completed(NullValue, Nil)
      case __TypeKind.LIST                         =>
        (value, fieldType.ofType) match {
          case (ListValue(values), Some(itemType)) =>
            val abstractType  = listItemAbstractType(itemType)
            val abstractPlan  =
              if (abstractType eq null) null else abstractCompletion(abstractType, field, path)
            val itemIsNonNull = itemType.kind == __TypeKind.NON_NULL
            val completed     = new mutable.ListBuffer[ResponseValue]
            val errors        = new mutable.ListBuffer[CalibanError.ExecutionError]
            var missing       = false
            var index         = 0
            var remaining     = values
            while (remaining ne Nil) {
              val itemPath = PathValue.Index(index) :: path
              val result   =
                if (abstractPlan eq null)
                  completeValue(itemType, field, remaining.head, itemPath, sourceErrors)
                else {
                  val item =
                    if (remaining.head == NullValue) Completed(NullValue, Nil)
                    else
                      completeAbstract(abstractPlan, field, remaining.head, itemPath, sourceErrors)
                  if (itemIsNonNull) enforceNonNull(item, field, itemPath, sourceErrors) else item
                }
              if (result.errors ne Nil) errors ++= result.errors
              result match {
                case Completed(completedValue, _) => completed += completedValue
                case _: BubbleNull                => missing = true
              }
              index += 1
              remaining = remaining.tail
            }
            if (missing) Completed(NullValue, errors.toList)
            else Completed(ListValue(completed.toList), errors.toList)
          case _                                   =>
            Completed(NullValue, invalidSourceValueErrors(path.reverse, sourceErrors))
        }
      case __TypeKind.INTERFACE | __TypeKind.UNION =>
        completeAbstract(
          abstractCompletion(fieldType, field, path),
          field,
          value,
          path,
          sourceErrors
        )
      case __TypeKind.OBJECT                       =>
        completeNestedObject(
          fieldType.innerType.name.getOrElse(""),
          field,
          value,
          path,
          sourceErrors
        )
      case __TypeKind.ENUM                         =>
        value match {
          case StringValue(name) if fieldType.allEnumValues.exists(_.name == name) => Completed(value, Nil)
          case EnumValue(name) if fieldType.allEnumValues.exists(_.name == name)   => Completed(value, Nil)
          case _                                                                   =>
            val errors =
              if (sourceErrors.overlaps(path.reverse)) Nil
              else {
                val enumName = fieldType.name.getOrElse("Unknown")
                List(
                  CalibanError.ExecutionError(
                    s"Invalid value for enum '$enumName'.",
                    path.reverse,
                    Some(field.locationInfo)
                  )
                )
              }
            Completed(NullValue, errors)
        }
      case __TypeKind.SCALAR                       =>
        val valid = fieldType.name match {
          case Some("String") | Some("ID") =>
            value match {
              case _: StringValue => true
              case _              => false
            }
          case Some("Int")                 =>
            value match {
              case _: IntValue.IntNumber                              => true
              case IntValue.LongNumber(number) if number.isValidInt   => true
              case IntValue.BigIntNumber(number) if number.isValidInt => true
              case _                                                  => false
            }
          case Some("Float")               =>
            value match {
              case _: IntValue | _: FloatValue => true
              case _                           => false
            }
          case Some("Boolean")             =>
            value match {
              case _: BooleanValue => true
              case _               => false
            }
          case _                           => true
        }
        if (valid) Completed(value, Nil)
        else Completed(NullValue, invalidSourceValueErrors(path.reverse, sourceErrors))
      case _                                       => Completed(value, Nil)
    }

  private def enforceNonNull(
    completed: Completion,
    field: Field,
    path: List[PathValue],
    sourceErrors: ErrorPathIndex
  ): Completion =
    completed match {
      case Completed(NullValue, _) =>
        BubbleNull(completed.errors ::: nullViolation(field, path.reverse, sourceErrors, completed.errors.nonEmpty))
      case _                       => completed
    }

  private def listItemAbstractType(itemType: __Type): __Type =
    itemType.kind match {
      case __TypeKind.INTERFACE | __TypeKind.UNION => itemType
      case __TypeKind.NON_NULL                     =>
        itemType.ofType match {
          case Some(inner) if inner.kind == __TypeKind.INTERFACE || inner.kind == __TypeKind.UNION => inner
          case _                                                                                   => null
        }
      case _                                       => null
    }

  private def completeAbstract(
    completion: AbstractCompletion,
    field: Field,
    value: ResponseValue,
    path: List[PathValue],
    sourceErrors: ErrorPathIndex
  ): Completion = {
    val runtime = runtimeType(value, completion.runtimeTypes)
    runtime.filter(name => completion.possible.isEmpty || completion.possible.contains(name)) match {
      case Some(typeName)                                         =>
        completeNestedObject(typeName, field, value, path, sourceErrors)
      case None if runtime.isEmpty && !completion.requiresRuntime =>
        completeNestedObject(completion.defaultType, field, value, path, sourceErrors)
      case None                                                   =>
        Completed(NullValue, invalidSourceValueErrors(path.reverse, sourceErrors))
    }
  }

  private def abstractCompletion(
    fieldType: __Type,
    field: Field,
    path: List[PathValue]
  ): AbstractCompletion = {
    val matching  = new mutable.ListBuffer[TypenameSelection]
    val fallback  = new mutable.ListBuffer[TypenameSelection]
    val expected  = responsePath(path)
    var remaining = typenameSelections
    while (remaining ne Nil) {
      val selection = remaining.head
      if (selection.path == expected) matching += selection else fallback += selection
      remaining = remaining.tail
    }
    AbstractCompletion(
      fieldType.possibleTypes.getOrElse(Nil).flatMap(_.name).toSet,
      RuntimeTypeLookup(
        matching.toList,
        fallback.toList,
        field.fields.iterator.filter(_.name == "__typename").map(_.aliasedName).toList
      ),
      field.fields.exists(child => child.name == "__typename" || child._condition.nonEmpty || child.targets.nonEmpty),
      fieldType.innerType.name.getOrElse("")
    )
  }

  private def completeNestedObject(
    typeName: String,
    field: Field,
    value: ResponseValue,
    path: List[PathValue],
    sourceErrors: ErrorPathIndex
  ): Completion = {
    val completed = completeObject(field.collectFields(typeName), value, path, sourceErrors)
    if (completed.bubblesNull) Completed(NullValue, completed.errors) else completed
  }

  private def nullViolation(
    field: Field,
    path: List[PathValue],
    sourceErrors: ErrorPathIndex,
    hasCompletedErrors: Boolean
  ): List[CalibanError.ExecutionError] =
    if (hasCompletedErrors || sourceErrors.overlaps(path)) Nil
    else {
      val parent = field.parentType.flatMap(_.name).getOrElse("Unknown")
      List(
        CalibanError.ExecutionError(
          s"Cannot return null for non-nullable field $parent.${field.name}.",
          path,
          Some(field.locationInfo)
        )
      )
    }

  private def invalidSourceValueErrors(
    path: List[PathValue],
    sourceErrors: ErrorPathIndex
  ): List[CalibanError.ExecutionError] =
    if ((path.isEmpty && sourceErrors.nonEmpty) || sourceErrors.overlaps(path)) Nil else List(RemoteError.at(path))

  private def runtimeType(
    value: ResponseValue,
    runtimeTypes: RuntimeTypeLookup
  ): Option[String] =
    value match {
      case obj: ObjectValue =>
        val lookup = IndexedFields(obj)
        runtimeTypes.matching.iterator
          .flatMap(selection => lookup.get(selection.responseName))
          .collectFirst { case StringValue(name) => name }
          .orElse(runtimeTypes.selectedAliases.iterator.flatMap(lookup.get(_)).collectFirst { case StringValue(name) =>
            name
          })
          .orElse(lookup.get("__typename").collect { case StringValue(name) => name })
          .orElse(
            runtimeTypes.fallback.iterator
              .flatMap(selection => lookup.get(selection.responseName))
              .collectFirst { case StringValue(name) => name }
          )
      case _                => None
    }

  private def responsePath(path: List[PathValue]): Vector[String] = {
    var names: List[String] = Nil
    var remaining           = path
    while (remaining ne Nil) {
      remaining.head match {
        case PathValue.Key(name) => names = name :: names
        case _                   => ()
      }
      remaining = remaining.tail
    }
    names.toVector
  }

}

private[gateway] object ResponseCompletion {
  private final case class RuntimeTypeLookup(
    matching: List[TypenameSelection],
    fallback: List[TypenameSelection],
    selectedAliases: List[String]
  )

  private final case class AbstractCompletion(
    possible: Set[String],
    runtimeTypes: RuntimeTypeLookup,
    requiresRuntime: Boolean,
    defaultType: String
  )

  private final class ErrorPathIndex private (paths: PathIndex, val nonEmpty: Boolean) {
    def overlaps(path: List[PathValue]): Boolean = paths.overlaps(path)
  }

  private object ErrorPathIndex {
    private val Empty = new ErrorPathIndex(PathIndex(Iterator.empty), nonEmpty = false)

    def apply(errors: List[CalibanError]): ErrorPathIndex =
      if (errors.isEmpty) Empty
      else
        new ErrorPathIndex(
          PathIndex(errors.iterator.collect {
            case error: CalibanError.ExecutionError if error.path.nonEmpty => error.path
          }),
          nonEmpty = true
        )
  }

  sealed trait Completion {
    def errors: List[CalibanError.ExecutionError]
    def bubblesNull: Boolean
    def toResponseValue: ResponseValue
  }

  final case class Completed(value: ResponseValue, errors: List[CalibanError.ExecutionError]) extends Completion {
    def bubblesNull: Boolean           = false
    def toResponseValue: ResponseValue = value
  }

  /**
   * A non-null violation that must propagate to the nearest nullable boundary.
   */
  final case class BubbleNull(errors: List[CalibanError.ExecutionError]) extends Completion {
    def bubblesNull: Boolean           = true
    def toResponseValue: ResponseValue = NullValue
  }
}

/**
 * Ordered response merging. Root merges retain independent non-null results;
 * entity patches overwrite fetched values, while blocked patches only fill missing values.
 */
private[gateway] object ResponseMerge {
  def applyPatches(
    value: ResponseValue,
    patches: List[(List[PathValue], ResponseValue)]
  ): ResponseValue =
    patches match {
      case Nil                  => value
      case (path, patch) :: Nil => mergeAt(value, path, patch)
      case _                    =>
        var current   = value
        var remaining = patches
        val nested    = new mutable.ListBuffer[(List[PathValue], ResponseValue)]
        while (remaining ne Nil) {
          val patch = remaining.head
          if (patch._1.isEmpty) {
            if (nested.nonEmpty) {
              current = applyNestedPatches(current, nested.toList)
              nested.clear()
            }
            current = mergeObject(current, patch._2)
          } else nested += patch
          remaining = remaining.tail
        }
        if (nested.isEmpty) current else applyNestedPatches(current, nested.toList)
    }

  private def applyNestedPatches(
    value: ResponseValue,
    patches: List[(List[PathValue], ResponseValue)]
  ): ResponseValue =
    patches match {
      case (path, patch) :: Nil => mergeAt(value, path, patch)
      case _                    => applyGroupedPatches(value, patches)
    }

  private def applyGroupedPatches(
    value: ResponseValue,
    patches: List[(List[PathValue], ResponseValue)]
  ): ResponseValue =
    value match {
      case ObjectValue(fields) if patches.lengthCompare(4) <= 0 =>
        var byKey: List[(String, mutable.ListBuffer[(List[PathValue], ResponseValue)])] = Nil
        var remaining                                                                   = patches
        while (remaining ne Nil) {
          val patch = remaining.head
          patch._1 match {
            case StringValue(key) :: tail =>
              var groups = byKey
              while ((groups ne Nil) && !groups.head._1.equals(key)) groups = groups.tail
              groups match {
                case (_, bucket) :: _ => bucket += (tail -> patch._2)
                case Nil              =>
                  val bucket = new mutable.ListBuffer[(List[PathValue], ResponseValue)]
                  bucket += (tail -> patch._2)
                  byKey = (key -> bucket) :: byKey
              }
            case _                        => ()
          }
          remaining = remaining.tail
        }
        if (byKey.isEmpty) value
        else
          ObjectValue(fields.map { field =>
            var groups = byKey
            while ((groups ne Nil) && !groups.head._1.equals(field._1)) groups = groups.tail
            groups match {
              case (_, nestedPatches) :: _ => (field._1, applyPatches(field._2, nestedPatches.toList))
              case Nil                     => field
            }
          })
      case ObjectValue(fields)                                  =>
        val byKey     = new java.util.HashMap[String, mutable.ListBuffer[(List[PathValue], ResponseValue)]]
        var remaining = patches
        while (remaining ne Nil) {
          val patch = remaining.head
          patch._1 match {
            case StringValue(key) :: tail =>
              var bucket = byKey.get(key)
              if (bucket eq null) {
                bucket = new mutable.ListBuffer[(List[PathValue], ResponseValue)]
                byKey.put(key, bucket)
              }
              bucket += (tail -> patch._2)
            case _                        => ()
          }
          remaining = remaining.tail
        }
        if (byKey.isEmpty) value
        else
          ObjectValue(fields.map { field =>
            val nestedPatches = byKey.get(field._1)
            if (nestedPatches eq null) field
            else (field._1, applyPatches(field._2, nestedPatches.toList))
          })
      case ListValue(values)                                    =>
        val byIndex = mutable.LongMap.empty[mutable.ListBuffer[(List[PathValue], ResponseValue)]]
        patches.foreach { patch =>
          patch._1 match {
            case IntValue.IntNumber(index) :: tail if index >= 0 =>
              byIndex.getOrElseUpdate(index.toLong, mutable.ListBuffer.empty) += (tail -> patch._2)
            case _                                               => ()
          }
        }
        if (byIndex.isEmpty) value
        else {
          var index = 0
          ListValue(values.map { nested =>
            val nestedPatches = byIndex.getOrNull(index.toLong)
            val patched       = if (nestedPatches eq null) nested else applyPatches(nested, nestedPatches.toList)
            index += 1
            patched
          })
        }
      case other                                                => other
    }

  private def mergeAt(
    value: ResponseValue,
    path: List[PathValue],
    patch: ResponseValue,
    missingWins: Boolean = false
  ): ResponseValue =
    path match {
      case Nil          => if (missingWins) mergeObject(patch, value) else mergeObject(value, patch)
      case head :: tail =>
        head match {
          case StringValue(key)          =>
            value match {
              case ObjectValue(fields) => ObjectValue(updateFieldAt(fields, key, tail, patch, missingWins))
              case other               => other
            }
          case IntValue.IntNumber(index) =>
            value match {
              case ListValue(values) if index >= 0 =>
                ListValue(updateValueAt(values, index, tail, patch, missingWins))
              case other                           => other
            }
        }
    }

  def mergeMissingAt(value: ResponseValue, path: List[PathValue], patch: ResponseValue): ResponseValue =
    mergeAt(value, path, patch, missingWins = true)

  private def updateFieldAt(
    fields: List[(String, ResponseValue)],
    key: String,
    path: List[PathValue],
    patch: ResponseValue,
    missingWins: Boolean
  ): List[(String, ResponseValue)] = {
    val updated   = new mutable.ListBuffer[(String, ResponseValue)]
    var found     = false
    var remaining = fields
    while (remaining ne Nil) {
      val field = remaining.head
      if (field._1.equals(key)) {
        found = true
        updated += ((key, mergeAt(field._2, path, patch, missingWins)))
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
    missingWins: Boolean
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
      case nested :: tail => reversedPrefix reverse_::: (mergeAt(nested, path, patch, missingWins) :: tail)
      case Nil            => values
    }
  }

  private[gateway] def mergeObject(left: ResponseValue, right: ResponseValue): ResponseValue =
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

  private def mergeValues(
    left: ResponseValue,
    right: ResponseValue
  )(mergeLeaf: (ResponseValue, ResponseValue) => ResponseValue): ResponseValue =
    (left, right) match {
      case (leftObj: ObjectValue, rightObj: ObjectValue) =>
        val leftFields                                          = leftObj.fields
        var leftSize                                            = 0
        var remaining                                           = leftFields
        while (remaining ne Nil) {
          leftSize += 1
          remaining = remaining.tail
        }
        var positions: java.util.HashMap[String, Integer]       = null
        if (leftSize >= IndexedFields.WideObjectFields) {
          positions = new java.util.HashMap[String, Integer](leftSize * 2)
          var position = 0
          remaining = leftFields
          while (remaining ne Nil) {
            positions.put(remaining.head._1, Integer.valueOf(position))
            position += 1
            remaining = remaining.tail
          }
        }
        val matches                                             = new Array[ResponseValue](leftSize)
        var extras: mutable.ListBuffer[(String, ResponseValue)] = null
        var rightRemaining                                      = rightObj.fields
        while (rightRemaining ne Nil) {
          val field   = rightRemaining.head
          var matched = false
          if (positions ne null) {
            val position = positions.get(field._1)
            if (position ne null) {
              matches(position.intValue) = field._2
              matched = true
            }
          } else {
            var position        = 0
            var matchedPosition = -1
            remaining = leftFields
            while (remaining ne Nil) {
              if (remaining.head._1.equals(field._1)) matchedPosition = position
              position += 1
              remaining = remaining.tail
            }
            if (matchedPosition >= 0) {
              matches(matchedPosition) = field._2
              matched = true
            }
          }
          if (!matched) {
            if (extras eq null) extras = new mutable.ListBuffer
            extras += field
          }
          rightRemaining = rightRemaining.tail
        }
        val merged                                              = new mutable.ListBuffer[(String, ResponseValue)]
        var position                                            = 0
        remaining = leftFields
        while (remaining ne Nil) {
          val field   = remaining.head
          val matched = matches(position)
          merged += (if (matched eq null) field else (field._1, mergeValues(field._2, matched)(mergeLeaf)))
          position += 1
          remaining = remaining.tail
        }
        if (extras ne null) merged ++= extras
        ObjectValue(merged.toList)
      case _                                             => mergeLeaf(left, right)
    }

}

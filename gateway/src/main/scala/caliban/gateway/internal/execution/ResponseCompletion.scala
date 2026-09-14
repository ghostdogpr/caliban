package caliban.gateway.internal.execution

import caliban.{ CalibanError, PathValue, ResponseValue }
import caliban.execution.Field
import caliban.gateway.internal.execution.ResponseCompletion._
import caliban.gateway.internal.planning.OperationPlan
import caliban.gateway.internal.planning.OperationPlan.TypenameSelection
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue }

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable

/**
 * Completes fetched values against the client selections, including GraphQL null propagation.
 */
private[gateway] final class ResponseCompletion(
  typenameSelections: List[TypenameSelection],
  fetchedFields: FetchedFields = null
) {
  private[this] val hasFetched = fetchedFields ne null
  private[this] val compiled   = new AtomicReference[List[(List[Field], Array[CompiledField])]](Nil)

  def complete(fields: List[Field], value: ResponseValue, errors: List[CalibanError]): Completion = {
    val walk   = new Walk(ErrorPathIndex(errors))
    val result = walk.completeObject(compiledRoot(fields), value, Nil, null)
    val found  = walk.errors.toList
    if (result eq null) BubbleNull(found) else Completed(result, found)
  }

  private def compiledRoot(fields: List[Field]): Array[CompiledField] =
    compiled.get.find(_._1 eq fields) match {
      case Some((_, root)) => root
      case None            =>
        val root = compileFields(fields, fetchedFields, null, Vector.empty)
        compiled.updateAndGet(current => if (current.exists(_._1 eq fields)) current else (fields, root) :: current)
        root
    }

  private def compileFields(
    fields: List[Field],
    fetched: FetchedFields,
    runtimeType: String,
    responsePath: Vector[String]
  ): Array[CompiledField] = {
    val result    = new Array[CompiledField](fields.length)
    var i         = 0
    var remaining = fields
    while (remaining ne Nil) {
      result(i) = compileField(remaining.head, fetched, runtimeType, responsePath)
      i += 1
      remaining = remaining.tail
    }
    result
  }

  private def compileField(
    field: Field,
    fetched: FetchedFields,
    runtimeType: String,
    responsePath: Vector[String]
  ): CompiledField = {
    val name         = field.aliasedName
    val fieldFetched =
      if ((fetched eq null) || (field.fields.isEmpty && (field._condition.isEmpty || (runtimeType eq null)))) null
      else fetched.child(name)
    val unselected   =
      field._condition.nonEmpty && hasFetched && (runtimeType ne null) && !fieldSelected(
        fieldFetched,
        field,
        runtimeType
      )
    new CompiledField(
      field,
      name,
      PathValue.Key(name),
      unselected,
      compileType(field.fieldType, field, fieldFetched, responsePath :+ name)
    )
  }

  private def compileType(
    fieldType: __Type,
    field: Field,
    fetched: FetchedFields,
    responsePath: Vector[String]
  ): CompiledType =
    fieldType.kind match {
      case __TypeKind.NON_NULL                     =>
        new NonNullType(fieldType.ofType.map(compileType(_, field, fetched, responsePath)).orNull)
      case __TypeKind.LIST                         =>
        new ListType(fieldType.ofType.map(compileType(_, field, fetched, responsePath)).orNull)
      case __TypeKind.INTERFACE | __TypeKind.UNION => compileAbstract(fieldType, field, fetched, responsePath)
      case __TypeKind.OBJECT                       =>
        val typeName = fieldType.name.getOrElse("")
        new ObjectType(compileFields(field.collectFields(typeName), fetched, typeName, responsePath))
      case __TypeKind.ENUM                         =>
        new EnumType(fieldType.allEnumValues.map(_.name).toSet, fieldType.name.getOrElse("Unknown"))
      case __TypeKind.SCALAR                       =>
        fieldType.name match {
          case Some("String") | Some("ID") => StringScalar
          case Some("Int")                 => IntScalar
          case Some("Float")               => FloatScalar
          case Some("Boolean")             => BooleanScalar
          case _                           => PassThroughType
        }
      case _                                       => PassThroughType
    }

  private def compileAbstract(
    fieldType: __Type,
    field: Field,
    fetched: FetchedFields,
    responsePath: Vector[String]
  ): AbstractType = {
    val (matching, fallback) = typenameSelections.partition(_.path == responsePath)
    new AbstractType(
      fieldType.possibleTypes.getOrElse(Nil).flatMap(_.name).toSet,
      matching.map(_.responseName) :::
        field.fields.iterator.filter(_.name == "__typename").map(_.aliasedName).toList :::
        "__typename" :: fallback.map(_.responseName),
      field.fields.exists(child => child.name == "__typename" || child._condition.nonEmpty || child.targets.nonEmpty),
      fieldType.name.getOrElse(""),
      typeName => compileFields(field.collectFields(typeName), fetched, typeName, responsePath)
    )
  }

  private def fieldSelected(node: FetchedFields, field: Field, typeName: String): Boolean = {
    if (node eq null) return false
    var remaining = node.fields
    while (remaining ne Nil) {
      val selected = remaining.head
      if (selected.name == field.name && selected._condition.forall(_.contains(typeName))) return true
      remaining = remaining.tail
    }
    false
  }

  private final class Walk(sourceErrors: ErrorPathIndex) {
    val errors = new mutable.ListBuffer[CalibanError.ExecutionError]

    def completeObject(
      fields: Array[CompiledField],
      value: ResponseValue,
      path: List[PathValue],
      indexed: IndexedFields
    ): ResponseValue =
      value match {
        case obj: ObjectValue =>
          val lookup    = if (indexed eq null) IndexedFields(obj) else indexed
          val completed = new mutable.ListBuffer[(String, ResponseValue)]
          var missing   = false
          var i         = 0
          while (i < fields.length) {
            val field  = fields(i)
            val found  = if (field.unselected) NullValue else lookup.getOrNull(field.name)
            val result =
              if (found ne null) completeValue(field.tpe, field, found, path, field.key)
              else {
                val fieldPath = field.key :: path
                val before    = errors.length
                invalid(fieldPath)
                if (field.tpe.isInstanceOf[NonNullType]) bubble(field, fieldPath, errors.length > before) else NullValue
              }
            if (result eq null) missing = true else completed += ((field.name, result))
            i += 1
          }
          if (missing) null else ObjectValue(completed.toList)
        case _                => invalid(path)
      }

    private def completeValue(
      tpe: CompiledType,
      field: CompiledField,
      value: ResponseValue,
      parentPath: List[PathValue],
      segment: PathValue
    ): ResponseValue =
      tpe match {
        case t: NonNullType          =>
          val before    = errors.length
          val completed = if (t.inner eq null) NullValue else completeValue(t.inner, field, value, parentPath, segment)
          if (completed eq NullValue) bubble(field, segment :: parentPath, errors.length > before) else completed
        case _ if value == NullValue => NullValue
        case t: ListType             =>
          value match {
            case ListValue(values) if t.item ne null =>
              val itemPath  = segment :: parentPath
              val completed = new mutable.ListBuffer[ResponseValue]
              var missing   = false
              var index     = 0
              var remaining = values
              while (remaining ne Nil) {
                val result = completeValue(t.item, field, remaining.head, itemPath, PathValue.Index(index))
                if (result eq null) missing = true else completed += result
                index += 1
                remaining = remaining.tail
              }
              if (missing) NullValue else ListValue(completed.toList)
            case _                                   => invalid(segment :: parentPath)
          }
        case t: AbstractType         => completeAbstract(t, value, segment :: parentPath)
        case t: ObjectType           => completeNested(t.fields, value, segment :: parentPath, null)
        case t: EnumType             => if (t.valid(value)) value else invalidEnum(t, field, segment :: parentPath)
        case t: ScalarType           => if (t.valid(value)) value else invalid(segment :: parentPath)
        case PassThroughType         => value
      }

    private def completeNested(
      fields: Array[CompiledField],
      value: ResponseValue,
      path: List[PathValue],
      indexed: IndexedFields
    ): ResponseValue = {
      val completed = completeObject(fields, value, path, indexed)
      if (completed eq null) NullValue else completed
    }

    private def completeAbstract(tpe: AbstractType, value: ResponseValue, path: List[PathValue]): ResponseValue = {
      val indexed = value match {
        case obj: ObjectValue => IndexedFields(obj)
        case _                => null
      }
      val runtime = runtimeType(indexed, tpe.runtimeTypes)
      if ((runtime ne null) && (tpe.possible.isEmpty || tpe.possible.contains(runtime)))
        completeNested(tpe.fields(runtime), value, path, indexed)
      else if ((runtime eq null) && !tpe.requiresRuntime)
        completeNested(tpe.fields(tpe.defaultType), value, path, indexed)
      else invalid(path)
    }

    private def bubble(field: CompiledField, path: List[PathValue], hasCompletedErrors: Boolean): ResponseValue = {
      val reversed = path.reverse
      if (!hasCompletedErrors && !sourceErrors.overlaps(reversed)) {
        val parent = field.source.parentType.flatMap(_.name).getOrElse("Unknown")
        errors += CalibanError.ExecutionError(
          s"Cannot return null for non-nullable field $parent.${field.source.name}.",
          reversed,
          Some(field.source.locationInfo)
        )
      }
      null
    }

    private def invalidEnum(tpe: EnumType, field: CompiledField, path: List[PathValue]): ResponseValue = {
      val reversed = path.reverse
      if (!sourceErrors.overlaps(reversed))
        errors += CalibanError.ExecutionError(
          s"Invalid value for enum '${tpe.name}'.",
          reversed,
          Some(field.source.locationInfo)
        )
      NullValue
    }

    private def invalid(path: List[PathValue]): ResponseValue = {
      val reversed = path.reverse
      if (!(reversed.isEmpty && sourceErrors.nonEmpty) && !sourceErrors.overlaps(reversed))
        errors += RemoteError.at(reversed)
      NullValue
    }

    private def runtimeType(value: IndexedFields, runtimeTypes: List[String]): String = {
      if (value eq null) return null
      var remaining = runtimeTypes
      while (remaining ne Nil) {
        value.getOrNull(remaining.head) match {
          case StringValue(name) => return name
          case _                 => ()
        }
        remaining = remaining.tail
      }
      null
    }
  }
}

private[gateway] object ResponseCompletion {
  def forPlan(plan: OperationPlan): ResponseCompletion = {
    // A valid plan can omit conditional fields outside the common runtime types of a shareable path.
    // Retain fetched selections so those omissions do not look like malformed upstream responses.
    val root                                                    = new FetchedFields
    var nonEmpty                                                = false
    def collect(fields: List[Field], node: FetchedFields): Unit =
      fields.foreach { field =>
        val child = node.childOrCreate(field.aliasedName)
        child.fields = field :: child.fields
        nonEmpty = true
        collect(field.fields, child)
      }
    collect(plan.localFields ::: plan.roots.flatMap(_.selections), root)
    plan.entities.foreach { fetch =>
      var node = root
      fetch.mergePath.foreach(name => node = node.childOrCreate(name))
      collect(fetch.fields, node)
    }
    new ResponseCompletion(plan.typenameSelections, if (nonEmpty) root else null)
  }

  private[execution] final class FetchedFields {
    private val children    = new java.util.HashMap[String, FetchedFields](8)
    var fields: List[Field] = Nil

    def child(name: String): FetchedFields = children.get(name)

    def childOrCreate(name: String): FetchedFields = {
      var node = children.get(name)
      if (node eq null) {
        node = new FetchedFields
        children.put(name, node)
      }
      node
    }
  }

  private final class CompiledField(
    val source: Field,
    val name: String,
    val key: PathValue,
    val unselected: Boolean,
    val tpe: CompiledType
  )

  private sealed abstract class CompiledType

  private final class NonNullType(val inner: CompiledType) extends CompiledType

  private final class ListType(val item: CompiledType) extends CompiledType

  private final class ObjectType(val fields: Array[CompiledField]) extends CompiledType

  private final class AbstractType(
    val possible: Set[String],
    val runtimeTypes: List[String],
    val requiresRuntime: Boolean,
    val defaultType: String,
    compile: String => Array[CompiledField]
  ) extends CompiledType {
    private val byType = new ConcurrentHashMap[String, Array[CompiledField]]

    def fields(typeName: String): Array[CompiledField] =
      if (possible.contains(typeName) || typeName == defaultType) byType.computeIfAbsent(typeName, compile(_))
      else compile(typeName)
  }

  private final class EnumType(values: Set[String], val name: String) extends CompiledType {
    def valid(value: ResponseValue): Boolean =
      value match {
        case StringValue(found) => values.contains(found)
        case EnumValue(found)   => values.contains(found)
        case _                  => false
      }
  }

  private sealed abstract class ScalarType extends CompiledType {
    def valid(value: ResponseValue): Boolean
  }

  private case object StringScalar extends ScalarType {
    def valid(value: ResponseValue): Boolean = value.isInstanceOf[StringValue]
  }

  private case object IntScalar extends ScalarType {
    def valid(value: ResponseValue): Boolean =
      value match {
        case _: IntValue.IntNumber         => true
        case IntValue.LongNumber(number)   => number.isValidInt
        case IntValue.BigIntNumber(number) => number.isValidInt
        case _                             => false
      }
  }

  private case object FloatScalar extends ScalarType {
    def valid(value: ResponseValue): Boolean = value.isInstanceOf[IntValue] || value.isInstanceOf[FloatValue]
  }

  private case object BooleanScalar extends ScalarType {
    def valid(value: ResponseValue): Boolean = value.isInstanceOf[BooleanValue]
  }

  private case object PassThroughType extends CompiledType

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
  private[internal] final case class ResponseNameMapping(
    clientName: String,
    children: Map[String, ResponseNameMapping]
  )

  def responseNameRestorer(
    clientFields: List[Field],
    executableFields: List[Field]
  ): Option[Map[String, ResponseNameMapping]] = {
    val mappings = responseNameMappings(clientFields, executableFields)

    def isIdentity(values: Map[String, ResponseNameMapping]): Boolean =
      values.forall { case (name, mapping) => mapping.clientName == name && isIdentity(mapping.children) }

    if (isIdentity(mappings)) None else Some(mappings)
  }

  def restoreResponseNames(
    mappings: Map[String, ResponseNameMapping],
    value: ResponseValue
  ): ResponseValue =
    value match {
      case ObjectValue(fields) =>
        val restored = mutable.LinkedHashMap.empty[String, ResponseValue]
        fields.foreach { case (name, nested) =>
          val (clientName, clientValue) = mappings.get(name) match {
            case Some(mapping) => mapping.clientName -> restoreResponseNames(mapping.children, nested)
            case None          => name               -> nested
          }
          restored.update(
            clientName,
            restored.get(clientName).fold(clientValue)(mergeRootValue(_, clientValue))
          )
        }
        ObjectValue(restored.toList)
      case ListValue(values)   => ListValue(values.map(restoreResponseNames(mappings, _)))
      case other               => other
    }

  private def responseNameMappings(
    clientFields: List[Field],
    executableFields: List[Field]
  ): Map[String, ResponseNameMapping] =
    executableFields
      .zip(clientFields)
      .groupBy(_._1.aliasedName)
      .map { case (responseName, matches) =>
        val executable = matches.iterator.map(_._1).reduce(_.combine(_))
        val client     = matches.iterator.map(_._2).reduce(_.combine(_))
        responseName -> ResponseNameMapping(
          client.aliasedName,
          responseNameMappings(client.fields, executable.fields)
        )
      }

  def restoreResponsePath(
    clientFields: List[Field],
    executableFields: List[Field],
    path: List[PathValue]
  ): List[PathValue] =
    path match {
      case PathValue.Key(name) :: tail    =>
        executableFields.zip(clientFields).find(_._1.aliasedName == name) match {
          case Some((executable, client)) =>
            PathValue.Key(client.aliasedName) :: restoreResponsePath(client.fields, executable.fields, tail)
          case None                       => path
        }
      case PathValue.Index(index) :: tail =>
        PathValue.Index(index) :: restoreResponsePath(clientFields, executableFields, tail)
      case _ :: _                         => path
      case Nil                            => Nil
    }

  def applyPatches(
    value: ResponseValue,
    patches: List[(List[PathValue], ResponseValue)]
  ): ResponseValue =
    patches match {
      case Nil                  => value
      case (path, patch) :: Nil => mergeAt(value, path, patch)
      case _                    =>
        val root = new PatchNode
        patches.foreach { case (path, patch) => root.add(path, patch) }
        root.patch(value)
    }

  private sealed trait PatchEntry

  private final case class PatchValue(value: ResponseValue) extends PatchEntry

  private final class PatchNode {
    private[this] var entries: List[PatchEntry] = Nil

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
    private[this] var keys: java.util.HashMap[String, PatchNode] = null
    private[this] var indices: mutable.LongMap[PatchNode]        = null

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

package caliban.gateway.internal.execution

import caliban.{ CalibanError, PathValue, ResponseValue }
import caliban.execution.Field
import caliban.gateway.TypenameField
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
  def complete(fields: List[Field], value: ResponseValue, errors: List[CalibanError]): Completion = {
    val completer        = new Completer(errorPaths(errors), hasSourceErrors = errors.nonEmpty)
    val result           = completer.completeObject(rootFields(fields), value, Nil, null)
    val completionErrors = completer.errors.toList
    if (result eq null) BubbleNull(completionErrors) else Completed(result, completionErrors)
  }

  private val hasFetchedFields = fetchedFields ne null
  private val compiledRoots    = new AtomicReference[List[(List[Field], Array[CompiledField])]](Nil)

  // Cached plans reuse the same selection lists, so root lookup intentionally uses reference identity.
  private def rootFields(fields: List[Field]): Array[CompiledField] = {
    val cached = compiledRoot(compiledRoots.get, fields)
    if (cached ne null) cached
    else {
      val root = compileFields(fields, fetchedFields, null, Vector.empty)
      compiledRoots.updateAndGet(current =>
        if (compiledRoot(current, fields) ne null) current else (fields, root) :: current
      )
      root
    }
  }

  /**
   * Returns null when `fields` has not been compiled yet.
   */
  private def compiledRoot(
    roots: List[(List[Field], Array[CompiledField])],
    fields: List[Field]
  ): Array[CompiledField] = {
    var remaining = roots
    while (remaining ne Nil) {
      val root = remaining.head
      if (root._1 eq fields) return root._2
      remaining = remaining.tail
    }
    null
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
    val name          = field.aliasedName
    val isConditional = field._condition.nonEmpty && (runtimeType ne null)
    val fetchedChild  = if ((fetched eq null) || (field.fields.isEmpty && !isConditional)) null else fetched.child(name)
    val unfetched     = isConditional && hasFetchedFields && !wasFetched(fetchedChild, field, runtimeType)
    new CompiledField(
      field,
      name,
      PathValue.Key(name),
      unfetched,
      compileType(field.fieldType, field, fetchedChild, responsePath :+ name)
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
        field.fields.iterator.filter(_.name == TypenameField).map(_.aliasedName).toList :::
        TypenameField :: fallback.map(_.responseName),
      field.fields.exists(child => child.name == TypenameField || child._condition.nonEmpty || child.targets.nonEmpty),
      fieldType.name.getOrElse(""),
      typeName => compileFields(field.collectFields(typeName), fetched, typeName, responsePath)
    )
  }

  private def wasFetched(node: FetchedFields, field: Field, typeName: String): Boolean = {
    if (node eq null) return false
    var remaining = node.fields
    while (remaining ne Nil) {
      val selected = remaining.head
      if (selected.name == field.name && selected._condition.forall(_.contains(typeName))) return true
      remaining = remaining.tail
    }
    false
  }

  /**
   * Paths are accumulated in reverse order. A Scala null signals a non-null violation that must bubble;
   * NullValue is a completed GraphQL null at a nullable boundary.
   */
  private final class Completer(sourceErrorPaths: PathIndex, hasSourceErrors: Boolean) {
    val errors = new mutable.ListBuffer[CalibanError.ExecutionError]

    def completeObject(
      fields: Array[CompiledField],
      value: ResponseValue,
      path: List[PathValue],
      indexed: IndexedFields
    ): ResponseValue =
      value match {
        case obj: ObjectValue =>
          val values    = if (indexed eq null) IndexedFields(obj) else indexed
          val completed = new mutable.ListBuffer[(String, ResponseValue)]
          var missing   = false
          var i         = 0
          while (i < fields.length) {
            val field  = fields(i)
            val result = completeField(field, values, path)
            if (result eq null) missing = true else completed += ((field.name, result))
            i += 1
          }
          if (missing) null else ObjectValue(completed.toList)
        case _                => invalid(path)
      }

    private def completeField(field: CompiledField, values: IndexedFields, path: List[PathValue]): ResponseValue = {
      val found = if (field.unfetched) NullValue else values.getOrNull(field.name)
      if (found ne null) completeValue(field.tpe, field, found, path, field.key)
      else {
        val fieldPath = field.key :: path
        val before    = errors.length
        invalid(fieldPath)
        if (field.tpe.isInstanceOf[NonNullType]) bubble(field, fieldPath, alreadyReported = errors.length > before)
        else NullValue
      }
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
          if (completed eq NullValue) bubble(field, segment :: parentPath, alreadyReported = errors.length > before)
          else completed
        case _ if value eq NullValue => NullValue
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
      val runtime = runtimeType(indexed, tpe.typenameFields)
      if ((runtime ne null) && tpe.isPossibleType(runtime)) completeNested(tpe.fields(runtime), value, path, indexed)
      else if ((runtime eq null) && !tpe.requiresTypename)
        completeNested(tpe.fields(tpe.declaredType), value, path, indexed)
      else invalid(path)
    }

    private def bubble(field: CompiledField, path: List[PathValue], alreadyReported: Boolean): ResponseValue = {
      val reversed = path.reverse
      if (!alreadyReported && !sourceErrorPaths.overlaps(reversed)) {
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
      if (!sourceErrorPaths.overlaps(reversed))
        errors += CalibanError.ExecutionError(
          s"Invalid value for enum '${tpe.name}'.",
          reversed,
          Some(field.source.locationInfo)
        )
      NullValue
    }

    private def invalid(path: List[PathValue]): ResponseValue = {
      val reversed = path.reverse
      if (!(reversed.isEmpty && hasSourceErrors) && !sourceErrorPaths.overlaps(reversed))
        errors += RemoteError.at(reversed)
      NullValue
    }

    private def runtimeType(value: IndexedFields, typenameFields: List[String]): String = {
      if (value eq null) return null
      var remaining = typenameFields
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
    val unfetched: Boolean,
    val tpe: CompiledType
  )

  private sealed abstract class CompiledType

  private final class NonNullType(val inner: CompiledType) extends CompiledType

  private final class ListType(val item: CompiledType) extends CompiledType

  private final class ObjectType(val fields: Array[CompiledField]) extends CompiledType

  private final class AbstractType(
    val possibleTypes: Set[String],
    val typenameFields: List[String],
    val requiresTypename: Boolean,
    val declaredType: String,
    compile: java.util.function.Function[String, Array[CompiledField]]
  ) extends CompiledType {
    private val byType = new ConcurrentHashMap[String, Array[CompiledField]]

    def isPossibleType(typeName: String): Boolean = possibleTypes.isEmpty || possibleTypes.contains(typeName)

    def fields(typeName: String): Array[CompiledField] =
      if (possibleTypes.contains(typeName) || typeName == declaredType) byType.computeIfAbsent(typeName, compile)
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

  private val NoErrorPaths = PathIndex(Iterator.empty)

  private def errorPaths(errors: List[CalibanError]): PathIndex =
    if (errors.isEmpty) NoErrorPaths
    else
      PathIndex(errors.iterator.collect {
        case error: CalibanError.ExecutionError if error.path.nonEmpty => error.path
      })

}

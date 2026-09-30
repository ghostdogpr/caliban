package caliban.gateway.internal.execution

import caliban.{ CalibanError, PathValue, ResponseValue }
import caliban.execution.{ isMetaField, Field }
import caliban.gateway.TypenameField
import caliban.gateway.internal.execution.ResponseCompletion._
import caliban.gateway.internal.planning.OperationPlan
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue }

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

/**
 * Completes fetched values against the client selections, including GraphQL null propagation.
 */
private[gateway] final class ResponseCompletion private (fields: Array[CompiledField]) {
  lazy val names: List[String] = fields.iterator.map(_.name).toList

  def only(names: Set[String]): ResponseCompletion = new ResponseCompletion(fields.filter(field => names(field.name)))

  def complete(value: ResponseValue, errors: List[CalibanError]): Completion =
    value match {
      case obj: ObjectValue =>
        val completer        = new Completer(errorPaths(errors))
        val result           = completer.completeObject(fields, IndexedFields(obj), Nil)
        val completionErrors = completer.errors.toList
        if (result eq null) BubbleNull(completionErrors) else Completed(result, completionErrors)
      case _                => Completed(NullValue, if (errors.isEmpty) List(RemoteError.at(Nil)) else Nil)
    }

  /**
   * Paths are accumulated in reverse order. A Scala null signals a non-null violation that must bubble;
   * NullValue is a completed GraphQL null at a nullable boundary.
   */
  private final class Completer(sourceErrorPaths: PathIndex) {
    val errors = new mutable.ListBuffer[CalibanError.ExecutionError]

    def completeObject(fields: Array[CompiledField], values: IndexedFields, path: List[PathValue]): ResponseValue = {
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
    }

    private def completeField(field: CompiledField, values: IndexedFields, path: List[PathValue]): ResponseValue = {
      val found = if (field.unfetched) NullValue else values.getOrNull(field.name)
      if (found ne null) completeValue(field.tpe, field, found, path, field.key)
      else {
        invalid(field.key :: path)
        if (field.tpe.isInstanceOf[NonNullType]) null else NullValue
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
          val completed = completeValue(t.inner, field, value, parentPath, segment)
          if (completed ne NullValue) completed
          else if (value eq NullValue) bubble(field, segment :: parentPath)
          else null
        case _ if value eq NullValue => NullValue
        case t: ListType             =>
          value match {
            case ListValue(values) =>
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
            case _                 => invalid(segment :: parentPath)
          }
        case t: AbstractType         => completeAbstract(t, value, segment :: parentPath)
        case t: ObjectType           =>
          value match {
            case obj: ObjectValue => completeNested(t.fields, IndexedFields(obj), segment :: parentPath)
            case _                => invalid(segment :: parentPath)
          }
        case t: EnumType             => if (t.valid(value)) value else invalidEnum(t, field, segment :: parentPath)
        case t: ScalarType           => if (t.valid(value)) value else invalid(segment :: parentPath)
        case PassThroughType         => value
      }

    private def completeNested(
      fields: Array[CompiledField],
      values: IndexedFields,
      path: List[PathValue]
    ): ResponseValue = {
      val completed = completeObject(fields, values, path)
      if (completed eq null) NullValue else completed
    }

    private def completeAbstract(tpe: AbstractType, value: ResponseValue, path: List[PathValue]): ResponseValue =
      value match {
        case obj: ObjectValue =>
          val indexed = IndexedFields(obj)
          val runtime = runtimeType(indexed, tpe.typenameFields)
          if ((runtime ne null) && tpe.isPossibleType(runtime)) completeNested(tpe.fields(runtime), indexed, path)
          else if ((runtime eq null) && !tpe.requiresTypename)
            completeNested(tpe.fields(tpe.declaredType), indexed, path)
          else invalid(path)
        case _                => invalid(path)
      }

    private def bubble(field: CompiledField, path: List[PathValue]): ResponseValue = {
      val parent = field.source.parentType.flatMap(_.name).getOrElse("Unknown")
      report(path)(
        CalibanError.ExecutionError(
          s"Cannot return null for non-nullable field $parent.${field.source.name}.",
          _,
          Some(field.source.locationInfo)
        )
      )
      null
    }

    private def invalidEnum(tpe: EnumType, field: CompiledField, path: List[PathValue]): ResponseValue = {
      report(path)(
        CalibanError.ExecutionError(s"Invalid value for enum '${tpe.name}'.", _, Some(field.source.locationInfo))
      )
      NullValue
    }

    private def invalid(path: List[PathValue]): ResponseValue = {
      report(path)(RemoteError.at)
      NullValue
    }

    private def report(path: List[PathValue])(error: List[PathValue] => CalibanError.ExecutionError): Unit = {
      val reversed = path.reverse
      if (!sourceErrorPaths.overlaps(reversed)) errors += error(reversed)
    }

    private def runtimeType(value: IndexedFields, typenameFields: List[String]): String = {
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
  private def compileFields(fields: List[Field], fetched: FetchedFields, runtimeType: String): Array[CompiledField] =
    fields.map(compileField(_, fetched, runtimeType)).toArray

  private def compileField(field: Field, fetched: FetchedFields, runtimeType: String): CompiledField = {
    val name          = field.aliasedName
    val isConditional = field._condition.nonEmpty && (runtimeType ne null)
    val fetchedChild  = fetched.child(name)
    val unfetched     = isConditional && !wasFetched(fetchedChild, field, runtimeType)
    new CompiledField(
      field,
      name,
      PathValue.Key(name),
      unfetched,
      compileType(field.fieldType, field, fetchedChild)
    )
  }

  private def compileType(fieldType: __Type, field: Field, fetched: FetchedFields): CompiledType =
    fieldType.kind match {
      case __TypeKind.NON_NULL                     =>
        new NonNullType(fieldType.ofType.fold[CompiledType](PassThroughType)(compileType(_, field, fetched)))
      case __TypeKind.LIST                         =>
        new ListType(fieldType.ofType.fold[CompiledType](PassThroughType)(compileType(_, field, fetched)))
      case __TypeKind.INTERFACE | __TypeKind.UNION => compileAbstract(fieldType, field, fetched)
      case __TypeKind.OBJECT                       =>
        val typeName = fieldType.name.getOrElse("")
        new ObjectType(compileFields(field.collectFields(typeName), fetched, typeName))
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

  private def compileAbstract(fieldType: __Type, field: Field, fetched: FetchedFields): AbstractType =
    new AbstractType(
      fieldType.possibleTypes.getOrElse(Nil).flatMap(_.name).toSet,
      fetched.typenames :::
        field.fields.iterator.filter(_.name == TypenameField).map(_.aliasedName).toList :::
        TypenameField :: Nil,
      field.fields.exists(child => child.name == TypenameField || child._condition.nonEmpty || child.targets.nonEmpty),
      fieldType.name.getOrElse(""),
      typeName => compileFields(field.collectFields(typeName), fetched, typeName)
    )

  private def wasFetched(node: FetchedFields, field: Field, typeName: String): Boolean =
    node.fields.exists(selected => selected.name == field.name && selected._condition.forall(_.contains(typeName)))

  def forPlan(plan: OperationPlan): ResponseCompletion = {
    // A valid plan can omit conditional fields outside the common runtime types of a shareable path.
    // Retain fetched selections so those omissions do not look like malformed upstream responses.
    val root                                                    = new FetchedFields
    def collect(fields: List[Field], node: FetchedFields): Unit =
      fields.foreach { field =>
        val child = node.childOrCreate(field.aliasedName)
        child.fields = field :: child.fields
        collect(field.fields, child)
      }
    collect(plan.roots.flatMap(_.downstream), root)
    plan.entities.foreach { fetch =>
      var node = root
      fetch.mergePath.foreach(name => node = node.childOrCreate(name))
      collect(fetch.fields, node)
    }
    plan.typenameSelections.foreach { selection =>
      var node = root
      selection.path.foreach(name => node = node.childOrCreate(name))
      node.typenames = node.typenames :+ selection.responseName
    }
    new ResponseCompletion(compileFields(plan.fields.filterNot(isMetaField), root, null))
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

  private val MissingFields = new FetchedFields

  private[execution] final class FetchedFields {
    private val children        = new java.util.HashMap[String, FetchedFields](8)
    var fields: List[Field]     = Nil
    var typenames: List[String] = Nil

    def child(name: String): FetchedFields = children.getOrDefault(name, MissingFields)

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

  private def errorPaths(errors: List[CalibanError]): PathIndex =
    PathIndex(errors.iterator.collect { case error: CalibanError.ExecutionError if error.path.nonEmpty => error.path })

}

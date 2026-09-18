package caliban.gateway.internal.execution

import caliban.{ CalibanError, PathValue }
import caliban.ResponseValue.ObjectValue
import caliban.Value.NullValue
import caliban.execution.Field
import caliban.introspection.adt.__Type
import caliban.schema.Types

/**
 * Sanitizes upstream errors and limits their paths to fields selected by the client.
 */
private[gateway] object RemoteError {

  def at(path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError(CalibanError.RemoteErrorMessage, path = path)

  def nullObject(fields: List[Field]): ObjectValue =
    ObjectValue(fields.map(field => field.aliasedName -> NullValue))

  def forFields(fields: List[Field]): List[CalibanError.ExecutionError] =
    fields.map(field => at(List(PathValue.Key(field.aliasedName))))

  def sanitize(error: CalibanError, remoteErrorMessages: Boolean): CalibanError.ExecutionError =
    error match {
      case value: CalibanError.ExecutionError =>
        val extensions = value.extensions.flatMap { current =>
          val retained = current.fields.filter { case (name, _) => name == "code" }
          if (retained.isEmpty) None else Some(ObjectValue(retained))
        }
        value.copy(
          msg = if (remoteErrorMessages) value.msg else CalibanError.RemoteErrorMessage,
          locationInfo = None,
          innerThrowable = None,
          extensions = extensions
        )
      case _                                  => at(Nil)
    }

  def hasClientPath(fields: List[Field], path: List[PathValue]): Boolean =
    path match {
      case PathValue.Key(name) :: tail =>
        fields.find(_.aliasedName == name).exists(field => hasClientSubpath(field, tail, field.fieldType))
      case _                           => false
    }

  private def hasClientSubpath(field: Field, path: List[PathValue], tpe: __Type): Boolean =
    path match {
      case Nil                                          => true
      case PathValue.Index(index) :: tail if index >= 0 =>
        Types.listOf(tpe).exists(itemType => hasClientSubpath(field, tail, itemType))
      case PathValue.Key(name) :: tail                  =>
        Types.listOf(tpe).isEmpty &&
        field.fields.find(_.aliasedName == name).exists(child => hasClientSubpath(child, tail, child.fieldType))
      case _                                            => false
    }
}

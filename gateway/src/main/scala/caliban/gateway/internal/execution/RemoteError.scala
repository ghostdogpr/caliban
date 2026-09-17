package caliban.gateway.internal.execution

import caliban.{ CalibanError, PathValue }
import caliban.ResponseValue.ObjectValue
import caliban.execution.Field
import caliban.schema.Types

/**
 * Sanitizes upstream errors and limits their paths to fields selected by the client.
 */
private[gateway] object RemoteError {

  def at(path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError(Message, path = path)

  def nullObject(fields: List[Field]): ObjectValue =
    ObjectValue(fields.map(field => field.aliasedName -> caliban.Value.NullValue))

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
          msg = if (remoteErrorMessages) value.msg else Message,
          locationInfo = None,
          innerThrowable = None,
          extensions = extensions
        )
      case _                                  => at(Nil)
    }

  def hasClientPath(fields: List[Field], path: List[PathValue]): Boolean =
    path match {
      case PathValue.Key(name) :: tail =>
        fields.find(_.aliasedName == name).exists(field => hasClientSubpath(field, tail))
      case _                           => false
    }

  private val Message = "Remote GraphQL request failed."

  private def hasClientSubpath(field: Field, path: List[PathValue]): Boolean = {
    def loop(current: Field, remaining: List[PathValue], currentType: caliban.introspection.adt.__Type): Boolean =
      remaining match {
        case Nil                                          => true
        case PathValue.Index(index) :: tail if index >= 0 =>
          Types.listOf(currentType).exists(itemType => loop(current, tail, itemType))
        case PathValue.Key(name) :: tail                  =>
          if (Types.listOf(currentType).nonEmpty) false
          else
            current.fields
              .find(_.aliasedName == name)
              .exists(child => loop(child, tail, child.fieldType))
        case _                                            => false
      }

    loop(field, path, field.fieldType)
  }
}

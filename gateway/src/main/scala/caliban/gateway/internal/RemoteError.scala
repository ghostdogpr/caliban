package caliban.gateway.internal

import caliban.execution.Field
import caliban.schema.Types
import caliban.{ CalibanError, PathValue }

private[gateway] object RemoteError {

  def at(path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError("Remote GraphQL request failed.", path = path)

  def hasClientPath(fields: List[Field], path: List[PathValue]): Boolean =
    path match {
      case PathValue.Key(name) :: tail =>
        fields.find(_.aliasedName == name).exists(field => hasClientSubpath(field, tail))
      case _                           => false
    }

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

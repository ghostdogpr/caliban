package caliban.parsing

import caliban.GraphQLRequest
import caliban.introspection.adt._
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt._
import caliban.schema.RootType
import caliban.{ InputValue, Value }
import zio.{ IO, UIO }

object VariablesUpdater {
  def updateVariables(
    req: GraphQLRequest,
    doc: Document,
    rootType: RootType
  ): GraphQLRequest = {
    val variableDefinitions = doc.operationDefinitions.flatMap(_.variableDefinitions)
    val updated             = req.variables.getOrElse(Map.empty).map { case (key, value) =>
      val v = variableDefinitions.find(_.name == key).map(resolveEnumValues(value, _, rootType)).getOrElse(value)
      key -> v
    }

    req.copy(variables = Some(updated))
  }

  // Since we cannot separate a String from an Enum when variables
  // are parsed, we need to translate from strings to enums here
  // if we have a valid enum field.
  private def resolveEnumValues(
    value: InputValue,
    definition: VariableDefinition,
    rootType: RootType
  ): InputValue = {
    val t = Type
      .innerType(definition.variableType)

    rootType.types
      .get(t)
      .map(_.kind)
      .flatMap { kind =>
        (kind, value) match {
          case (__TypeKind.ENUM, InputValue.ListValue(v)) =>
            Some(
              InputValue.ListValue(v.map(resolveEnumValues(_, definition, rootType)))
            )
          case (__TypeKind.ENUM, Value.StringValue(v))    =>
            Some(Value.EnumValue(v))
          case _                                          => None
        }
      }
      .getOrElse(value)
  }
}

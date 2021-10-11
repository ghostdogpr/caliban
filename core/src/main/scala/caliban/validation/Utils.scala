package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.InputValue.VariableValue
import caliban.Value.NullValue
import caliban.execution.{ ExecutionRequest, Field => F }
import caliban.introspection.Introspector
import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind._
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.{ TypeSystemDefinition, TypeSystemExtension }
import caliban.parsing.adt.OperationType._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt._
import caliban.schema.{ RootSchema, RootSchemaBuilder, RootType, Types }
import caliban.{ InputValue, Rendering, Value }
import zio.IO

object Utils {
  def isObjectType(t: __Type): Boolean =
    t.kind match {
      case OBJECT => true
      case _      => false
    }

  def isNonNull(t: __Type): Boolean = t.kind == __TypeKind.NON_NULL

  def isListType(t: __Type): Boolean = t.kind == __TypeKind.LIST

  def getFields(t: __Type)                                                = t.fields(__DeprecatedArgs(Some(true)))
  def getType(t: Option[NamedType], parentType: __Type, context: Context) =
    t.fold(Option(parentType))(t => context.rootType.types.get(t.name)).getOrElse(parentType)

  def getType(t: NamedType, context: Context) =
    context.rootType.types.get(t.name)
}

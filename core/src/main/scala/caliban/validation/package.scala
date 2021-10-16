package caliban

import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.introspection.adt.{ __Field, __Type }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.{ Document, Selection, VariableDefinition }
import caliban.parsing.adt.Selection.Field
import caliban.schema.{ RootType, Types }

package object validation {
  case class SelectedField(
    parentType: __Type,
    selection: Field,
    fieldDef: __Field
  )

  type FieldMap = Map[String, Set[SelectedField]]

  case class Context(
    document: Document,
    rootType: RootType,
    operations: List[OperationDefinition],
    fragments: Map[String, FragmentDefinition],
    selectionSets: List[Selection]
  ) {
    lazy val variableDefinitions: Map[String, VariableDefinition] =
      operations.flatMap(_.variableDefinitions.map(d => d.name -> d)).toMap
  }

  object Context {
    val empty: Context =
      Context(Document(Nil, SourceMapper.empty), RootType(Types.boolean, None, None), Nil, Map.empty, Nil)
  }
}

package caliban

import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.introspection.adt.{ __Field, __Type }
import caliban.parsing.adt.{ Document, Selection }
import caliban.parsing.adt.Selection.Field
import caliban.schema.RootType

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
  )
}

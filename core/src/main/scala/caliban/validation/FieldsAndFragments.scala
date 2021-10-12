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
import Utils._

case class SelectedField(
  parentType: __Type,
  selection: Field,
  fieldDef: __Field
)

case class FieldsAndFragments(
  fields: FieldMap = Map.empty
) { self =>
  def ++(that: FieldsAndFragments): FieldsAndFragments =
    self.copy(
      (fields.keySet ++ that.fields.keySet).map { k =>
        k -> (fields.get(k).getOrElse(Set.empty) ++ that.fields.get(k).getOrElse(Set.empty))
      }.toMap
    )

  def addField(
    f: Field,
    parentType: __Type,
    selection: Field
  ) = {
    val responseName = f.alias.getOrElse(f.name)

    getFields(parentType)
      .flatMap(fields => fields.find(_.name == f.name))
      .map { f =>
        val sf    = SelectedField(parentType, selection, f)
        val entry = fields.get(responseName).map(_ + sf).getOrElse(Set(sf))
        self.copy(fields = self.fields + (responseName -> entry))
      }
      .getOrElse(self)
  }
}

object FieldsAndFragments {
  val empty = FieldsAndFragments(Map.empty)

  def apply(context: Context, parentType: __Type, selectionSet: List[Selection]): FieldsAndFragments =
    selectionSet.foldLeft(FieldsAndFragments.empty)({ case (fieldsAndFragments, selection) =>
      selection match {
        case FragmentSpread(name, directives)               =>
          context.fragments
            .get(name)
            .map { definition =>
              val typ = getType(Some(definition.typeCondition), parentType, context)
              apply(context, typ, definition.selectionSet) ++ fieldsAndFragments
            }
            .getOrElse(fieldsAndFragments)
        case f: Field                                       =>
          fieldsAndFragments.addField(f, parentType, f)
        case InlineFragment(typeCondition, _, selectionSet) =>
          val typ = getType(typeCondition, parentType, context)
          apply(context, typ, selectionSet) ++ fieldsAndFragments
      }
    })
}

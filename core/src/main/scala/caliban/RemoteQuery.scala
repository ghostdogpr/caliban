package caliban

import caliban.execution.Field
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt.{ Document, OperationType, Selection }
import caliban.rendering.DocumentRenderer

import scala.collection.mutable.ListBuffer

case class RemoteQuery(field: Field) { self =>
  def toGraphQLRequest: GraphQLRequest =
    GraphQLRequest(
      query = Some(DocumentRenderer.renderCompact(toDocument))
    )

  def toDocument: Document = RemoteQuery.toDocument(OperationType.Query, field)
}

case class RemoteMutation(field: Field) { self =>
  def toGraphQLRequest: GraphQLRequest =
    GraphQLRequest(query =
      Some(
        DocumentRenderer.renderCompact(toDocument)
      )
    )

  def toDocument: Document = RemoteQuery.toDocument(OperationType.Mutation, field)
}

object RemoteQuery {
  object QueryRenderer {
    def render(r: RemoteMutation): String =
      DocumentRenderer.renderCompact(r.toDocument)
    def render(r: RemoteQuery): String    =
      DocumentRenderer.renderCompact(r.toDocument)
  }

  def toDocument(operationType: OperationType, field: Field): Document =
    document(operationType, field, identity)(selectionSet => List(field.selecting(field.arguments, selectionSet)))

  /**
   * Renders the operation whose root field is `root`, selecting its sub-fields and rendering each field's arguments
   * with `arguments`.
   */
  private[caliban] def operationDocument(
    operationType: OperationType,
    root: Field,
    arguments: Map[String, InputValue] => Map[String, InputValue]
  ): Document =
    document(operationType, root, arguments)(identity)

  /**
   * Renders each field selected from several places once, as a named fragment.
   */
  private def document(
    operationType: OperationType,
    root: Field,
    arguments: Map[String, InputValue] => Map[String, InputValue]
  )(
    selections: List[Selection] => List[Selection]
  ): Document = {
    val seen      = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Field, java.lang.Boolean]())
    val names     = new java.util.IdentityHashMap[Field, String]()
    val fragments = ListBuffer.empty[() => FragmentDefinition]

    def render(field: Field): Selection = {
      val selectionSet = names.get(field) match {
        case null => field.selectionSet(render)
        case name => List(Selection.FragmentSpread(name, Nil))
      }
      field.selecting(arguments(field.arguments), selectionSet)
    }

    def collect(field: Field): Unit =
      field.fields.foreach { child =>
        if (seen.add(child)) collect(child)
        else if (child.fields.nonEmpty && !names.containsKey(child))
          child.fieldType.innerType.name.foreach { typeName =>
            val name = s"F${fragments.length}"
            names.put(child, name)
            fragments += (() =>
              FragmentDefinition(name, NamedType(typeName, nonNull = false), Nil, child.selectionSet(render))
            )
          }
      }

    collect(root)

    val operation = OperationDefinition(operationType, None, Nil, Nil, selections(root.selectionSet(render)))
    Document(operation :: fragments.toList.map(_()), SourceMapper.empty)
  }
}

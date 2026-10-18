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
    document(operationType, field)(render => List(render(field)))

  /**
   * Renders the operation whose root field is `root`, selecting its sub-fields.
   */
  private[caliban] def operationDocument(operationType: OperationType, root: Field): Document =
    document(operationType, root)(root.selectionSet)

  /**
   * Spreads of a fragment share its fields, so a field selected from several places becomes a fragment rendered once,
   * which keeps the document as small as the field graph rather than the number of paths through it.
   */
  private def document(operationType: OperationType, field: Field)(
    selections: (Field => Selection) => List[Selection]
  ): Document = {
    val references = new java.util.IdentityHashMap[Field, Array[Int]]()
    var pending    = List(field)
    references.put(field, Array(0))
    while (pending.nonEmpty) {
      val current = pending.head
      pending = pending.tail
      current.fields.foreach { child =>
        val count = references.get(child)
        if (count eq null) {
          references.put(child, Array(1))
          pending = child :: pending
        } else count(0) += 1
      }
    }

    val fragments = ListBuffer.empty[FragmentDefinition]
    val rendered  = new java.util.IdentityHashMap[Field, Selection]()
    var names     = 0

    def render(field: Field): Selection = {
      val existing = rendered.get(field)
      if (existing ne null) existing
      else {
        val shared    = field.fields.nonEmpty && Option(references.get(field)).exists(_(0) > 1)
        val selection = field.fieldType.innerType.name match {
          case Some(typeName) if shared =>
            val name = s"F$names"
            names += 1
            fragments += FragmentDefinition(name, NamedType(typeName, nonNull = false), Nil, field.selectionSet(render))
            field.selecting(List(Selection.FragmentSpread(name, Nil)))
          case _                        =>
            field.selecting(field.selectionSet(render))
        }
        rendered.put(field, selection)
        selection
      }
    }

    val operation = OperationDefinition(operationType, None, Nil, Nil, selections(render))
    Document(operation :: fragments.toList, SourceMapper.empty)
  }
}

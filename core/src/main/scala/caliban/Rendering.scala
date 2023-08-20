package caliban

import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind._
import caliban.parsing.adt.Directive
import caliban.rendering.DocumentRenderer

@deprecated("Prefer the methods in caliban.rendering.DocumentRenderer instead.", "2.3.1")
object Rendering {

  /**
   * Returns a string that renders the provided types into the GraphQL format.
   */
  @deprecated("Prefer DocumentRenderer.render() to render a Document.", "2.3.1")
  def renderTypes(types: List[__Type]): String =
    DocumentRenderer.typesRenderer.render(types.sorted(typeOrdering))

  @deprecated("Prefer DocumentRenderer.directivesRenderer.render instead", "2.3.1")
  def renderSchemaDirectives(directives: List[Directive]): String =
    DocumentRenderer.directivesRenderer.render(directives)

  @deprecated("Prefer DocumentRenderer.descriptionRenderer instead", "2.3.1")
  private[caliban] def renderDescription(description: Option[String], newline: Boolean = true): String =
    if (newline) DocumentRenderer.descriptionRenderer.render(description)
    else DocumentRenderer.descriptionRenderer.renderCompact(description)

  @deprecated("Prefer DocumentRenderer.renderTypeName instead", "2.3.1")
  private[caliban] def renderTypeName(fieldType: __Type): String =
    DocumentRenderer.renderTypeName(fieldType)
}

package caliban

import caliban.Rendering.renderType
import caliban.Types.collectTypes

object GraphQL {

  def schema[A](implicit ev: Schema[A]): String = collectTypes(ev.toType).map(renderType).mkString("\n")

  def execute[A](query: Parser.OperationDefinition, resolver: A)(implicit ev: Executer[A]): String =
    ev.exec(resolver, query.selectionSet)
}

package caliban

import caliban.Rendering.renderType
import caliban.Types.collectTypes
import caliban.parsing.adt.Document
import caliban.parsing.adt.ExecutableDefinition.OperationDefinition

object GraphQL {

  def schema[A](implicit ev: Schema[A]): String = collectTypes(ev.toType).map(renderType).mkString("\n")

  def execute[A](query: Document, resolver: A)(implicit ev: Executer[A]): String =
    query.definitions.map {
      case OperationDefinition(_, _, _, _, selection) => ev.exec(resolver, selection)
      case _                                          => ""
    }.mkString("\n")

}

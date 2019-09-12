package caliban

import caliban.Rendering.renderType
import caliban.Types.collectTypes
import caliban.execution.{ Executer, ResponseValue }
import caliban.parsing.adt.Document
import caliban.parsing.adt.ExecutableDefinition.OperationDefinition

object GraphQL {

  def schema[A](implicit ev: Schema[A]): String = collectTypes(ev.toType).map(renderType).mkString("\n")

  def execute[A](query: Document, resolver: A)(implicit ev: Executer[A]): List[ResponseValue] =
    query.definitions.flatMap {
      case OperationDefinition(_, _, _, _, selection) => Some(ev.exec(resolver, selection))
      case _                                          => None
    }

}

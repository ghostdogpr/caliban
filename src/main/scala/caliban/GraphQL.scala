package caliban

import caliban.Rendering.renderType
import caliban.schema.Types.{ collectTypes, Type }
import caliban.parsing.adt.{ Document, Selection, Value }
import caliban.parsing.adt.ExecutableDefinition.OperationDefinition
import caliban.schema.{ ResponseValue, Schema }
import zio.{ Runtime, Task, ZIO }

class GraphQL[G](schema: Schema[G]) {

  def render: String = collectTypes(schema.toType).map(renderType).mkString("\n")

  def execute(query: Document, resolver: G): Task[List[ResponseValue]] =
    Task.collectAll(query.definitions.flatMap {
      case OperationDefinition(_, _, _, _, selection) => Some(schema.exec(resolver, selection))
      case _                                          => None
    })
}

object GraphQL {

  def graphQL[G](implicit schema: Schema[G]): GraphQL[G] = new GraphQL[G](schema)

  implicit def effectSchema[R, E <: Throwable, A](implicit ev: Schema[A], runtime: Runtime[R]): Schema[ZIO[R, E, A]] =
    new Schema[ZIO[R, E, A]] {
      override def toType: Type = ev.toType
      override def exec(
        value: ZIO[R, E, A],
        selectionSet: List[Selection],
        arguments: Map[String, Value]
      ): Task[ResponseValue] = value.flatMap(ev.exec(_, selectionSet, arguments)).provide(runtime.Environment)
    }

}

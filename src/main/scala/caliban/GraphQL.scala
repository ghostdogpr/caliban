package caliban

import caliban.Rendering.renderType
import caliban.parsing.Parser
import caliban.parsing.adt.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.{ Selection, Value }
import caliban.schema.Types.{ collectTypes, Type }
import caliban.schema.{ ResponseValue, Schema }
import zio.{ Runtime, Task, ZIO }

class GraphQL[G](schema: Schema[G]) {

  def render: String = collectTypes(schema.toType).map(renderType).mkString("\n")

  def execute(query: String, resolver: G): Task[List[ResponseValue]] =
    for {
      document <- Task(Parser.parseQuery(query).get).map(_.value)
      result <- Task.collectAll(document.definitions.flatMap {
                 case OperationDefinition(_, _, _, _, selection) => Some(schema.exec(resolver, selection))
                 case _                                          => None
               })
    } yield result

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

package caliban

import caliban.CalibanError.ExecutionError
import caliban.Rendering.renderTypes
import caliban.parsing.Parser
import caliban.parsing.adt.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.{ Selection, Value }
import caliban.schema.Types.{ collectTypes, Type }
import caliban.schema.{ ResponseValue, Schema }
import caliban.validation.Validator
import zio.{ IO, Runtime, ZIO }

class GraphQL[G](schema: Schema[G]) {

  def render: String = renderTypes(collectTypes(schema.toType))

  def execute(query: String, resolver: G): IO[CalibanError, List[ResponseValue]] =
    for {
      document <- Parser.parseQuery(query)
      _        <- Validator.validate(document, schema)
      result <- IO.collectAll(document.definitions.flatMap {
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
      ): IO[ExecutionError, ResponseValue] =
        value.flatMap(ev.exec(_, selectionSet, arguments)).provide(runtime.Environment).mapError {
          case e: ExecutionError => e
          case other             => ExecutionError("Caught error during execution of effectful field", Some(other))
        }
    }

}

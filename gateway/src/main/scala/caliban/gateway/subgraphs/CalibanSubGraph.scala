package caliban.gateway.subgraphs

import caliban.CalibanError.ExecutionError
import caliban.{ GraphQL, ResponseValue }
import caliban.execution.Field
import caliban.gateway.SubGraph
import caliban.gateway.SubGraph.SubGraphExecutor
import caliban.introspection.Introspector
import caliban.introspection.adt.__Schema
import caliban.parsing.adt.OperationType
import caliban.validation.Validator
import zio.{ RIO, ZIO }

case class CalibanSubGraph[R](name: String, api: GraphQL[R], exposeAtRoot: Boolean) extends SubGraph[R] { self =>
  def build: RIO[R, SubGraphExecutor[R]] =
    for {
      interpreter  <- api.interpreter
      schemaBuilder = api.getSchemaBuilder
      rootSchema   <- Validator.validateSchema(schemaBuilder)
    } yield new SubGraphExecutor[R] {
      val name: String          = self.name
      val exposeAtRoot: Boolean = self.exposeAtRoot
      val schema: __Schema      = __Schema(
        schemaBuilder.schemaDescription,
        rootSchema.query.opType,
        rootSchema.mutation.map(_.opType),
        rootSchema.subscription.map(_.opType),
        schemaBuilder.types,
        Introspector.directives ++ api.getAdditionalDirectives
      )

      def run(field: Field, operationType: OperationType): ZIO[R, ExecutionError, ResponseValue] =
        interpreter
          .executeRequest(field.toGraphQLRequest(operationType))
          .map(_.data) // TODO: handle errors
    }
}

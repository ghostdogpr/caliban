package caliban

import caliban.Rendering.renderTypes
import caliban.execution.Executor
import caliban.introspection.Introspector
import caliban.introspection.adt.__Introspection
import caliban.parsing.Parser
import caliban.parsing.adt.Value
import caliban.schema.RootSchema.Operation
import caliban.schema._
import caliban.validation.Validator
import zio.ZIO

/**
 * A `GraphQL[R, Q, M, S]` represents a GraphQL interpreter for a query type `Q`, a mutation type `M`
 * and a subscription type `S`, requiring a ZIO environment of type `R` to be ran.
 *
 * It is intended to be created only once, typically when you start your server.
 * The introspection schema will be generated when this class is instantiated.
 */
class GraphQL[R, Q, M, S](schema: RootSchema[R, Q, M, S]) {

  private val rootType =
    RootType(
      schema.query.schema.toType(),
      schema.mutation.map(_.schema.toType()),
      schema.subscription.map(_.schema.toType())
    )
  private val introspectionRootSchema: RootSchema[Any, __Introspection, Nothing, Nothing] =
    Introspector.introspect(rootType)
  private val introspectionRootType = RootType(introspectionRootSchema.query.schema.toType(), None, None)

  /**
   * Returns a string that renders the interpreter types into the GraphQL format.
   */
  def render: String = renderTypes(rootType.types)

  /**
   * Parses, validates and finally runs the provided query against this interpreter.
   * @param query a string containing the GraphQL query.
   * @param operationName the operation to run in case the query contains multiple operations.
   * @param variables a list of variables.
   * @return an effect that either fails with a [[CalibanError]] or succeeds with a [[ResponseValue]]
   */
  def execute(
    query: String,
    operationName: Option[String] = None,
    variables: Map[String, Value] = Map()
  ): ZIO[R, CalibanError, ResponseValue] =
    for {
      document        <- Parser.parseQuery(query)
      intro           = Introspector.isIntrospection(document)
      typeToValidate  = if (intro) introspectionRootType else rootType
      _               <- Validator.validate(document, typeToValidate)
      schemaToExecute = if (intro) introspectionRootSchema else schema
      result          <- Executor.executeRequest(document, schemaToExecute, operationName, variables)
    } yield result
}

object GraphQL {

  /**
   * Builds a GraphQL interpreter for the given resolver.
   *
   * It requires an instance of [[caliban.schema.Schema]] for each operation type.
   * This schema will be derived by Magnolia automatically.
   */
  def graphQL[R, Q, M, S: SubscriptionSchema](resolver: RootResolver[Q, M, S])(
    implicit querySchema: Schema[R, Q],
    mutationSchema: Schema[R, M],
    subscriptionSchema: Schema[R, S]
  ): GraphQL[R, Q, M, S] =
    new GraphQL[R, Q, M, S](
      RootSchema(
        Operation(querySchema, resolver.queryResolver),
        resolver.mutationResolver.map(Operation(mutationSchema, _)),
        resolver.subscriptionResolver.map(Operation(subscriptionSchema, _))
      )
    )

}

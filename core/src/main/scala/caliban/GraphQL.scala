package caliban

import caliban.Rendering.renderTypes
import caliban.execution.Executor
import caliban.introspection.Introspector
import caliban.introspection.adt.__Introspection
import caliban.parsing.Parser
import caliban.schema.RootSchema.Operation
import caliban.schema._
import caliban.validation.Validator
import zio.{ IO, ZIO }

/**
 * A `GraphQL[R, Q, M, S, E]` represents a GraphQL interpreter for a query type `Q`, a mutation type `M`
 * and a subscription type `S`, whose execution requires a ZIO environment of type `R` and can fail with an `E`.
 *
 * It is intended to be created only once, typically when you start your server.
 * The introspection schema will be generated when this class is instantiated.
 */
trait GraphQL[-R, -Q, -M, -S, +E] { self =>

  /**
   * Parses and validates the provided query against this interpreter.
   * @param query a string containing the GraphQL query.
   * @return an effect that either fails with a [[CalibanError]] or succeeds with `Unit`
   */
  def check(query: String): IO[CalibanError, Unit]

  /**
   * Parses, validates and finally runs the provided query against this interpreter.
   * @param query a string containing the GraphQL query.
   * @param operationName the operation to run in case the query contains multiple operations.
   * @param variables a list of variables.
   * @param skipValidation skips the validation step if true
   * @return an effect that either fails with an `E` or succeeds with a [[ResponseValue]]
   */
  def execute(
    query: String,
    operationName: Option[String] = None,
    variables: Map[String, InputValue] = Map(),
    skipValidation: Boolean = false
  ): ZIO[R, E, ResponseValue]

  /**
   * Returns a string that renders the interpreter types into the GraphQL format.
   */
  def render: String

  /**
   * Changes the error channel of the `execute` method.
   * This can be used to customize error messages.
   * @param f a function from the current error type `E` to another type `E2`
   * @return a new GraphQL interpreter with error type `E2`
   */
  def mapError[E2](f: E => E2): GraphQL[R, Q, M, S, E2] = wrapExecutionWith(_.mapError(f))

  /**
   * Eliminates the ZIO environment R requirement of the interpreter.
   * @param r a value of type `R`
   * @return a new GraphQL interpreter with R = `Any`
   */
  def provide(r: R): GraphQL[Any, Q, M, S, E] = wrapExecutionWith(_.provide(r))

  /**
   * Wraps the `execute` method of the interpreter with the given function.
   * This can be used to customize errors, add global timeouts or logging functions.
   * @param f a function from `ZIO[R, E, ResponseValue]` to `ZIO[R2, E2, ResponseValue]`
   * @return a new GraphQL interpreter
   */
  def wrapExecutionWith[R2, E2](f: ZIO[R, E, ResponseValue] => ZIO[R2, E2, ResponseValue]): GraphQL[R2, Q, M, S, E2] =
    new GraphQL[R2, Q, M, S, E2] {
      override def check(query: String): IO[CalibanError, Unit] = self.check(query)
      override def execute(
        query: String,
        operationName: Option[String],
        variables: Map[String, InputValue],
        skipValidation: Boolean
      ): ZIO[R2, E2, ResponseValue] = f(self.execute(query, operationName, variables, skipValidation))
      override def render: String   = self.render
    }
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
  ): GraphQL[R, Q, M, S, CalibanError] =
    new GraphQL[R, Q, M, S, CalibanError] {
      val schema: RootSchema[R, Q, M, S] = RootSchema(
        Operation(querySchema.toType(), querySchema.resolve(resolver.queryResolver)),
        resolver.mutationResolver.map(r => Operation(mutationSchema.toType(), mutationSchema.resolve(r))),
        resolver.subscriptionResolver.map(r => Operation(subscriptionSchema.toType(), subscriptionSchema.resolve(r)))
      )
      val rootType = RootType(schema.query.opType, schema.mutation.map(_.opType), schema.subscription.map(_.opType))
      val introspectionRootSchema: RootSchema[Any, __Introspection, Nothing, Nothing] =
        Introspector.introspect(rootType)
      val introspectionRootType = RootType(introspectionRootSchema.query.opType, None, None)

      def check(query: String): IO[CalibanError, Unit] =
        for {
          document       <- Parser.parseQuery(query)
          intro          = Introspector.isIntrospection(document)
          typeToValidate = if (intro) introspectionRootType else rootType
          _              <- Validator.validate(document, typeToValidate)
        } yield ()

      def execute(
        query: String,
        operationName: Option[String] = None,
        variables: Map[String, InputValue] = Map(),
        skipValidation: Boolean = false
      ): ZIO[R, CalibanError, ResponseValue] =
        for {
          document        <- Parser.parseQuery(query)
          intro           = Introspector.isIntrospection(document)
          typeToValidate  = if (intro) introspectionRootType else rootType
          schemaToExecute = if (intro) introspectionRootSchema else schema
          _               <- IO.when(!skipValidation)(Validator.validate(document, typeToValidate))
          result          <- Executor.executeRequest(document, schemaToExecute, operationName, variables)
        } yield result

      def render: String = renderTypes(rootType.types)
    }
}

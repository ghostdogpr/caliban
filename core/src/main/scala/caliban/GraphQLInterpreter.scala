package caliban

import caliban.Value.NullValue
import caliban.execution.QueryExecution
import zio.{ Has, IO, NeedsEnv, Tag, URIO, ZEnv, ZLayer }

/**
 * A `GraphQLInterpreter[-R, +E]` represents a GraphQL interpreter whose execution requires
 * a ZIO environment of type `R` and can fail with an `E`.
 *
 * It is a wrapper around a `GraphQL` API definition that allows adding some middleware around
 * query execution, and possibly transform the environment or the error type.
 */
trait GraphQLInterpreter[-R, +E] { self =>

  /**
   * Parses and validates the provided query against this API.
   * @param query a string containing the GraphQL query.
   * @return an effect that either fails with a [[CalibanError]] or succeeds with `Unit`
   */
  def check(query: String): IO[CalibanError, Unit]

  /**
   * Parses, validates and finally runs the provided request against this interpreter.
   * @param request a GraphQL request
   * @param skipValidation skips the validation step if true
   * @param enableIntrospection returns an error for introspection queries when false
   * @param queryExecution a strategy for executing queries in parallel or not
   * @return an effect that either fails with an `E` or succeeds with a [[ResponseValue]]
   */
  def executeRequest(
    request: GraphQLRequest,
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): URIO[R, GraphQLResponse[E]]

  /**
   * Parses, validates and finally runs the provided query against this interpreter.
   * @param query a string containing the GraphQL query
   * @param operationName the operation to run in case the query contains multiple operations
   * @param variables a map of variables
   * @param extensions a map of extensions
   * @param skipValidation skips the validation step if true
   * @param enableIntrospection returns an error for introspection queries when false
   * @param queryExecution a strategy for executing queries in parallel or not
   * @return an effect that either fails with an `E` or succeeds with a [[ResponseValue]]
   */
  def execute(
    query: String,
    operationName: Option[String] = None,
    variables: Map[String, InputValue] = Map(),
    extensions: Map[String, InputValue] = Map(),
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): URIO[R, GraphQLResponse[E]] =
    executeRequest(
      GraphQLRequest(Some(query), operationName, Some(variables), Some(extensions)),
      skipValidation = skipValidation,
      enableIntrospection = enableIntrospection,
      queryExecution = queryExecution
    )

  /**
   * Changes the error channel of the `execute` method.
   * This can be used to customize error messages.
   * @param f a function from the current error type `E` to another type `E2`
   * @return a new GraphQL interpreter with error type `E2`
   */
  final def mapError[E2](f: E => E2): GraphQLInterpreter[R, E2] =
    wrapExecutionWith(_.map(res => GraphQLResponse(res.data, res.errors.map(f), res.extensions)))

  /**
   * Provides the interpreter with its required environment, which eliminates
   * its dependency on `R`.
   */
  final def provide(r: R)(implicit ev: NeedsEnv[R]): GraphQLInterpreter[Any, E] = wrapExecutionWith(_.provide(r))

  /**
   * Provides a layer to this interpreter, which translates it to another level.
   */
  final def provideLayer[E1 >: E, R0, R1 <: Has[_]](
    layer: ZLayer[R0, E1, R1]
  )(implicit ev1: R1 <:< R, ev2: NeedsEnv[R]): GraphQLInterpreter[R0, E1] =
    wrapExecutionWith(_.provideLayer(layer).fold(e => GraphQLResponse(NullValue, List(e)), identity))

  /**
   * Provides the part of the environment that is not part of the `ZEnv`,
   * leaving a query that only depends on the `ZEnv`.
   */
  final def provideCustomLayer[E1 >: E, R1 <: Has[_]](
    layer: ZLayer[ZEnv, E1, R1]
  )(implicit ev: ZEnv with R1 <:< R, tagged: Tag[R1]): GraphQLInterpreter[ZEnv, E1] =
    provideSomeLayer[ZEnv](layer)

  /**
   * Splits the environment into two parts, providing one part using the
   * specified layer and leaving the remainder `R0`.
   */
  final def provideSomeLayer[R0 <: Has[_]]: GraphQLInterpreter.ProvideSomeLayer[R0, R, E] =
    new GraphQLInterpreter.ProvideSomeLayer[R0, R, E](self)

  /**
   * Wraps the `execute` method of the interpreter with the given function.
   * This can be used to customize errors, add global timeouts or logging functions.
   * @param f a function from `URIO[R, GraphQLResponse[E]]` to `URIO[R2, GraphQLResponse[E2]]`
   * @return a new GraphQL interpreter
   */
  final def wrapExecutionWith[R2, E2](
    f: URIO[R, GraphQLResponse[E]] => URIO[R2, GraphQLResponse[E2]]
  ): GraphQLInterpreter[R2, E2] = new GraphQLInterpreter[R2, E2] {
    override def check(query: String): IO[CalibanError, Unit] = self.check(query)
    override def executeRequest(
      request: GraphQLRequest,
      skipValidation: Boolean,
      enableIntrospection: Boolean,
      queryExecution: QueryExecution
    ): URIO[R2, GraphQLResponse[E2]]                          =
      f(self.executeRequest(request, skipValidation = skipValidation, enableIntrospection, queryExecution))
  }

}

object GraphQLInterpreter {

  final class ProvideSomeLayer[R0 <: Has[_], -R, +E](private val self: GraphQLInterpreter[R, E]) extends AnyVal {
    def apply[E1 >: E, R1 <: Has[_]](
      layer: ZLayer[R0, E1, R1]
    )(implicit ev1: R0 with R1 <:< R, ev2: NeedsEnv[R], tagged: Tag[R1]): GraphQLInterpreter[R0, E1] =
      self.provideLayer[E1, R0, R0 with R1](ZLayer.identity[R0] ++ layer)
  }
}

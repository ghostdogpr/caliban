package caliban

import zio.URIO

/**
 * A `GraphQLInterpreter[-R, +E]` represents a GraphQL interpreter whose execution requires
 * a ZIO environment of type `R` and can fail with an `E`.
 *
 * It is a wrapper around a `GraphQL` API definition that allows adding some middleware around
 * query execution, and possibly transform the environment or the error type.
 */
trait GraphQLInterpreter[-R, +E] { self =>

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
  ): URIO[R, GraphQLResponse[E]]

  /**
   * Changes the error channel of the `execute` method.
   * This can be used to customize error messages.
   * @param f a function from the current error type `E` to another type `E2`
   * @return a new GraphQL interpreter with error type `E2`
   */
  final def mapError[E2](f: E => E2): GraphQLInterpreter[R, E2] =
    wrapExecutionWith(_.map(res => GraphQLResponse(res.data, res.errors.map(f), res.extensions)))

  /**
   * Eliminates the ZIO environment R requirement of the interpreter.
   * @param r a value of type `R`
   * @return a new GraphQL interpreter with R = `Any`
   */
  final def provide(r: R): GraphQLInterpreter[Any, E] = wrapExecutionWith(_.provide(r))

  /**
   * Wraps the `execute` method of the interpreter with the given function.
   * This can be used to customize errors, add global timeouts or logging functions.
   * @param f a function from `URIO[R, GraphQLResponse[E]]` to `URIO[R2, GraphQLResponse[E2]]`
   * @return a new GraphQL interpreter
   */
  final def wrapExecutionWith[R2, E2](
    f: URIO[R, GraphQLResponse[E]] => URIO[R2, GraphQLResponse[E2]]
  ): GraphQLInterpreter[R2, E2] =
    (query: String, operationName: Option[String], variables: Map[String, InputValue], skipValidation: Boolean) =>
      f(self.execute(query, operationName, variables, skipValidation))
}

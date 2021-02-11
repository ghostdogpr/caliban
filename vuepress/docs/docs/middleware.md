# Middleware

Caliban allows you to perform additional actions at various levels of a query processing, via the concept of `Wrapper`. Using wrappers, you can:
- verify that a query doesn't reach some limit (e.g. depth, complexity)
- modify a query before it's executed
- add timeouts to queries or fields
- log each field execution time
- support [Apollo Tracing](https://github.com/apollographql/apollo-tracing), [Apollo Caching](https://github.com/apollographql/apollo-cache-control) or anything similar
- etc.

## Wrapper types

There are 6 basic types of wrappers:
 - `OverallWrapper` to wrap the whole query processing
 - `ParsingWrapper` to wrap the query parsing only
 - `ValidationWrapper` to wrap the query validation only
 - `ExecutionWrapper` to wrap the query execution only
 - `FieldWrapper` to wrap each field execution
 - `IntrospectionWrapper` to wrap the introspection query only

Each one requires a function that takes a `ZIO` or `ZQuery` computation together with some contextual information (e.g. the query string) and should return another computation.

Let's see how to implement a wrapper that times out the whole query if its processing takes longer that 1 minute.

```scala
val wrapper = OverallWrapper { process => (request: GraphQLRequest) =>
  process(request)
    .timeout(1 minute)
    .map(
      _.getOrElse(
        GraphQLResponse(
          NullValue,
          List(ExecutionError(s"Query was interrupted after timeout of ${duration.render}:\n$query"))
        )
      )
    )
  }
```

You can also combine wrappers using `|+|` and create a wrapper that requires an effect to be run at each query using `EffectfulWrapper`.

To use your wrapper, call `GraphQL#withWrapper` or its alias `@@`.
```scala
val api = graphQL(...).withWrapper(wrapper)
// or
val api = graphQL(...) @@ wrapper
```

## Pre-defined wrappers


Caliban comes with a few pre-made wrappers in `caliban.wrappers.Wrappers`:
- `maxDepth` returns a wrapper that fails queries whose depth is higher than a given value
- `maxFields` returns a wrapper that fails queries whose number of fields is higher than a given value
- `timeout` returns a wrapper that fails queries taking more than a specified time
- `printErrors` returns a wrapper that prints errors
- `printSlowQueries` returns a wrapper that prints slow queries
- `onSlowQueries` returns a wrapper that can run a given function on slow queries

In addition to those, Caliban also ships with some non-spec but standard wrappers
- `caliban.wrappers.ApolloTracing.apolloTracing` returns a wrapper that adds tracing data into the `extensions` field of each response following [Apollo Tracing](https://github.com/apollographql/apollo-tracing) format.
- `caliban.wrappers.ApolloCaching.apolloCaching` returns a wrapper that adds caching hints to properly annotated fields using the [Apollo Caching](https://github.com/apollographql/apollo-cache-control) format.
- `caliban.wrappers.ApolloPersistedQueries.apolloPersistedQueries` returns a wrapper that caches and retrieves query using a hash using the [Apollo Persisted Queries](https://github.com/apollographql/apollo-link-persisted-queries) format.

They can be used like this:
```scala
val api =
  graphQL(...) @@
    maxDepth(50) @@
    timeout(3 seconds) @@
    printSlowQueries(500 millis) @@
    apolloTracing @@
    apolloCaching
```

## Wrapping the interpreter

All the wrappers mentioned above require that you don't modify the environment `R` and the error type which is always a `CalibanError`. It is also possible to wrap your `GraphQLInterpreter` by calling `wrapExecutionWith` on it. This method takes in a function `f` and returns a new `GraphQLInterpreter` that will wrap the `execute` method with this function `f`.

It is used internally to implement `mapError` (customize errors) and `provide` (eliminate the environment), but you can use it for other purposes such as adding a general timeout, logging response times, etc.

```scala
val i: GraphQLInterpreter[MyEnv, CalibanError] = ???

// change error type to String
val i2: GraphQLInterpreter[MyEnv, String] = i.mapError(_.toString)

// provide the environment
val i3: GraphQLInterpreter[Any, CalibanError] = i.provide(myEnv)

// add a timeout on every query execution
val i4: GraphQLInterpreter[MyEnv with Clock, CalibanError] =
  i.wrapExecutionWith(
    _.timeout(30 seconds).map(
      _.getOrElse(GraphQLResponse(NullValue, List(ExecutionError("Timeout!"))))
    )
  )
```

## Customizing error responses

During various phases of executing a query, an error may occur. Caliban renders the different instances of `CalibanError` to a GraphQL spec compliant response. As a user, you will most likely encounter `ExecutionError` at some point because this will encapsulate the errors in the error channel of your effects. For Caliban to be able to render some basic message about the error that occured during query execution, it is important that your error extends `Throwable`.

For more meaningful error handling, GraphQL spec allows for an [`extension`](http://spec.graphql.org/June2018/#example-fce18) object in the error response. This object may include, for instance, `code` information to model enum-like error codes that can be handled by a front-end. In order to generate this information, one can use the `mapError` function on a `GraphQLInterpreter`. An example is provided below in which we map a custom domain error within an `ExecutionError` to a meaningful error code.

```scala
sealed trait ExampleAppEncodableError extends Throwable {
    def errorCode: String
}
case object UnauthorizedError extends ExampleAppEncodableError {
    override def errorCode: String = "UNAUTHORIZED"
}

def withErrorCodeExtensions[R](
  interpreter: GraphQLInterpreter[R, CalibanError]
): GraphQLInterpreter[R, CalibanError] = interpreter.mapError {
  case err @ ExecutionError(_, _, _, Some(exampleError: ExampleAppEncodableError), _) =>
    err.copy(extensions = Some(ObjectValue(List(("errorCode", StringValue(exampleError.errorCode))))))
  case err: ExecutionError =>
    err.copy(extensions = Some(ObjectValue(List(("errorCode", StringValue("EXECUTION_ERROR"))))))
  case err: ValidationError =>
    err.copy(extensions = Some(ObjectValue(List(("errorCode", StringValue("VALIDATION_ERROR"))))))
  case err: ParsingError =>
    err.copy(extensions = Some(ObjectValue(List(("errorCode", StringValue("PARSING_ERROR"))))))
}
```
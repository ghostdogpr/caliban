# Middleware

Caliban allows you to perform additional actions at various level of a query processing, via the concept of `Wrapper`. Using wrappers, you can:
- verify that a query doesn't reach some limit (e.g. depth, complexity)
- modify a query before it's executed
- add timeouts to queries or fields
- log each field execution time
- support [Apollo Tracing](https://github.com/apollographql/apollo-tracing) or anything similar
- etc.

## Wrapper types

There are 5 basic types of wrappers:
 - `OverallWrapper` to wrap the whole query processing
 - `ParsingWrapper` to wrap the query parsing only
 - `ValidationWrapper` to wrap the query validation only
 - `ExecutionWrapper` to wrap the query execution only
 - `FieldWrapper` to wrap each field execution

Each one requires a function that takes a `ZIO` or `ZQuery` computation together with some contextual information (e.g. the query string) and should return another computation.

Let's see how to implement a wrapper that times out the whole query if its processing takes longer that 1 minute.

```scala
val wrapper = OverallWrapper {
  case (io, query) =>
    io.timeout(1 minute)
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
- `printSlowQueries` returns a wrapper that prints slow queries
- `onSlowQueries` returns a wrapper that can run a given function on slow queries

In addition to those, `caliban.wrappers.ApolloTracing.apolloTracing` returns a wrapper that adds tracing data into the `extensions` field of each response following [Apollo Tracing](https://github.com/apollographql/apollo-tracing) format.

They can be used like this:
```scala
val api =
  graphQL(...) @@
    maxDepth(50) @@
    timeout(3 seconds) @@
    printSlowQueries(500 millis) @@
    apolloTracing
```

## Wrapping the interpreter

All the wrappers mentioned above require that you don't modify the environment `R` and the error type which is always a `CalibanError`. It is also possible to wrap your `GraphQLInterpreter` by calling `wrapExecutionWith` on it. This method takes in a function `f` and returns a new `GraphQLInterpreter` that will wrap the `execute` method with this function `f`.

It is used internally to implement `mapError` (customize errors) and `provide` (eliminate the environment), but you can use it for other purposes such as adding a general timeout, logging response times, etc.

```scala
// create an interpreter
val i: GraphQLInterpreter[MyEnv, CalibanError] = graphqQL(...).interpreter

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
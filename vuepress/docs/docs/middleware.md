# Middleware

You might want to perform some actions on every query received. Caliban supports this in 2 different ways:

- you can wrap the execution of each query, or only some part of it, with arbitrary code.
- you can analyze the requested fields before execution, and possibly modify or reject the query

## Wrappers

You can use `GraphQL#withWrapper` to register a function that will wrap the execution of the query processing (or only a part of it).

There are 5 types of wrappers:
 - `OverallWrapper` to wrap the whole query processing
 - `ParsingWrapper` to wrap the query parsing only
 - `ValidationWrapper` to wrap the query validation only
 - `ExecutionWrapper` to wrap the query execution only
 - `FieldWrapper` to wrap each field execution

Each one requires a function that takes an `IO` or `ZQuery` computation together with some contextual information (e.g. the query string) and should return another computation.

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

val api = graphQL(...).withWrapper(wrapper)
```

Caliban comes with a few pre-made wrappers:
- `caliban.wrappers.Wrappers.logSlowQueries` returns a wrapper that logs slow queries
- `caliban.wrappers.Wrappers.timeout` returns a wrapper that fails queries taking more than a specified time
- `caliban.wrappers.ApolloTracing.apolloTracing` returns a wrapper that adds tracing data into the `extensions` field of each response following [Apollo Tracing](https://github.com/apollographql/apollo-tracing) format.

They can be used like this:
```scala
val api =
  apolloTracing( // note: this constructor is effectful
    logSlowQueries(500 millis)(
      timeout(3 seconds)(  
        graphQL(...)
      )
    )
  )
```

All the wrappers mentioned above require that you don't modify the environment `R` and the error type which is always a `CalibanError`. It is also possible to wrap your `GraphQL` interpreter by calling `wrapExecutionWith` on it. This method takes in a function `f` and returns a new `GraphQL` interpreter that will wrap the `execute` method with this function `f`.

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

## Query Analyzers

You can use `GraphQL#withQueryAnalyzer` to register a hook function that will be run before query execution. Such function is called a `QueryAnalyzer` and looks like this:

```scala
type QueryAnalyzer[-R] = Field => ZIO[R, CalibanError, Field]
```

As an input, you receive the root `Field` object that contains the whole query in a convenient format (the fragments are replaced by the actual fields). You can analyze this object and return a `ZIO[R, CalibanError, Field]` that allows you to:

- modify the query (e.g. add or remove fields)
- return an error to prevent execution
- run some effect (e.g. write metrics somewhere)

A typical use case is to limit the number of fields or the depth of the query. Those are already implemented in Caliban and can be used like this:

```scala
val api =
  maxDepth(30)(
    maxFields(200)(
      graphQL(...)
    )
  )
```

You can look at their implementation which is only a few lines long.

You can even use a Query Analyzer to "inject" some data into your ZIO environment and possibly use it later during execution. For example, you can have a `Context` as part of your environment:

```scala
case class ContextData(cost: Int)
trait Context {
  def context: Ref[ContextData]
}
```

Then you can register a Query Analyzer that calculates the cost of every query (in this example, I just used the number of fields) and save it into the context. that way you can support an API that returns the cost associated with each query.

```scala
api.withQueryAnalyzer { root =>
  val cost = QueryAnalyzer.countFields(root)
  ZIO.accessM[Context](_.context.update(_.copy(cost = cost))).as(root)
}
```

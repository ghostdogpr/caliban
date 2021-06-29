# Getting Started

**Caliban** is a purely functional library for creating GraphQL servers and clients in Scala.

For more details on Caliban Client, see the [dedicated section](client.md). The rest of this page is about the backend part of the library.

The design principles of Caliban are the following:

- **pure interface**: errors and effects are returned explicitly (no exceptions thrown), all returned types are referentially transparent (no usage of `Future`).
- **minimal amount of boilerplate**: no need to manually define a schema for every type in your API. Let the compiler do the boring work.
- **excellent interoperability**: out-of-the-box support for major HTTP server libraries ([http4s](https://http4s.org/), [Akka HTTP](https://doc.akka.io/docs/akka-http/current/index.html), [Play](https://www.playframework.com/), [Finch](https://github.com/finagle/finch), [ZIO HTTP](https://github.com/dream11/zio-http)), effect types (Future, [ZIO](https://zio.dev/), [Cats Effect](https://typelevel.org/cats-effect/), [Monix](https://monix.io/)), Json libraries ([Circe](https://circe.github.io/circe/), [Play Json](https://github.com/playframework/play-json), [ZIO Json](https://github.com/zio/zio-json)), various integrations ([Apollo Tracing](https://github.com/apollographql/apollo-tracing), [Apollo Federation](https://www.apollographql.com/docs/federation/), [Tapir](https://tapir.softwaremill.com/en/latest/), etc.) and more.

## Dependencies

To use `caliban`, add the following line in your `build.sbt` file:

```
libraryDependencies += "com.github.ghostdogpr" %% "caliban" % "1.0.1"
```

The following modules are optional:

```
libraryDependencies += "com.github.ghostdogpr" %% "caliban-http4s"     % "1.0.1" // routes for http4s
libraryDependencies += "com.github.ghostdogpr" %% "caliban-akka-http"  % "1.0.1" // routes for akka-http
libraryDependencies += "com.github.ghostdogpr" %% "caliban-play"       % "1.0.1" // routes for play
libraryDependencies += "com.github.ghostdogpr" %% "caliban-finch"      % "1.0.1" // routes for finch
libraryDependencies += "com.github.ghostdogpr" %% "caliban-zio-http"   % "1.0.1" // routes for zio-http
libraryDependencies += "com.github.ghostdogpr" %% "caliban-cats"       % "1.0.1" // interop with cats effect
libraryDependencies += "com.github.ghostdogpr" %% "caliban-monix"      % "1.0.1" // interop with monix
libraryDependencies += "com.github.ghostdogpr" %% "caliban-federation" % "1.0.1" // interop with apollo federation
```

## A simple example

Creating a GraphQL API with Caliban is as simple as creating a case class. Indeed, the whole GraphQL schema will be derived from a case class structure (its fields and the other types it references), and the resolver is just an instance of that case class.

Let's say we have a class `Character` and 2 functions: `getCharacters` and `getCharacter`:

```scala
case class Character(name: String, age: Int)

def getCharacters: List[Character] = ???
def getCharacter(name: String): Option[Character] = ???
```

Let's create a case class named `Queries` that will represent our API, with 2 fields named and modeled after the functions we want to expose (a _record of functions_). We then create a value of this class that calls our actual functions. This is our resolver.

```scala
// schema
case class CharacterName(name: String)
case class Queries(characters: List[Character],
                   character: CharacterName => Option[Character])
// resolver
val queries = Queries(getCharacters, args => getCharacter(args.name))
```

The next step is creating our GraphQL API definition. First, we wrap our query resolver inside a `RootResolver`, the root object that contains queries, mutations and subscriptions. Only queries are mandatory.
Then we can call the `graphQL` function which will turn our simple resolver value into a GraphQL API definition.
The whole schema will be derived at compile time, meaning that if it compiles, it will be able to serve it.

```scala
import caliban.GraphQL.graphQL
import caliban.RootResolver

val api = graphQL(RootResolver(queries))
```

You can use `api.render` to visualize the schema generated, in this case:

```graphql
type Character {
  name: String!
  age: Int!
}

type Queries {
  characters: [Character!]!
  character(name: String!): Character
}
```

In order to process requests, you need to turn your API into an interpreter, which can be done easily by calling `.interpreter`.
An interpreter is a light wrapper around the API definition that allows plugging in some middleware and possibly modifying the environment and error types (see [Middleware](middleware.md) for more info).
Creating the interpreter may fail with a `ValidationError` if some type is found invalid.

```scala
for {
  interpreter <- api.interpreter
} yield interpreter
```

Now you can call `interpreter.execute` with a given GraphQL query, and you will get an `ZIO[R, Nothing, GraphQLResponse[CalibanError]]` as a response, with `GraphQLResponse` defined as follows:

```scala
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E])
```

Use `ResponseValue#toString` to get the JSON representation of the result.

```scala
val query = """
  {
    characters {
      name
    }
  }"""

for {
  result <- interpreter.execute(query)
  _      <- zio.console.putStrLn(result.data.toString)
} yield ()
```

A `CalibanError` can be:

- a `ParsingError`: the query has invalid syntax
- a `ValidationError`: the query was parsed but does not match the schema
- an `ExecutionError`: an error happened while executing the query

Caliban itself is not tied to any web framework, you are free to expose this function using the protocol and library of your choice. The [caliban-http4s](https://github.com/ghostdogpr/caliban/tree/master/adapters/http4s) module provides an `Http4sAdapter` that exposes an interpreter over HTTP and WebSocket using http4s. There are also similar adapters for Akka HTTP, Play, Finch and zio-http.

::: tip Combining GraphQL APIs
You don't have to define all your root fields into a single case class: you can use smaller case classes and combine `GraphQL` objects using the `|+|` operator.

```scala
val api1 = graphQL(...)
val api2 = graphQL(...)

val api = api1 |+| api2
```

You can use `.rename` to change the names of the generated root types.
:::

## Mutations

Creating mutations is the same as queries, except you pass them as the second argument to `RootResolver`:

```scala
case class CharacterArgs(name: String)
case class Mutations(deleteCharacter: CharacterArgs => Task[Boolean])
val mutations = Mutations(???)
val api = graphQL(RootResolver(queries, mutations))
```

## Subscriptions

Similarly, subscriptions are passed as the third argument to `RootResolver`:

```scala
case class Subscriptions(deletedCharacter: ZStream[Any, Nothing, Character])
val subscriptions = Subscriptions(???)
val api = graphQL(RootResolver(queries, mutations, subscriptions))
```

All the fields of the subscription root case class MUST return `ZStream` or `? => ZStream` objects. When a subscription request is received, an output stream of `ResponseValue` (a `StreamValue`) will be returned wrapped inside an `ObjectValue`.


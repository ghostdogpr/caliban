# Getting Started

**Caliban** is a purely functional library for creating GraphQL servers and clients in Scala.

For more details on Caliban Client, see the [dedicated section](client.md). The rest of this page is about the backend part of the library.

The design principles of Caliban are the following:

- **pure interface**: errors and effects are returned explicitly (no exceptions thrown), all returned types are referentially transparent (no usage of `Future`).
- **minimal amount of boilerplate**: no need to manually define a schema for every type in your API. Let the compiler do the boring work.
- **excellent interoperability**: out-of-the-box support for major HTTP server libraries ([http4s](https://http4s.org/), [Akka HTTP](https://doc.akka.io/docs/akka-http/current/index.html), [Play](https://www.playframework.com/), [ZIO HTTP](https://github.com/dream11/zio-http)), effect types (Future, [ZIO](https://zio.dev/), [Cats Effect](https://typelevel.org/cats-effect/), [Monix](https://monix.io/)), Json libraries ([Circe](https://circe.github.io/circe/), [Jsoniter](https://github.com/plokhotnyuk/jsoniter-scala), [Play Json](https://github.com/playframework/play-json), [ZIO Json](https://github.com/zio/zio-json)), various integrations ([Apollo Tracing](https://github.com/apollographql/apollo-tracing), [Apollo Federation](https://www.apollographql.com/docs/federation/), [Tapir](https://tapir.softwaremill.com/en/latest/), etc.) and more.

## Dependencies

To use `caliban`, add the following dependency to your `build.sbt` file:

```scala
"com.github.ghostdogpr" %% "caliban" % "2.1.0"
```

The following modules are optional:

```scala
"com.github.ghostdogpr" %% "caliban-http4s"     % "2.1.0" // routes for http4s
"com.github.ghostdogpr" %% "caliban-akka-http"  % "2.1.0" // routes for akka-http
"com.github.ghostdogpr" %% "caliban-play"       % "2.1.0" // routes for play
"com.github.ghostdogpr" %% "caliban-zio-http"   % "2.1.0" // routes for zio-http
"com.github.ghostdogpr" %% "caliban-cats"       % "2.1.0" // interop with cats effect
"com.github.ghostdogpr" %% "caliban-monix"      % "2.1.0" // interop with monix
"com.github.ghostdogpr" %% "caliban-tapir"      % "2.1.0" // interop with tapir
"com.github.ghostdogpr" %% "caliban-federation" % "2.1.0" // interop with apollo federation
```

Support for JSON encoding / decoding of the inputs and responses is enabled by adding **one** of the following dependencies to your `build.sbt` file:

```scala
"com.softwaremill.sttp.tapir" %% "tapir-json-circe"     % "1.2.11" // Circe
"com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % "1.2.11" // Jsoniter
"com.softwaremill.sttp.tapir" %% "tapir-json-play"      % "1.2.11" // Play JSON
"com.softwaremill.sttp.tapir" %% "tapir-json-zio"       % "1.2.11" // ZIO JSON
```

And then later in your code (you only need one!):

```scala
import sttp.tapir.json.circe._
import sttp.tapir.json.jsoniter._
import sttp.tapir.json.play._
import sttp.tapir.json.zio._
```

For more info on the adapters and the JSON implementations, see [here](adapters.md#json-handling).

## A simple example

Creating a GraphQL API with Caliban is as simple as creating a case class. Indeed, the whole GraphQL schema will be derived from a case class structure (its fields and the other types it references), and the resolver is just an instance of that case class.

Let's say we have a class `Character` and 2 functions: `getCharacters` and `getCharacter`:

```scala mdoc:silent
case class Character(name: String, age: Int)

def getCharacters: List[Character] = Nil
def getCharacter(name: String): Option[Character] = ???
```

Let's create a case class named `Queries` that will represent our API, with 2 fields named and modeled after the functions we want to expose (a _record of functions_). We then create a value of this class that calls our actual functions. This is our resolver.

```scala mdoc:silent
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

```scala mdoc:silent
import caliban._
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._

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

```scala mdoc:silent
for {
  interpreter <- api.interpreter
} yield interpreter
```

Now you can call `interpreter.execute` with a given GraphQL query, and you will get an `ZIO[R, Nothing, GraphQLResponse[CalibanError]]` as a response, with `GraphQLResponse` defined as follows:

```scala
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E])
```

Use `ResponseValue#toString` to get the JSON representation of the result.

```scala mdoc:silent
val query = """
  {
    characters {
      name
    }
  }"""

for {
  interpreter <- api.interpreter
  result      <- interpreter.execute(query)
  _           <- zio.ZIO.debug(result.data.toString)
} yield ()
```

A `CalibanError` can be:

- a `ParsingError`: the query has invalid syntax
- a `ValidationError`: the query was parsed but does not match the schema
- an `ExecutionError`: an error happened while executing the query

Caliban itself is not tied to any web framework, you are free to expose this function using the protocol and library of your choice.
The [caliban-http4s](https://github.com/ghostdogpr/caliban/tree/master/adapters/http4s) module provides an `Http4sAdapter` that exposes an interpreter over HTTP and WebSocket using http4s. There are also similar adapters for Akka HTTP, Play and zio-http.
Read more on the [adapters' documentation](adapters.md).

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

```scala mdoc:nest:silent
import zio.Task

case class CharacterArgs(name: String)
case class Mutations(deleteCharacter: CharacterArgs => Task[Boolean])
val mutations = Mutations(_ => ???)
val api = graphQL(RootResolver(queries, mutations))
```

## Subscriptions

Similarly, subscriptions are passed as the third argument to `RootResolver`:

```scala mdoc:compile-only
import zio.stream.ZStream

case class Subscriptions(deletedCharacter: ZStream[Any, Nothing, Character])
val subscriptions = Subscriptions(???)
val api = graphQL(RootResolver(queries, mutations, subscriptions))
```

All the fields of the subscription root case class MUST return `ZStream` or `? => ZStream` objects. When a subscription request is received, an output stream of `ResponseValue` (a `StreamValue`) will be returned wrapped inside an `ObjectValue`.


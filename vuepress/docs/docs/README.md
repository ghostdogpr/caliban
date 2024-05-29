# Getting Started

**Caliban** is a purely functional library for creating GraphQL servers and clients in Scala.

For more details on Caliban Client, see the [dedicated section](client.md). The rest of this page focuses on the backend part of the library.

The design principles of Caliban are the following:

- **high performance**: while all public interfaces are pure and immutable, the library's internals are optimized for speed, offering the best performance of any Scala GraphQL library.
- **minimal amount of boilerplate**: you don't need to manually define schemas for every type in your API. Let the compiler handle the tedious work.
- **excellent interoperability**: out-of-the-box support for major HTTP server libraries ([http4s](https://http4s.org/), [Akka HTTP](https://doc.akka.io/docs/akka-http/current/index.html), [Pekko HTTP](https://github.com/apache/incubator-pekko-http), [Play](https://www.playframework.com/), [ZIO HTTP](https://github.com/dream11/zio-http)), effect types (Future, [ZIO](https://zio.dev/), [Cats Effect](https://typelevel.org/cats-effect/), [Monix](https://monix.io/)), JSON libraries ([Circe](https://circe.github.io/circe/), [Jsoniter](https://github.com/plokhotnyuk/jsoniter-scala), [Play Json](https://github.com/playframework/play-json), [ZIO Json](https://github.com/zio/zio-json)), plus various integrations ([Apollo Tracing](https://github.com/apollographql/apollo-tracing), [Apollo Federation](https://www.apollographql.com/docs/federation/), [Tapir](https://tapir.softwaremill.com/en/latest/), etc.) and more.

## A simple schema

First, add the following dependency to your `build.sbt` file:

```scala
"com.github.ghostdogpr" %% "caliban" % "2.7.0"
```

Creating a GraphQL API with Caliban is as simple as creating a case class in Scala.
Indeed, the whole GraphQL schema will be derived from a case class hierarchy (its fields and the other types it references), and the resolver is just an instance of that case class.

Let's say we have a class `Character` and 2 functions: `getCharacters` and `getCharacter`:

```scala mdoc:silent
case class Character(name: String, age: Int)

def getCharacters: List[Character] = Nil
def getCharacter(name: String): Option[Character] = ???
```

Let's create a case class named `Queries` that will represent our API, with 2 fields named and modeled after the functions we want to expose.
We then create a value of this class that calls our actual functions. This is our resolver.

```scala mdoc:silent
// schema
case class CharacterName(name: String)
case class Queries(characters: List[Character],
                   character: CharacterName => Option[Character])
// resolver
val queries = Queries(getCharacters, args => getCharacter(args.name))
```

The next step is creating our GraphQL API definition.

First, we wrap our query resolver inside a `RootResolver`, the root object that contains queries, mutations and subscriptions. Only queries are mandatory.
Then we call the `graphQL` function which will turn our simple resolver value into a GraphQL API definition.
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

## Serving over HTTP
Once you have your API object, you can turn it into an interpreter (`api.interpreter`) and start processing requests (`interpreter.execute`).
The interpreter is not tied any web framework, so you are free to expose this function using the protocol and library of your choice.

The easiest (and most performant!) way to expose your API over HTTP is to use the optional `caliban-quick` module based on [zio-http](https://github.com/zio/zio-http).

```scala
"com.github.ghostdogpr" %% "caliban-quick"  % "2.7.0"
```

You can then serve your GraphQL API over HTTP using a single command:

```scala mdoc:compile-only
import caliban.quick._ // adds extension methods to `api`

api.unsafe.runServer(
  port = 8080,
  apiPath = "/api/graphql",
)
```

And that's it - now you have a fully functional GraphQL server running on port 8080!

## Interop with 3rd-party libraries

If you have any specific server requirements or need to interop with other libraries, Caliban offers a wide range of modules to help you do that.

```scala
"com.github.ghostdogpr" %% "caliban-http4s"     % "2.7.0" // routes for http4s
"com.github.ghostdogpr" %% "caliban-akka-http"  % "2.7.0" // routes for akka-http
"com.github.ghostdogpr" %% "caliban-pekko-http" % "2.7.0" // routes for pekko-http
"com.github.ghostdogpr" %% "caliban-play"       % "2.7.0" // routes for play

"com.github.ghostdogpr" %% "caliban-cats"       % "2.7.0" // interop with cats-effect
"com.github.ghostdogpr" %% "caliban-monix"      % "2.7.0" // interop with monix
"com.github.ghostdogpr" %% "caliban-tapir"      % "2.7.0" // interop with tapir

"com.github.ghostdogpr" %% "caliban-federation" % "2.7.0" // apollo federation
"com.github.ghostdogpr" %% "caliban-reporting"  % "2.7.0" // apollo schema reporting
"com.github.ghostdogpr" %% "caliban-tracing"    % "2.7.0" // open-telemetry
```

Support for JSON encoding / decoding of the inputs and responses for tapir-based adapters is enabled by adding **one** of the following dependencies to your `build.sbt` file:

```scala
"com.softwaremill.sttp.tapir" %% "tapir-json-circe"     % "1.10.7" // circe
"com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % "1.10.7" // jsoniter
"com.softwaremill.sttp.tapir" %% "tapir-json-play"      % "1.10.7" // play-json
"com.softwaremill.sttp.tapir" %% "tapir-json-zio"       % "1.10.7" // zio-json
```

And then later in your code (you only need one!):

```scala
import sttp.tapir.json.circe._
import sttp.tapir.json.jsoniter._
import sttp.tapir.json.play._
import sttp.tapir.json.zio._
```

For more info on the adapters and the JSON implementations, see [here](adapters.md#json-handling).

## Where to go next?

The rest of the documentation covers in details how to create [schemas](schema.md), [customize](middleware.md) and [optimize](optimization.md) query processing, [expose](adapters.md) APIs, and much more.

If you'd like to see a more detailed version of this tutorial, check out this [Beginner's Guide to GraphQL in Scala](https://blog.pierre-ricadat.com/a-beginners-guide-to-graphql-in-scala).
If you prefer looking at some code first, check [this list of examples](https://ghostdogpr.github.io/caliban/docs/examples.html).

If you have any questions, just come to the [Discord channel](https://discord.gg/EYpumuv)!

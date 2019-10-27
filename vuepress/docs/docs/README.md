# Getting Started

**Caliban** is a purely functional library for creating GraphQL backends in Scala.
It relies on [Magnolia](https://github.com/propensive/magnolia) to automatically derives GraphQL schemas from your data types, [Fastparse](https://github.com/lihaoyi/fastparse) to parse queries and [ZIO](https://github.com/zio/zio) to handle various effects.

The design principles behind the library are the following:
- pure interface: errors and effects are returned explicitly (no exceptions thrown), all returned types are referentially transparent (no `Future`).
- clean separation between schema definition and implementation: schema is defined and validated at compile-time (no reflection) using Scala standard types, resolver is a simple value provided at runtime.
- minimal amount of boilerplate: no need to manually define a schema for every type in your API.

## Dependencies
To use `caliban`, add the following line in your `build.sbt` file:

```
libraryDependencies += "com.github.ghostdogpr" %% "caliban" % "0.1.1"
```

The following modules are optional:
``` 
libraryDependencies += "com.github.ghostdogpr" %% "caliban-http4s" % "0.1.1" // routes for http4s
libraryDependencies += "com.github.ghostdogpr" %% "caliban-cats"   % "0.1.1" // interop with cats effect
```

Note that Caliban is also available for ScalaJS.

## A simple example
Creating a GraphQL API with Caliban is as simple as creating a case class. Indeed, the whole GraphQL schema will be derived from a case class structure (its fields and the other types it references), and the resolver is just an instance of that case class.

Let's say we have a class `Character` and 2 functions: `getCharacters` and `getCharacter`:
```scala
case class Character(name: String)

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

The next step is creating our graphql interpreter. First, we wrap our query resolver inside a `RootResolver`, the root object that contains queries, mutations and subscriptions. Only queries are mandatory. Then we can call the `graphQL` function which will turn our simple resolver value into an interpreter. The whole schema will be derived at compile time, meaning that if it compiles, it will be able to serve it.
```scala
import caliban.GraphQL.graphQL
import caliban.RootResolver

val interpreter = graphQL(RootResolver(queries))
```
You can use `interpreter.render` to visualize the schema generated, in this case:
```graphql
type Character {
  name: String!
}

type Queries {
  characters: [Character!]!
}
```

Now you can call `interpreter.execute` with a given GraphQL query, and you will get an `ZIO[R, CalibanError, ResponseValue]` as a response. Use `ResponseValue#toString` to get the JSON representation of the result.

```scala
val query = """
  { 
    characters {
      name
    }
  }"""

for {
  result <- interpreter.execute(query)
  _      <- zio.console.putStrLn(result.toString)
} yield ()
```

The `CalibanError` can be:
- a `ParsingError`: the query has invalid syntax
- a `ValidationError`: the query was parsed but does not match the schema
- an `ExecutionError`: an error happened while executing the query

Caliban itself is not tied to any web framework, you are free to expose this function using the protocol and library of your choice. The [caliban-http4s](https://github.com/ghostdogpr/caliban/tree/master/http4s) module provides an `Http4sAdapter` that exposes an interpreter over HTTP and WebSocket using http4s.

## Mutations
Creating mutations is the same as queries, except you pass them as the second argument to `RootResolver`:
```scala
case class CharacterArgs(name: String)
case class Mutations(deleteCharacter: CharacterArgs => Task[Boolean])
val mutations = Mutations(???)
val interpreter = graphQL(RootResolver(queries, mutations))
```

## Subscriptions
Similarly, subscriptions are passed as the third argument to `RootResolver`:
```scala
case class Subscriptions(deletedCharacter: ZStream[Any, Nothing, Character])
val subscriptions = Subscriptions(???)
val interpreter = graphQL(RootResolver(queries, mutations, subscriptions))
```
All the fields of the subscription root case class MUST return `ZStream` objects. When a subscription request is received, an output stream of `ResponseValue` will be returned wrapped in an `ObjectValue`.

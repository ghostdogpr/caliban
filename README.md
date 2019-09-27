# Caliban
Caliban is a purely functional library for creating GraphQL backends in Scala.
It relies on [Magnolia](https://github.com/propensive/magnolia) to automatically derives GraphQL schemas from your data types, [Fastparse](https://github.com/lihaoyi/fastparse) to parse queries and [ZIO](https://github.com/zio/zio) to handle various effects.

**Current state:** most of the [GraphQL spec](https://graphql.github.io/graphql-spec/June2018/) is implemented, but a few items are still missing, in particular **variables** and **directives**.

## Getting Started
To use `caliban`, add the following line in your `build.sbt` file:

```
libraryDependencies += "com.github.ghostdogpr" %% "caliban" % "TODO"
```

### Simple query
Creating a GraphQL API with Caliban is as simple as creating a case class. The whole GraphQL schema will be derived from the case class structure (its fields and the other types it references), and the resolver is just an instance of that case class.

Let's say we have a class `Character` and a function `getCharacters`:
```scala
case class Character(name: String)

def getCharacters: List[Character] = ???
```

Let's create a case class named `Queries` that will represent our API, with one field named `characters` that returns a list of `Character`. We then create a value of this class that calls our `getCharacters` function. 

```scala
case class Queries(characters: List[Character])

val queries = Queries(getCharacters)
```

The next step is creating our graphql interpreter. First, we wrap our query resolver inside a `RootResolver`, the root object that contains queries, mutations and subscriptions. Only queries are mandatory. Then we can call the `graphQL` function which will turn our simple value into an interpreter. The whole schema will be derived at compile time, meaning that if it compiles, it will be able to serve it.
```scala
import caliban.GraphQL._
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

Now you can call `interpreter.execute` with a given GraphQL query, and you will get an `IO[CalibanError, ResponseValue]` as a response. Use `ResponseValue#toString` to get the JSON representation of the result.

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

Caliban itself is not tied to any web framework, you are free to expose this function using the protocol and library of your choice. The [examples](https://github.com/ghostdogpr/caliban/tree/master/examples/) project provides an `Http4sAdapter` that exposes an interpreter over HTTP and WebSocket using http4s.

### Enum and union
A sealed trait will be converted to a different GraphQL type depending on its content:
- a sealed trait with only case objects will be converted to an `ENUM`
- a sealed trait with only case classes will be converted to a `UNION`

**Important:** sealed traits containing both case objects and case classes are not supported, because GraphQL does not have a corresponding type. Also, union types are not supported as arguments.

```scala
sealed trait ORIGIN
object ORIGIN {
  case object EARTH extends ORIGIN
  case object MARS  extends ORIGIN
  case object BELT  extends ORIGIN
}
```
The snippet above will produce the following GraphQL type:
```graphql
enum Origin {
  BELT
  EARTH
  MARS
}
```

Here's an example of union:
```scala
sealed trait Role
object Role {
  case class Captain(shipName: String) extends Role
  case class Engineer(specialty: String) extends Role
}
```
The snippet above will produce the following GraphQL type:
```graphql
union Role = Captain | Engineer

type Captain {
  shipName: String!
}

type Engineer {
  specialty: String!
}
```

### Arguments
To declare a field that take arguments, create a dedicated case class representing the arguments and make the field a *function* from this class to the result type.
```scala
case class FilterArgs(origin: Option[Origin])
case class Queries(characters: FilterArgs => List[Character])
```
The snippet above will produce the following GraphQL type:
```graphql
input FilterArgs {
  origin: Origin
}
type Queries {
  characters(origin: Origin): [Character!]!
} 
```
### Effects
Fields can return ZIO effects. The only thing you need is an `implicit Runtime[R]` in scope when you call `graphQL(...)`. The effect will be run every time a query is executed. This allows you to leverage all the features provided by ZIO: timeouts, retries, access to ZIO environment, memoizing, etc.

### Mutations
Creating mutations is the same as queries, except you pass them as the second argument to `RootResolver`:
```scala
case class CharacterArgs(name: String)
case class Mutations(deleteCharacter: CharacterArgs => Task[Boolean])
val mutations = Mutations(???)
val interpreter = graphQL(RootResolver(queries, mutations))
```

### Subscriptions
Similarly, subscriptions are passed as the third argument to `RootResolver`:
```scala
case class Subscriptions(deleteCharacters: ZStream[Any, Nothing, Character])
val subscriptions = Subscriptions(???)
val interpreter = graphQL(RootResolver(queries, mutations, subscriptions))
```
All the fields of the subscription root case class MUST return `ZStream` objects. When a subscription request is received, an output stream of `ResponseValue` will be returned wrapped in an `ObjectValue`.

### Annotations
Caliban supports a few annotation to enrich data types:
- `@GQLName("name")` allows you to specify a different name for a data type or a field.
- `@GQLDescription("description")` lets you provide a description for a data type or field. This description will be visible when your schema is introspected.
- `@GQLDeprecated("reason")` allows deprecating a field or an enum value.

### Custom types
Caliban provides auto-derivation for common types such as `Int`, `String`, `List`, `Option`, etc. but you can also support your own types by providing an implicit instance of `caliban.schema.Schema`.

An easy way to do this is to reuse existing instances and use `contramap` to map from your type to the original type. Here's an example of creating an instance for [refined](https://github.com/fthomas/refined)'s `NonEmptyString` reusing existing instance for `String`:
```scala
import caliban.schema._
implicit val nonEmptyStringSchema: Schema[NonEmptyString] = Schema.stringSchema.contramap(_.value)
```
You can also use the `scalarSchema` helper to create your own scalar types, providing a name, an optional description, and a function from your type to a `ResponseValue`:
```scala
import caliban.schema._
implicit val unitSchema: Schema[Unit] = scalarSchema("Unit", None, _ => ObjectValue(Nil))
```

## Introspection
Introspection queries are fully supported, which means you can use your favorite tool to inspect your schema and generate documentation for free.

Here's an example of documentation generated by introspection in [Altair GraphQL Client](https://altair.sirmuel.design/):

![altair screenshot](assets/altair.png)

## Examples
A sample project showing how to serve a simple GraphQL schema over HTTP and WebSocket using [http4s](https://github.com/http4s/http4s) is available in the [examples](https://github.com/ghostdogpr/caliban/tree/master/examples/) folder.

## About

Caliban is a project developed by Pierre Ricadat aka [ghostdogpr](https://github.com/ghostdogpr). The name is inspired by the SF novel and tv series [The Expanse](https://en.wikipedia.org/wiki/Caliban%27s_War).
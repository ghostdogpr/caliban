# Getting Started

**Caliban Client** is a module independent from Caliban Server that makes it possible to write GraphQL queries using Scala code in a type-safe and functional fashion. It is built on top of [sttp](https://github.com/softwaremill/sttp), which means you can run requests using the backend of your choice.

Just like the server module, Caliban Client offers a purely functional interface and keeps the boilerplate minimal. It works as follows:
1. Use the `caliban-codegen-sbt` tool to generate boilerplate code from a given GraphQL schema
2. Write your GraphQL queries/mutations by combining helpers from the generated code
3. Transform your queries/mutations into an `sttp` request and run them with your preferred backend

## Dependencies

To use `caliban-client`, add the following line in your `build.sbt` file:

```
libraryDependencies += "com.github.ghostdogpr" %% "caliban-client" % "2.1.0"
```

Caliban-client is available for ScalaJS. To use it in a ScalaJS project, instead add this line to your `build.sbt` file:

```
libraryDependencies += "com.github.ghostdogpr" %%% "caliban-client" % "2.1.0"
```

## Code generation

Caliban provides several ways to generate the boilerplate code. To get started, we are going to generate it by running a
simple sbt command, but you can look at the [Code generation](client-codegen.md) page for more options.

You'll first need to add following dependency to your `project/plugins.sbt` file:
```scala
addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % "2.1.0")
```

And to enable the plugin in your `build.sbt` file:
```scala
enablePlugins(CalibanPlugin)
```

Then, you can run the following command in your sbt.

```bash
calibanGenClient schemaPath outputPath

# example
calibanGenClient project/schema.graphql src/main/client/Client.scala
```
`schemaPath` is the path to a GraphQL schema file (if your backend uses `caliban`, you can get it by calling `GraphQL#render` on your API).
Instead of a file, you can provide a URL and the schema will be obtained using introspection.

`outputPath` is the path where the generated code will be written. The folder needs to exist.

This command will generate a Scala file in `outputPath` containing helper functions for all the types defined in the provided GraphQL schema defined at `schemaPath`.

## Query building

Once the boilerplate code is generated, you can start building queries. For each *type* in your schema, a corresponding Scala object has been created. For each *field* in your schema, a corresponding Scala function has been created.

For example, given the following schema:
```graphql
type Character {
  name: String!
  nicknames: [String!]!
  origin: Origin!
}
```

Your generated code will have the following:
```scala
object Character {
  def name: SelectionBuilder[Character, String]            = ???
  def nicknames: SelectionBuilder[Character, List[String]] = ???
  def origin: SelectionBuilder[Character, Origin]          = ???
}
```

A `SelectionBuilder[Origin, A]` is a selection from a parent type `Origin` that returns a result of type `A`. In this example, `name` is a selection from a `Character` that returns a `String`.

You can combine multiple selections using the `~` operator. The new result type will be a tuple from the 2 combined result types. Note that you can only combine selections that have the same origin.

```scala
val selection: SelectionBuilder[Character, (String, List[String])] =
  Character.name ~ Character.nicknames
```

If you combine multiple fields, it is more convenient to have a case class to represent your data (to avoid seeing nested tuples). You can use `mapN` to map a nested tuple to a case class.

```scala
case class CharacterView(name: String, nickname: List[String], origin: Origin)

val character: SelectionBuilder[Character, CharacterView] =
  (Character.name ~ Character.nicknames ~ Character.origin)
    .mapN(CharacterView)
```

Fields that return an object type will require an inner selection, which is another `SelectionBuilder`. Let's consider the following `Query` type.

```graphql
type Query {
  characters: [Character!]!
}
```
When calling `characters`, we need to provide a `SelectionBuilder[Character, ?]` to indicate which fields to select on the returned `Character`.

```scala
val query: SelectionBuilder[RootQuery, List[CharacterView]] =
  Query.characters {
    (Character.name ~ Character.nicknames ~ Character.origin)
      .mapN(CharacterView)
  }
```

Or if we reuse the `character` selection we just created:
```scala
val query: SelectionBuilder[RootQuery, List[CharacterView]] =
  Query.characters {
    character
  }
```

Because this is Scala code, you can easily reuse a selection in multiple places without having to worry about GraphQL fragments. The Scala compiler will also make sure that you only combine fields that make sense.

When a field requires an argument, the helper method for the field will require one as well. Let's enrich our query:

```graphql
type Query {
  characters(origin: Origin!): [Character!]!
}
```

You now need to provide an `Origin` when calling `characters`:

```scala
val query: SelectionBuilder[RootQuery, List[CharacterView]] =
  Query.characters(Origin.MARS) {
    character
  }
```

## Request execution

Once your query or mutation is created, it is time to execute it. To do that, you can transform your `SelectionBuilder` into an `sttp` request by calling `.toRequest`.

This function takes the URL of your GraphQL server and some options:
- a boolean `useVariables` that determines if arguments should be using variables or not (default: false)
- an optional string `queryName` if you want to name your query (default: no name)
- a boolean `dropNullInputValues` that determines if null fields from input objects should be dropped (default: false)

You can then simply run the `sttp` request with the backend of your choice. See the [sttp docs](https://sttp.readthedocs.io/en/latest/) if you are not familiar with it.

Here is an example using the `AsyncHttpClient` backend for `ZIO`:
```scala
import sttp.client._
import sttp.client.asynchttpclient.zio.AsyncHttpClientZioBackend

AsyncHttpClientZioBackend().flatMap { implicit backend =>
  val serverUrl = uri"http://localhost:8088/api/graphql"
  val result: Task[List[CharacterView]] =
    query.toRequest(serverUrl).send().map(_.body).absolve
  ...
}
```

As a result, we get a ZIO `Task` whose return type is the same as our `SelectionBuilder`. The sttp request does not only contain the request to send, but also takes care of parsing the response into the expected type.

The [examples](https://github.com/ghostdogpr/caliban/tree/master/examples/) project contains a runnable sample code that queries the example GraphQL backend.

::: warning Limitations
Only Queries and Mutations are supported as sttp requests.
Subscriptions are supported in the laminext module, and this code can easily be adapted to other frameworks (the relevant code is only a few lines long).

Type extensions are not supported by the codegen tool.
:::

# GraphQL Client

**Caliban-client** is a module independent from Caliban that makes it possible to write GraphQL queries using Scala code in a type-safe and functional fashion. It is built on top of [sttp](https://github.com/softwaremill/sttp), which means you can run requests using the backend of your choice.

Just like Caliban, `caliban-client` offers a purely functional interface and keeps the boilerplate minimal. It works as follows:
1. Use the `caliban-codegen-sbt` tool to generate boilerplate code from a given GraphQL schema
2. Write your GraphQL query/mutation by combining helpers from the generated code
3. Transform your query/mutation into an `sttp` request and run it with your preferred backend

## Dependencies

To use `caliban-client`, add the following line in your `build.sbt` file:

```
libraryDependencies += "com.github.ghostdogpr" %% "caliban-client" % "0.9.5"
```

Caliban-client is available for ScalaJS. To use it in a ScalaJS project, instead add this line to your `build.sbt` file:

```
libraryDependencies += "com.github.ghostdogpr" %%% "caliban-client" % "0.9.5"
```

## Code generation

The first step for building GraphQL queries with `caliban-client` is to generate boilerplate code from a GraphQL schema. For that, you need a file containing your schema (if your backend uses `caliban`, you can get it by calling `GraphQL#render` on your API).

To use this feature, add the `caliban-codegen-sbt` sbt plugin to your `project/plugins.sbt` file:
```scala
addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % "0.9.5")
```

And enable it in your `build.sbt` file:
```scala
enablePlugins(CodegenPlugin)
```

Then call the `calibanGenClient` sbt command.
```scala
calibanGenClient schemaPath outputPath [--scalafmtPath path] [--headers name:value,name2:value2] [--genView true|false] [--scalarMappings gqlType:f.q.d.n.Type,gqlType2:f.q.d.n.Type2] [--imports a.b.c._,c.d.E]

calibanGenClient project/schema.graphql src/main/client/Client.scala --genView true  
```
This command will generate a Scala file in `outputPath` containing helper functions for all the types defined in the provided GraphQL schema defined at `schemaPath`.
Instead of a file, you can provide a URL and the schema will be obtained using introspection.
The generated code will be formatted with Scalafmt using the configuration defined by `--scalafmtPath` option (default: `.scalafmt.conf`).
If you provide a URL for `schemaPath`, you can provide request headers with `--headers` option.
The package of the generated code is derived from the folder of `outputPath`.
This can be overridden by providing an alternative package with the `--packageName`
option.
Provide `--genView true` option if you want to generate a view for the GraphQL types. 
If you want to force a mapping between a GraphQL type and a Scala class (such as scalars), you can use the
`--scalarMappings` option. Also you can add imports for example for your ArgEncoder implicits by providing `--imports` option.

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

## Automated generation of a view projection

`ClientWriter` can generate a view projection for a GraphQL type. 

For example, given the following schema:
```graphql
type Origin {
  description: String
  details: String  
}

type Character {
  name: String!
  nicknames: [String!]!
  origin(filter: String): Origin!
}
```

Your generated code will have the following:
```scala
type Origin
object Origin {

  final case class OriginView(description: Option[String], details: Option[String])

  type ViewSelection = SelectionBuilder[Origin, OriginView]

  def view: ViewSelection = (description ~ details).map { case (description, details) =>
    OriginView(description, details)
   }

  def description: SelectionBuilder[Origin, Option[String]] = Field("description", OptionOf(Scalar()))
  def details: SelectionBuilder[Origin, Option[String]]     = Field("details", OptionOf(Scalar()))
}

type Character
object Character {

  final case class CharacterView[OriginSelection](name: String, nicknames: List[String], origin: OriginSelection)

  type ViewSelection[OriginSelection] = SelectionBuilder[Character, CharacterView[OriginSelection]]

  def view[OriginSelection](originFilter: Option[String] = None)(
    originSelection: SelectionBuilder[Origin, OriginSelection]
  ): ViewSelection[OriginSelection] = (name ~ nicknames ~ origin(originFilter)(originSelection)).map {
    case ((name, nicknames), origin) => CharacterView(name, nicknames, origin)
  }

  def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
  def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
  def origin[A](
    filter: Option[String] = None
  )(innerSelection: SelectionBuilder[Origin, A]): SelectionBuilder[Character, A] =
    Field("origin", Obj(innerSelection), arguments = List(Argument("filter", filter)))
}
```

Then you can build a query the way you want:
```scala
val characterWithOriginAllFields: SelectionBuilder[Character, Character.CharacterView[Origin.View]] =
  Character.view(None)(Origin.view)

val characterWithOriginOnlyDetails: SelectionBuilder[Character, Character.CharacterView[String]] =
  Character.view(Some("some filter"))(Origin.details)
```

## Request execution

Once your query or mutation is created, it is time to execute it. To do that, you can transform your `SelectionBuilder` into an `sttp` request by calling `.toRequest`. This function takes the URL of your GraphQL server and an optional boolean `useVariables` that determines if arguments should be using variables or not (default: false).

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
Only Queries and Mutations are supported. Subscriptions support will be added in the future.
Type extensions are not supported by the codegen tool.
:::

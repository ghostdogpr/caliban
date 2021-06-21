# GraphQL Client

**Caliban-client** is a module independent from Caliban that makes it possible to write GraphQL queries using Scala code in a type-safe and functional fashion. It is built on top of [sttp](https://github.com/softwaremill/sttp), which means you can run requests using the backend of your choice.

Just like Caliban, `caliban-client` offers a purely functional interface and keeps the boilerplate minimal. It works as follows:
1. Use the `caliban-codegen-sbt` tool to generate boilerplate code from a given GraphQL schema
2. Write your GraphQL query/mutation by combining helpers from the generated code
3. Transform your query/mutation into an `sttp` request and run it with your preferred backend

## Dependencies

To use `caliban-client`, add the following line in your `build.sbt` file:

```
libraryDependencies += "com.github.ghostdogpr" %% "caliban-client" % "1.0.1"
```

Caliban-client is available for ScalaJS. To use it in a ScalaJS project, instead add this line to your `build.sbt` file:

```
libraryDependencies += "com.github.ghostdogpr" %%% "caliban-client" % "1.0.1"
```

## Code generation

The first step for building GraphQL queries with `caliban-client` is to generate boilerplate code from a GraphQL schema. For that, you need a file containing your schema (if your backend uses `caliban`, you can get it by calling `GraphQL#render` on your API).

To use this feature, add the `caliban-codegen-sbt` sbt plugin to your `project/plugins.sbt` file:
```scala
addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % "1.0.1")
```

And enable it in your `build.sbt` file:
```scala
enablePlugins(CalibanPlugin)
```

### File generation

At this point, the `caliban` command will cause any files in `src/main/graphql` to be translated into a Caliban-generated client library. This happens automatically any time you `compile`.

By default, all clients are generated with the same client name as the source file, in the `caliban` top-level package.

In order to supply more configuration options to the code generator, you can use the `calibanSettings` sbt setting, combined with the `calibanSetting` function to scope the settings to a particular file:
```scala
      // The `file("Service.graphql")` is a path suffix for some file in `src/main/graphql`
      Compile / caliban / calibanSettings += calibanSetting(file("Service.graphql"))(
        cs =>
          cs.packageName("com.example.graphql.client")
            .scalarMapping(
              "LanguageCode" -> "com.example.models.LanguageCode",
            )
            .scalarMapping(
              "Timestamp" -> "java.sql.Timestamp",
              "DayOfWeek" -> "java.time.DayOfWeek",
              "IntRange"  -> "com.github.tminglei.slickpg.Range[Int]"
            )
            .imports("com.example.graphql.client.implicits._")
      )
```

### URL generation

The `calibanSetting` function also permits generating clients for supplied `url`'s:
```scala
      Compile / caliban / calibanSettings += calibanSetting(url("http://my-example-service/graphql"))(
        cs =>
          cs.clientName("ExampleServiceClient")
            .packageName("com.example.graphql.client")
      )
```

### `calibanSetting` config parameters

The settings available on the `cs` (`CalibanSettings`) builder are:
```
  def scalafmtPath(path: String): CalibanSettings
  def packageName(name: String): CalibanSettings
  def genView(value: Boolean): CalibanSettings
  def effect(tpe: String): CalibanSettings
  def scalarMapping(mapping: (String,String)*): CalibanSettings
  def imports(values: String*): CalibanSettings

  // Only defined for `url` settings, for use in supplying extra headers when fetching the schema itself
  def headers(pairs: (String,String)*): CalibanSettings
```

### `calibanGenClient`

Should you prefer or otherwise need the previous style of manual client codegen, supplied by the `calibanGenClient` function, that command is still available. Documentation for that command is as follows:

```scala
calibanGenClient schemaPath outputPath [--scalafmtPath path] [--headers name:value,name2:value2] [--genView true|false] [--scalarMappings gqlType:f.q.d.n.Type,gqlType2:f.q.d.n.Type2] [--imports a.b.c._,c.d.E]

calibanGenClient project/schema.graphql src/main/client/Client.scala --genView true  
```
This command will generate a Scala file in `outputPath` containing helper functions for all the types defined in the provided GraphQL schema defined at `schemaPath`.
If you need to disable generating clients from `src/main/graphql`, please include `Compile / caliban / calibanGenerator := Seq.empty` in your project settings.
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

## Laminext Integration

If you are using the Scala.js framework [Laminar](https://laminar.dev), there is a module that makes the integration even nicer, with support for subscriptions.
It is depending on [Laminext](https://laminext.dev), a library that provides nice little helpers for Laminar, in particular for using `Fetch` and `WebSocket`.

To use it, import the `caliban-client-laminext` module:
```
libraryDependencies += "com.github.ghostdogpr" %%% "caliban-client-laminext" % "1.0.1"
```

Add the following import to your code:
```scala
import caliban.client.laminext._
```

This import adds an extension method `toEventStream(uri)` to `SelectionBuilder`, which is similar to `toRequest` except it creates an `EventStream` instead of an sttp `Request`.

```scala
val characters: Var[List[String]] = Var(Nil)

val uri = "http://localhost:8088/api/graphql"

val getCharacters = Queries.characters(None)(Client.Character.name).toEventStream(uri)

val view: Div = 
  div(
    "Characters: ",
    getCharacters.collectRight --> characters.set _,
    child <-- characters.signal.map(c => div(c.mkString(", ")))
  )
```

To use subscriptions, you first need to create a `WebSocket` with protocol `graphql-ws`. Use the extension method `.graphql` instead of `.text` or `.json`.
Then use the extension method `toSubscription` on your `SelectionBuilder` and pass the `WebSocket` object.
```scala
val ws = WebSocket.url("ws://localhost:8088/ws/graphql", "graphql-ws").graphql.build()

val deletedCharacters = Subscriptions.characterDeleted.toSubscription(ws)
```

Finally, you can use `ws.connect` to connect the `WebSocket`, `ws.init()` to initialize the communication with the graphql server and `.received` to get an `EventStream` of the type returned by your subscription.
```scala
ws.connect,
ws.connected --> (_ => ws.init()),
deletedCharacters.received.collectRight --> 
  (name => characters.update(_.filterNot(_ == name))),
```

There is a full example in the `test` folder of the `caliban-client-laminext` module.
To use it:
- run `ExampleApp` of the http4s server example (it supports CORS)
- run `clientLaminextJS/Test/fastLinkJS` to compile the Scala.js code
- run `yarn install` and `yarn exec vite` in the `caliban-client-laminext` folder
- the example page will be running on [http://localhost:3000](http://localhost:3000)

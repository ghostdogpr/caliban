# Stitching

**Stitching** is a part of `caliban-tools` which can be used to use parts of another GraphQL API from an API you're building in situations where using Apollo federation isn't an option. You can also use it to fully subsume and proxy another GraphQL schema.

In general, Federation should be your preferred choice.

You should also be careful when using stitching since it's very easy to pull in large parts of an external schema's types and structure into your API. This can make it error prone since the likelihood that you'll get type clashes between your API and the API you're stitching in quickly increases.

## Dependencies

In order to use stitching, add `caliban-tools` to your dependencies:

```scala
libraryDependencies += "com.github.ghostdogpr" %% "caliban-tools" % "1.3.3"
```

## Stitching in Action

Let's start out by defining our API. We'll have `AppUser` profiles, that has a linked `featuredRepository`. For the `featuredRepository`, we want to leverage [Github's GraphQL API](https://docs.github.com/en/graphql).

```scala mdoc:silent
import caliban._
import caliban.schema._
import zio._

object StitchingExample extends GenericSchema[ZEnv] {
  case class AppUser(id: String, name: String, featuredRepository: Repository)
  case class Repository(owner: String, name: String)

  case class GetUserQuery(name: String, repository: String)

  case class Queries(
    GetUser: GetUserQuery => URIO[ZEnv, AppUser]
  )

  val graphQL: GraphQL[ZEnv] = GraphQL.graphQL(
    RootResolver(
      Queries(
        GetUser = query =>
          Random.nextUUID.map(uuid =>
            AppUser(
              id = uuid.toString,
              name = query.name,
              featuredRepository = Repository(query.name, query.repository)
            )
          )
      )
    )
  )
}
```

Now let's integrate with the Github API!

In order to do this we're going to do a couple of things:

1. Load the introspection schema from Github's API
1. Parse the introspected schema into a `caliban.introspection.adt.__Schema`
1. Use the parsed schema to generate an `implicit Schema[R, A]` for the entities we're stitching. This effectively replaces our own schema with one from Github.
1. Teach our implicit schema how to map our local resolver to a query that can be resolved remotely by calling Github's API.


```scala
  val GITHUB_API = "https://api.github.com/graphql"

  val api = for {
    sttpClient           <- ZIO.environment[SttpClient]
    // 1
    schemaLoader          = SchemaLoader.fromIntrospection(GITHUB_API, None)
    schema               <- schemaLoader.load
    // 2
    remoteSchema         <- ZIO.fromOption(RemoteSchema.parseRemoteSchema(schema))
    remoteSchemaResolvers = RemoteSchemaResolver.fromSchema(remoteSchema)
  } yield {
    // 3
    implicit val githubProfileSchema: Schema[ZEnv, Repository] =
      remoteSchemaResolvers
        .remoteResolver("Repository")(
          // 4
          // Here we need to translate our local `Repository` case class into
          // a top-level query which can be issued towards Github's API.
          // We do this by accepting a `caliban.execution.Field`, representing
          // all the selected fields for a repository and map that to the
          // top-level `repository` query in the Github API.
          // This means the final query will end up looking something like this:
          // query {
          //   repository(owner: r.args.owner, name: r.args.name) {
          // .   <incoming query>
          //   }
          // }
          RemoteResolver.fromFunction((r: ResolveRequest[Repository]) =>
            r.field.copy(
              name = "repository",
              arguments = Map(
                "owner" -> Value.StringValue(r.args.owner),
                "name"  -> Value.StringValue(r.args.name)
              )
            )
          ) >>> RemoteResolver.fromUrl(GITHUB_API)
        )
        .provide(sttpClient)
  }
```

However, when running this we will experience failing requests due to `401 Unauthorized`. This is because all queries to Github's API requires authorization to be provided. In order to fix this, we need to add authorization to both the introspection query as well as our remote resolver. We also need a config module that can provide us with a Github token based on the value of `GITHUB_TOKEN` in our environment.

```scala
case class Configuration(githubToken: String)

object Configuration {
  def fromEnvironment =
    (for {
      githubToken <- read("GITHUB_TOKEN")
    } yield Configuration(githubToken)).toLayer

  private def read(key: String): Task[String] = Task.effect(
    sys.env(key)
  )
}
```

We can now update the introspection query to use our token:

```scala
  val api = for {
    config     <- ZIO.environment[Has[Configuration]]
    schemaLoader = SchemaLoader.fromIntrospection(
                     GITHUB_API,
                     Some(
                       List(
                         Options.Header(
                           "Authorization",
                           s"Bearer ${config.get.githubToken}"
                         )
                       )
                     )
                   )
    // ...
  }  yield ???
```

as well as update our resolver to authorize our request.

In order to do this, we can use `RemoteResolver[R, E, A, B]` which lets us compose resolution steps via `>>>`.

In order to make this code easier, we can extract the mechanics around sending the actual request:

```scala
val apiRequest =
    RemoteResolver.toQuery >>> RemoteResolver.request(GITHUB_API) >>> RemoteResolver.fromFunctionM((r: HttpRequest) =>
    for {
        config <- ZIO.service[Configuration]
    } yield r.header("Authorization", s"Bearer ${config.githubToken}")
    ) >>> RemoteResolver.execute >>> RemoteResolver.unwrap
```

And now we can use our new `apiRequest` when resolving our `Schema[ZEnv, Repository]`:

```scala
implicit val githubProfileSchema: Schema[ZEnv, Repository] =
  remoteSchemaResolvers
  .remoteResolver("Repository")(
      RemoteResolver.fromFunction((r: ResolveRequest[Repository]) =>
      r.field.copy(
          name = "repository",
          arguments = Map(
            "owner" -> Value.StringValue(r.args.owner),
            "name"  -> Value.StringValue(r.args.name)
          )
        )
      ) >>> apiRequest
  )
  .provide(sttpClient ++ config)
```

All that's left to do is to hook up to an HTTP server and configure a Github API token. And now you have an API that can handle queries such as these:


```graphql
query {
  GetUser(name:"ghostdogpr", repository:"caliban") {
    id
    name
    featuredRepository {
      pullRequests(states: OPEN, first: 10) {
        edges {
          node {
            title
            author {
              login
            }
          }
        }
      }
    }
  }
}
```

See the [examples directory](https://github.com/ghostdogpr/caliban/tree/master/examples/src/main/scala/example/stitching) for a full example.

## Things not yet supported
- Type renaming.
- Type conflict resolution strategies.

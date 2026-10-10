# Usage Reporting

The `caliban-hive` module reports which operations your API serves to [GraphQL Hive](https://the-guild.dev/graphql/hive). Hive uses these reports to show which types and fields clients use, and to flag schema changes that would break them.

```scala
libraryDependencies += "com.github.ghostdogpr" %% "caliban-hive" % "3.1.5"
```

## Reporting from an interpreter

Wrap your API with `HiveUsage.wrapper` and provide a `HiveUsage` layer. The layer needs a `zio.http.Client` to send its reports:

```scala mdoc:compile-only
import caliban._
import caliban.hive.{ HiveConfig, HiveUsage }
import caliban.schema.Schema.auto._
import zio._
import zio.http.Client

case class Queries(hello: String)

val api = graphQL(RootResolver(Queries("world")))

val config = HiveConfig(
  token = Config.Secret("<access token>"),
  target = "my-organization/my-project/production"
)

val serve: ZIO[Any, Throwable, Unit] =
  ZIO.scoped {
    for {
      interpreter <- (api @@ HiveUsage.wrapper).interpreter
      _           <- QuickAdapter(interpreter).runServer(8080, "/graphql")
    } yield ()
  }.provide(HiveUsage.layer(config), Client.default)
```

The token needs permission to report usage for the target. `target` is the target's ID, or its organization, project and target slugs separated by `/`. A self-hosted Hive serves the usage API under `/usage`; pass it as `endpoint`.

To report from a Caliban gateway, attach `GatewayHive.hooks` instead, as described in [Gateway hooks](gateway/hooks.md#reporting-usage-to-graphql-hive).

## What is reported

Each operation is reported with its text, its name, the schema coordinates it used (such as `Query.user`, `Query.user.id` and `Role.ADMIN`), its duration and its number of errors. Before it leaves the server, the text is normalized like Hive's own clients do it: string, int and float values are replaced, aliases are removed and selections are sorted. Values written into a query, such as IDs or search terms, are never sent. Variables are not sent either, but their values count toward the coordinates.

If a request carries `x-graphql-client-name` and `x-graphql-client-version` headers, Hive groups its usage by that client. `QuickAdapter`, the tapir-based adapters and the gateway pass request headers on; with other integrations, operations are reported without a client. Operations that fail before execution, such as validation errors, are not reported. For `@defer` and `@stream`, the duration ends with the initial payload.

## Configuration

`HiveConfig` also controls which operations are reported and when:

| Setting         | Default                                | Description                                                          |
| --------------- | -------------------------------------- | -------------------------------------------------------------------- |
| `endpoint`      | `https://app.graphql-hive.com/usage`   | The usage API                                                        |
| `sampleRate`    | `1.0`                                  | The share of operations to report, from 0 to 1                       |
| `exclude`       | empty                                  | Names of operations that are never reported, such as health checks   |
| `flushInterval` | 5 seconds                              | How often collected operations are sent                              |
| `maxBatchSize`  | 1000                                   | The most operations sent in one report                               |
| `bufferSize`    | 10000                                  | The most operations kept while waiting; further ones are dropped     |
| `timeout`       | 10 seconds                             | How long one report may take, and how long shutdown waits for the last |

Reporting never fails or slows down a request. Operations are sent in the background, and once more when the layer is released. A report that fails because of the connection, a timeout, a server error or rate limiting is retried twice. A report Hive rejects, for example because of a wrong token, is logged as a warning the first time and at debug level afterwards. Dropped operations are logged with their count.

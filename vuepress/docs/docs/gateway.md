# Getting Started

Caliban Gateway gives clients one GraphQL endpoint backed by multiple services. It can combine ordinary GraphQL services, Apollo Federation subgraphs, and in-process Caliban APIs.

It is an alternative to [Apollo Router](https://www.apollographql.com/docs/graphos/routing/get-started), [Hive Router](https://the-guild.dev/graphql/hive/router), and [Cosmo Router](https://wundergraph.com/blog/an-intro-to-cosmo-router), with configuration and customization in Scala.

Define your subgraphs with `Gateway.compose`, or load an existing supergraph with `Gateway.fromSupergraph`. Serve the combined API with `QuickAdapter`.

## Installation

Add the gateway and an HTTP adapter to `build.sbt`:

```scala
libraryDependencies ++= Seq(
  "com.github.ghostdogpr" %% "caliban-gateway" % "3.1.5",
  "com.github.ghostdogpr" %% "caliban-quick"   % "3.1.5"
)
```

## Your first gateway

This example combines the products and reviews services from the Caliban repository. Clone the repository, then start each service in a separate terminal from its root directory:

```sh
git clone --branch series/3.x https://github.com/ghostdogpr/caliban.git
cd caliban
```

```sh
sbt "gatewayExamples/runMain example.gateway.ProductsApi"
```

```sh
sbt "gatewayExamples/runMain example.gateway.ReviewsApi"
```

The services listen on ports 8081 and 8082. In your own sbt project, add the dependencies above and save the following as `src/main/scala/Main.scala`:

```scala
import caliban.QuickAdapter
import caliban.gateway.{ Gateway, Subgraph }
import zio._
import zio.http._

object Main extends ZIOAppDefault {
  private val products = Subgraph.graphql(
    "products",
    url"http://localhost:8081/graphql"
  )

  private val reviews = Subgraph.graphql(
    "reviews",
    url"http://localhost:8082/graphql"
  )

  private val gateway = Gateway.compose(products, reviews)

  def run =
    for {
      interpreter <- gateway.interpreter
      _           <- QuickAdapter(interpreter).runServer(
                       port = 4000,
                       apiPath = "/graphql",
                       graphiqlPath = Some("/graphiql")
                     )
    } yield ()
}
```

At startup, the gateway loads and combines both schemas. If composition fails, the application exits and identifies the subgraph or schema that caused the error.

## Run and query the gateway

With both services running, start your gateway from your own project's directory:

```sh
sbt "runMain Main"
```

Open `http://localhost:4000/graphiql`, or send a query from another terminal:

```sh
curl http://localhost:4000/graphql \
  -H 'Content-Type: application/json' \
  --data '{"query":"{ product(id: \"caliban\") { name } latestReviews { body } }"}'
```

The response combines data from both services:

```json
{
  "data": {
    "product": { "name": "Caliban" },
    "latestReviews": [
      { "body": "Composable and type-safe" },
      { "body": "Structured concurrency all the way down" }
    ]
  }
}
```

These root fields are independent, so they need no lookup configuration. To fetch reviews inside `product { ... }`, see [Connecting objects across ordinary services](gateway/planning.md#connecting-objects-across-ordinary-services).

For a complete project, see the repository's [gateway examples](https://github.com/ghostdogpr/caliban/tree/series/3.x/gateway-examples). They cover local, ordinary remote, mixed-subgraph, and Federation gateways.

## Interpreter lifetime

Build `gateway.interpreter` once at application startup and share it across requests. It acquires resources in the surrounding ZIO `Scope`. Keep that scope open while the HTTP server runs, as the `ZIOAppDefault` example does. Closing it shuts down the gateway and drains active requests.

Use [`.reloadable`](gateway/configuration.md#hot-reload) when schemas must refresh without restarting the server.

A gateway interpreter works with the existing [HTTP adapters](adapters.md). Use `QuickAdapter(interpreter).routes(...)` or `.handlers` to add it to an existing zio-http server.

## Next steps

- [Add subgraphs](gateway/subgraphs.md) from remote services, local Caliban APIs, or a supergraph.
- [Connect objects across services](gateway/planning.md#connecting-objects-across-ordinary-services) when one query needs fields from several services for the same object.
- [Add subscriptions](gateway/subscriptions.md) from local or remote sources.
- [Attach hooks](gateway/hooks.md) for document resolution, authorization, headers, metrics, and tracing.
- [Configure the gateway](gateway/configuration.md) for authentication, timeouts, retries, limits, and schema reloads.

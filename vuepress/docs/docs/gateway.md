# Gateway

Caliban Gateway gives clients one GraphQL endpoint backed by multiple GraphQL services. A gateway can combine:

- ordinary GraphQL services
- Apollo Federation subgraphs
- in-process Caliban APIs

You describe the subgraphs, combine them with `Gateway.compose`, and serve the result with `QuickAdapter`.

::: tip
If all your services use Apollo Federation, start with [Federation subgraphs](#federation-subgraphs). For ordinary GraphQL services, start with [Ordinary GraphQL services](#ordinary-graphql-services). Add a lookup only when one query needs fields for the same object from more than one service.
:::

## Installation

Add the gateway and an HTTP adapter to `build.sbt`:

```scala
libraryDependencies ++= Seq(
  "com.github.ghostdogpr" %% "caliban-gateway" % "3.1.5",
  "com.github.ghostdogpr" %% "caliban-quick"   % "3.1.5"
)
```

For OpenTelemetry tracing, also add:

```scala
libraryDependencies += "com.github.ghostdogpr" %% "caliban-gateway-tracing" % "3.1.5"
```

## Your first gateway

The following application combines two remote GraphQL services and exposes the result at `/graphql`:

```scala
import caliban.QuickAdapter
import caliban.gateway.{ Gateway, Subgraph }
import sttp.client4.UriContext
import zio._

object Main extends ZIOAppDefault {
  private val products = Subgraph.graphql(
    "products",
    uri"http://products:8080/graphql"
  )

  private val reviews = Subgraph.graphql(
    "reviews",
    uri"http://reviews:8080/graphql"
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

At startup, the gateway loads and combines both schemas. If that fails, the application exits with messages explaining which subgraph or schema caused the problem.

## Adding subgraphs

Give every subgraph a unique name such as `products` or `reviews`. Caliban uses it in error messages and monitoring data.

### Ordinary GraphQL services

Use `Subgraph.graphql` for a regular GraphQL endpoint:

```scala
val catalog = Subgraph.graphql(
  "catalog",
  uri"http://catalog:8080/graphql"
)
```

By default, the gateway acquires the schema through introspection. You can instead provide SDL directly:

```scala
val catalog = Subgraph.graphql(
  "catalog",
  uri"http://catalog:8080/graphql",
  """
    type Query {
      product(id: ID!): Product
    }

    type Product {
      id: ID!
      name: String!
    }
  """
)
```

If you omit the SDL, the gateway loads it from the service at startup. Providing SDL directly is useful when the schema endpoint is unavailable from your deployment environment. Keep provided SDL in sync with the deployed service.

### Federation subgraphs

Use `Subgraph.federation` for an Apollo Federation subgraph:

```scala
val products = Subgraph.federation(
  "products",
  uri"http://products:8080/graphql"
)

val reviews = Subgraph.federation(
  "reviews",
  uri"http://reviews:8080/graphql"
)

val gateway = Gateway.compose(products, reviews)
```

The gateway acquires each Federation schema through `_service`. You may also pass pinned Federation SDL as the third argument to `Subgraph.federation`.

The gateway reads entity information from the Federation schemas. You do not need to add the `Lookup` configuration described in the next section.

### In-process Caliban APIs

Use `Subgraph.local` to include a Caliban API without an HTTP call:

```scala
import caliban._
import caliban.gateway.Subgraph
import caliban.schema.GenericSchema

object LocalApi extends GenericSchema[Any] {
  import auto._

  final case class Query(gatewayVersion: String)

  val api = graphQL(RootResolver(Query("v1")))
}

val local = Subgraph.local("gateway", LocalApi.api)
```

Remote, Federation, and local subgraphs can be used together in the same `Gateway.compose` call.

## Connecting objects across ordinary services

Federation schemas already explain how to fetch an entity from another service. With ordinary GraphQL services, you provide that information using a `Lookup`.

Suppose the catalog service returns a `Product`, while the reviews service adds `reviews` to `Product` and exposes this batch field:

```graphql
type Query {
  productsByIds(ids: [ID!]!): [Product!]!
}

type Product {
  id: ID!
  reviews: [Review!]!
}
```

Describe how the reviews service recalls products:

```scala
import caliban.gateway.Lookup

val reviews = Subgraph
  .graphql("reviews", uri"http://reviews:8080/graphql", reviewsSdl)
  .withLookup(
    Lookup.list(
      "Product",
      List("id"),
      "productsByIds",
      Lookup.Correlation.byKey(Map("id" -> "id")),
      "ids" -> Lookup.Argument.batch(Lookup.Argument.key("id"))
    )
  )
```

Use `Lookup.single` when the subgraph fetches one object at a time. Use `Lookup.list` when it accepts several keys in one request:

- `Correlation.byKey` matches returned objects using a field such as `id`
- `Correlation.ordered` matches results by their position in the returned list
- `Argument.key("id")` reads the `id` from the object being fetched
- `Argument.obj(...)` builds an input object expected by the subgraph
- `Argument.batch(...)` builds one argument value for each requested object

Prefer a batch lookup when the subgraph supports it. It lets the gateway recall several objects with one subgraph request.

## Configuring remote services

Use `RemoteGraphQLConfig` when a remote service needs custom timeouts, retries, concurrency, headers, or body-size limits.

```scala
import caliban.gateway.RemoteGraphQLConfig
import sttp.model.Header
import zio._

val remoteConfig = RemoteGraphQLConfig.default
  .withExecution(
    _.withTimeout(10.seconds)
      .withMaxConcurrentCalls(64)
      .withRetries(2, 100.millis)
  )

val products = Subgraph.graphql(
  "products",
  uri"http://products:8080/graphql",
  remoteConfig
)
```

The configuration belongs to that subgraph only. You can configure each remote service differently.

The gateway only retries requests that are safe to repeat.

If loading the schema at startup requires authentication, configure its headers separately:

```scala
val remoteConfig = RemoteGraphQLConfig.default.withAcquisition(
  _.withTimeout(5.seconds)
    .withHeaders(Header("X-Schema-Token", "schema-secret"))
)
```

These headers are used only while the gateway starts. The headers in the next section are sent with client queries and mutations.

### Authentication and request headers

To send the same credentials with every request to a service:

```scala
val config = RemoteGraphQLConfig.default.withExecution(
  _.withHeaders(Header("Authorization", "Bearer service-token"))
)
```

If the token must be loaded or refreshed dynamically, use `withExecutionHeadersZIO`:

```scala
val loadToken: Task[String] = ???

val config = RemoteGraphQLConfig.default.withExecutionHeadersZIO(
  loadToken.map(token => List(Header("Authorization", s"Bearer $token")))
)
```

Forward selected client headers by name:

```scala
val config = RemoteGraphQLConfig.default.withExecution(
  _.forwardIncomingHeaders("Authorization", "X-Request-ID")
)
```

Header names are matched case-insensitively. Prefer an explicit allowlist; use `forwardAllIncomingHeaders` only when every incoming header is safe to send to the subgraph.

`QuickAdapter` handles forwarded headers automatically. If you build your own HTTP integration, pass the incoming headers with `interpreter.executeRequest(request, headers)`.

## Shaping the public schema

Transform a subgraph before composition when its source names should not appear in the public schema:

```scala
import caliban.gateway.SchemaTransformation

val reviews = Subgraph
  .graphql("reviews", uri"http://reviews:8080/graphql", reviewsSdl)
  .transform(
    SchemaTransformation.renameField("Product", "reviews", "customerReviews"),
    SchemaTransformation.hideField("Product", "internalScore"),
    SchemaTransformation.renameEnumValue("ReviewStatus", "WAITING", "PENDING")
  )
```

Clients use the new names, while the remote service continues to receive its original names.

You can rename or hide types, fields, optional arguments, input fields, and enum values. Invalid or conflicting changes are reported when the gateway starts.

## Gateway limits and shutdown

Configure limits shared by the whole gateway with `withConfig`:

```scala
val gateway = Gateway
  .compose(products, reviews)
  .withConfig(
    _.withMaxConcurrentRequests(256)
      .withRequestTimeout(10.seconds)
      .withDrainTimeout(20.seconds)
  )
```

The defaults are suitable for getting started. The most common settings are:

- `withMaxConcurrentRequests` for how many requests the gateway handles at once
- `withRequestTimeout` for the maximum duration of a client request
- `withDrainTimeout` for the time allowed to finish requests during shutdown
- `withMaxConcurrentLocalCalls` if the gateway includes an in-process Caliban API

Leave the other limits at their defaults unless you have a specific reason to change them.

QuickAdapter has separate HTTP body limits. Configure them on the adapter when necessary:

```scala
QuickAdapter(interpreter)
  .withMaxRequestBodyBytes(2 * 1024 * 1024)
  .withMaxUploadBodyBytes(32 * 1024 * 1024)
  .withMaxResponseBodyBytes(16 * 1024 * 1024)
  .runServer(4000, "/graphql")
```

## Authorizing operations

Add an `OperationPolicy` when the gateway must allow or reject a complete operation before running it:

```scala
import caliban.gateway.OperationPolicy
import zio.ZIO

val isAllowed: OperationPolicy.ValidatedOperation => Boolean = ???

val policy = OperationPolicy[Any] { operation =>
  ZIO.succeed {
    if (isAllowed(operation)) OperationPolicy.Allow
    else OperationPolicy.Reject("Operation is not authorized.")
  }
}

val secured = Gateway
  .compose(products, reviews)
  .withOperationPolicy(policy)
```

The policy can inspect the request and the fields it selects. Federation directives such as `@authenticated`, `@requiresScopes`, and `@policy` are available through `operation.securityRequirements`.

If your Federation schemas use those security directives, you must install an `OperationPolicy`. The gateway will refuse to start until you do.

Use `OperationPolicy.Reject()` for a generic public message. Only pass an explicit reason when it is safe to return that reason to the client.

## Introspection and remote errors

The gateway exposes the composed schema through normal GraphQL introspection. To disable client introspection at the QuickAdapter boundary:

```scala
import caliban.ExecutionConfiguration

QuickAdapter(interpreter)
  .configure(ExecutionConfiguration(enableIntrospection = false))
  .runServer(4000, "/graphql")
```

This controls whether clients can inspect the combined schema. It does not prevent the gateway from loading remote schemas at startup.

Remote GraphQL error messages are redacted by default, and only the `code` extension is retained. If a trusted service returns details that clients should see, opt in for that subgraph:

```scala
val config = RemoteGraphQLConfig.default.withErrorDisclosure(
  _.withMessages(true)
    .withAdditionalExtensionKeys("requestId")
)
```

Only enable extra error details for services you trust.

## Inspecting and operating the gateway

### Explain a query plan

Use `explain` to see which subgraphs a query will call without executing it:

```scala
val plan = interpreter.explain("""
  query {
    product(id: "p1") {
      name
      customerReviews { body }
    }
  }
""")
```

A plan for this query could look like:

```text
query
fetch products at $.product fields [name, id (key)]
fetch reviews after products at $.product via Product(id) fields [customerReviews.body]
```

Each `fetch` is followed by the name given to the subgraph. Here, `products` and `reviews` are the names passed to `Subgraph.graphql`. `$` is the root of the client response, so `$.product` refers to its `product` field. A field marked `(key)` was added by the gateway for a later lookup, and `after products` means that fetch depends on the result from `products`.

This is useful when testing a new lookup or checking why a field is being sent to a particular service.

### Check interpreter status

`interpreter.status` shows whether the gateway is running and summarizes active requests, remote-service usage, and operation-cache activity. You can expose the fields you need through a health or diagnostics endpoint.

### Metrics and tracing

Metrics are opt-in:

```scala
import caliban.gateway.{ Gateway, GatewayMetrics }

val gateway = Gateway.compose(products, reviews) @@ GatewayMetrics.wrapper
```

The built-in metrics cover requests, routing, remote calls, retries, concurrency, cache activity, and request or response sizes.

Add OpenTelemetry tracing with the optional tracing module:

```scala
import caliban.gateway.GatewayMetrics
import caliban.gateway.tracing.GatewayTracing

val gateway = Gateway.compose(products, reviews) @@
  (GatewayMetrics.wrapper |+| GatewayTracing.wrapper)
```

The tracing wrapper creates spans for gateway requests and remote calls. `QuickAdapter` propagates incoming trace headers automatically.

## Current protocol support

The gateway currently routes queries and mutations. It does not route subscriptions or incremental delivery through subgraphs.

Complete runnable examples are available in the repository's [`gateway-examples`](https://github.com/ghostdogpr/caliban/tree/series/3.x/gateway-examples) project, including local, ordinary remote, mixed-subgraph, and Federation gateways.

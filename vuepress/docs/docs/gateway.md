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

## Hot reload

Use `gateway.reloadable` instead of `gateway.interpreter` to refresh acquired remote schemas without replacing your HTTP adapter:

```scala
import caliban.gateway.GatewayReloadConfig
import zio._

val reloadConfig = GatewayReloadConfig.default
  .withPollInterval(30.seconds)
  .withJitter(0.2)

for {
  interpreter <- gateway.reloadable(reloadConfig)
  _           <- QuickAdapter(interpreter).runServer(4000, "/graphql")
} yield ()
```

Startup still requires a valid initial interpreter. After startup, the gateway polls ordinary acquired schemas through introspection and Federation schemas through `_service`. Pinned SDL, parsed documents, local graphs, endpoints, and configuration stay fixed. At least one subgraph must use an acquired schema.

Every refresh collects all acquired schemas using their configured acquisition timeouts and body-size limits. One acquisition HTTP client is reused for the reloadable interpreter's lifetime, separately from each generation's execution client. A failed collection leaves the current generation untouched. Parsed schema content is compared with the active snapshot, ignoring whitespace, comments, source locations, and the order of definitions, fields, arguments, enum values, union members, and implemented interfaces. Descriptions, directive application order, and input list-value order are retained. Only the fingerprint is canonicalized; the documents used for composition are unchanged. Unchanged checks preserve the interpreter and its warm operation cache.

When content changes, a candidate is built from those exact documents without fetching again. Candidates must pass the usual build validation; there is no additional breaking-change check. Request admission uses atomic references without a shared lock. If a selected generation has already stopped accepting work, selection is retried before any request work starts. A lease reserved just after publication but before the old generation starts draining still belongs to that old generation. Already admitted operations finish on their original generation; operation resolution and execution are never retried to hide a replacement race. Shutdown is checked again after reservation; a reservation that observes closed admission is released without executing the request.

The default interval is 30 seconds with up to twenty percent jitter in either direction. This delay starts after a refresh cycle, including retirement, finishes. Cycles never overlap, so the interval is not a schema-freshness guarantee. Acquisition or construction failures keep the active interpreter serving and are retried in the next cycle.

### Draining and resource limits

The old interpreter drains using `GatewayConfig.withDrainTimeout`. There are at most two interpreter generations: active plus candidate during construction, or active plus retiring after publication. Further refreshes wait for retirement to finish.

Retirement does not impose a minimum delay: an idle generation closes immediately. A stuck request can delay the next schema check by the full drain timeout (30 seconds by default), followed by the configured polling delay. Uninterruptible work can postpone it indefinitely. If schema updates appear to have stopped, check the reload phase and retiring generation: `Draining` means polling is waiting for that generation, even while the current generation continues serving. `retirementOverdue` and the warning log identify retirement that has exceeded its timeout.

**Concurrency limits remain per interpreter.** During overlap, total request and subgraph concurrency can reach twice the configured limits, and each generation has its own operation cache. The drain timeout requests interruption; uninterruptible work can hold the old generation indefinitely. In that case, the active generation continues serving and further refreshes remain paused.

Closing the owning scope stops request admission and further publication, cancels refresh work, and closes all owned scopes. Active and retiring generations drain concurrently, without resetting the old generation's deadline. Scope closure still waits for uninterruptible work to finish.

### Monitoring reloads

`interpreter.reloadStatus` reports the active and retiring generation identifiers, their individual interpreter status, refresh phase, activation time, last attempt, last successful check, latest failure, and overdue retirement. A successful unchanged check advances the successful-check timestamp without changing the activation time. Failed candidates do not advance that timestamp.

`interpreter.status` aggregates lifecycle counts and admission usage across active and retiring generations, and reports the active generation's operation cache. Summed admission limits describe separate per-interpreter budgets, not a shared pool. Check each generation through `reloadStatus` for the breakdown.

Keep reload health separate from serving readiness: failed refreshes do not evict an otherwise serving generation. Monitor `lastFailure`, the age of `lastSuccessfulCheckAt`, and `retirementOverdue` through your application's diagnostics or monitoring integration. Failure details are bounded summaries with affected subgraph names; raw schemas, remote messages, response bodies, and exception causes are not retained.

The gateway logs activation, failure, recovery, and overdue retirement, suppressing identical repeated failure logs. Hot reload does not add administrative HTTP endpoints, manual refresh, or new metrics and tracing interfaces.

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

Retries are disabled by default. When enabled, the gateway only retries requests that are safe to repeat.

Concurrent identical remote queries share one in-flight call by default. The request body and semantic outbound headers must match; mutations are never shared. Disable this per subgraph with `.withExecution(_.withInFlightQueryDeduplication(false))`.

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

## Persisted and trusted documents

Normally, clients send the full GraphQL query with each request. With **persisted documents**, they send an ID instead, and the server looks up the query text. Accepting only documents registered by your application also lets you restrict which queries clients can run; these are **trusted documents**.

`OperationResolver` is the gateway hook for this lookup: it takes an incoming request and supplies the query text to parse, validate, and execute. Use `trustedDocuments` for an in-memory registry:

```scala
import caliban.Value.StringValue
import caliban.gateway.{ Gateway, OperationResolver }

val documents = Map(
  "product-v1" -> "query Product($id: ID!) { product(id: $id) { name } }"
)

val resolver = OperationResolver.trustedDocuments(documents) { request =>
  request.extensions.flatMap(_.get("documentId")).collect {
    case StringValue(id) => id
  }
}

val gateway = Gateway
  .compose(products, reviews)
  .withOperationResolver(resolver)
```

The client can now omit `query`:

```json
{
  "extensions": { "documentId": "product-v1" },
  "operationName": "Product",
  "variables": { "id": "p1" }
}
```

The helper uses `product-v1` to find the registered query, preserving the request's operation name, variables, and extensions. It ignores any client-supplied query text and never registers new documents. Missing, malformed, or empty IDs return `TRUSTED_DOCUMENT_ID_INVALID`; unknown IDs return `TRUSTED_DOCUMENT_NOT_FOUND` in `extensions.code`. You still need an [operation policy](#authorizing-operations) to enforce authorization.

For a database or other lookup, use `OperationResolver(resolve)`, where `resolve` is a `GraphQLRequest => ZIO[R, Throwable, String]`. Resolution runs on every request, before preparation-cache lookup. Use `OperationResolver.uncached(resolve)` to disable prepared-document and plan reuse; validation still applies. The hook runs for `executeRequest` and `explain(request)`, not `check(query)`.

Custom resolvers can fail with `ZIO.fail(OperationResolver.Rejection(message, code))` to expose a safe message and `extensions.code` (HTTP 200 with `QuickAdapter`). Unexpected failures remain masked.

## Authorizing operations

Use `OperationPolicy.fromClaims` to enforce `@authenticated` and `@requiresScopes` before an operation runs. Your authentication layer must verify the JWT first; this helper only maps trusted claims to scopes.

```scala
import caliban.GraphQLRequest
import caliban.gateway.{ Gateway, GatewayInterpreter, OperationPolicy }
import zio.{ Task, ZIO, ZLayer }

final case class VerifiedClaims(scope: String)
trait RequestClaims {
  def current: Task[Option[VerifiedClaims]]
}

val policy = OperationPolicy.fromClaims(
  ZIO.serviceWithZIO[RequestClaims](_.current)
) { claims =>
  claims.scope.split(" ").filter(_.nonEmpty).toSet
}

val secured = Gateway
  .compose(products, reviews)
  .withOperationPolicy(policy)

// Build secured.interpreter once; supply verified claims for each request:
def execute(
  interpreter: GatewayInterpreter[RequestClaims],
  request: GraphQLRequest,
  verified: Option[VerifiedClaims]
) = interpreter.executeRequest(request).provideLayer(
  ZLayer.succeed(new RequestClaims {
    def current: Task[Option[VerifiedClaims]] = ZIO.succeed(verified)
  })
)
```

`None` means anonymous; `Some` is authenticated even with no scopes. Claims are read once per protected execution, including cache hits; public operations skip the lookup. Scope alternatives use outer OR / inner AND: `[["read", "tenant"], ["admin"]]` requires both `read` and `tenant`, or `admin`. Empty `[]` or `[[]]` requires authentication only.

For named checks in `@policy`, use `OperationPolicy.fromClaims(readClaims, policyHandler)(scopes)`. The handler has type `(VerifiedClaims, String) => ZIO[R, Throwable, Boolean]` and should return `false` for unknown names. Alternatives use the same OR/AND rules as scopes, with sequential short-circuiting. An empty outer list or any empty alternative (even `[["owner"], []]`) requires only authentication and skips the handler. Other policy expressions require a handler at startup.

The helper checks every potentially selected protected field, including possible interface implementations, and rejects the **whole operation** on failure. Denials and claims or handler failures return generic messages; handler failures do not try other alternatives.

The gateway refuses to start without a policy when its schemas contain security directives. Custom policies can inspect `operation.securityRequirements`; use `OperationPolicy.Reject()` unless an explicit reason is safe to return to clients.

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

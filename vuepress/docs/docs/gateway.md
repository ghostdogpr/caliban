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

Retirement does not impose a minimum delay: an idle generation closes immediately. A stuck request can delay the next schema check by the full drain timeout (30 seconds by default), followed by the configured polling delay. Uninterruptible work can postpone it indefinitely. A warning is logged when retirement exceeds its drain timeout; the active generation continues serving while polling waits for cleanup.

**Concurrency limits remain per interpreter.** During overlap, total request and subgraph concurrency can reach twice the configured limits, and each generation has its own operation cache. The drain timeout requests interruption; uninterruptible work can hold the old generation indefinitely. In that case, the active generation continues serving and further refreshes remain paused.

Closing the owning scope stops request admission and further publication, cancels refresh work, and closes all owned scopes. Active and retiring generations drain concurrently, without resetting the old generation's deadline. Scope closure still waits for uninterruptible work to finish.

### Monitoring reloads

`interpreter.lastReloadFailure` returns a `UIO[Option[String]]` with a bounded summary of the latest failed refresh. A successful check, including an unchanged schema, clears it. Raw schemas, remote messages, response bodies, and exception causes are not retained.

Keep reload health separate from serving readiness: failed refreshes do not evict an otherwise serving generation.

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
      Map("id" -> "id"),
      "ids" -> Lookup.Argument.batch(Lookup.Argument.key("id"))
    )
  )
```

Use `Lookup.single` when the subgraph fetches one object at a time. Use `Lookup.list` when it accepts several keys in one request:

- The correlation map matches returned objects using their key fields; results must be non-null, and missing objects are omitted
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
    SchemaTransformation.hideField("Product", "internalScore")
  )
```

Clients use the new names, while the remote service continues to receive its original names.

You can rename types, fields, and arguments, or hide types, fields, optional arguments, and optional input fields. Enum values and input-field names are unchanged. Invalid or conflicting changes are reported when the gateway starts.

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

Local Caliban subgraphs run directly within the request budget. Remote subgraphs also have their own concurrency limits.

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

For a database or other lookup, use `OperationResolver(resolve)`, where `resolve` is a `GraphQLRequest => ZIO[R, Throwable, String]`. Resolution runs on every request, before preparation-cache lookup. Use `OperationResolver.uncached(resolve)` to disable prepared-document and plan reuse; validation still applies. The hook runs for `executeRequest`, `executeStream`, and `explain(request)`, not `check(query)`.

Custom validations set through `Configurator.setValidations` are part of the preparation cache key. Reuse the validation function instances across requests (for example, from one `ExecutionConfiguration`). Rebuilding a list with the same functions is fine; allocating fresh lambdas causes misses and eviction churn. The cache remains bounded by weight.

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

`@policy` is recorded as a deny-only guard, including aliased and namespace-qualified applications. Composition and reload succeed, but an operation selecting a guarded coordinate or requiring it through a lookup or `@requires` dependency is rejected before contacting any subgraph. A custom policy cannot override this rejection; unrelated operations remain available. These checks also apply to `explain(request)`.

The helper checks every potentially selected protected field, including possible interface implementations, and rejects the **whole operation** on failure. Denials and claims failures return generic messages.

Schemas with `@authenticated` or `@requiresScopes` still require an operation policy at startup. A schema containing only `@policy` needs no policy configuration. Custom policies can inspect `operation.securityRequirements`, which identify protected types and fields; use `OperationPolicy.Reject()` unless an explicit reason is safe to return to clients.

## Introspection and remote errors

The gateway exposes the composed schema through normal GraphQL introspection. To disable client introspection at the QuickAdapter boundary:

```scala
import caliban.ExecutionConfiguration

QuickAdapter(interpreter)
  .configure(ExecutionConfiguration(enableIntrospection = false))
  .runServer(4000, "/graphql")
```

This controls whether clients can inspect the combined schema. It does not prevent the gateway from loading remote schemas at startup.

Remote GraphQL error messages are redacted by default. Enable them for the entire gateway only when all upstream messages are safe for clients:

```scala
val gateway = Gateway.compose(products, reviews)
  .withConfig(_.withRemoteErrorMessages(true))
```

Only the `code` extension is passed through, regardless of the message setting.

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

### Metrics and tracing

Metrics are opt-in:

```scala
import caliban.gateway.{ Gateway, GatewayMetrics }

val gateway = Gateway.compose(products, reviews) @@ GatewayMetrics.wrapper
```

The built-in metrics cover requests, routing, subgraph calls, retries, admission counts, operation-cache activity, and subscriptions.

Add OpenTelemetry tracing with the optional tracing module:

```scala
import caliban.gateway.GatewayMetrics
import caliban.gateway.tracing.GatewayTracing

val gateway = Gateway.compose(products, reviews) @@
  (GatewayMetrics.wrapper |+| GatewayTracing.wrapper)
```

The tracing wrapper creates spans for gateway requests and remote calls. `QuickAdapter` propagates incoming trace headers automatically.

## Subscriptions

The gateway supports subscriptions from local Caliban schemas and remote GraphQL services, including fields fetched from other subgraphs. Each subscription root field must have one owner. Events arrive in source order.

### Transports

Use the existing Quick or Tapir adapters. Clients can use `graphql-transport-ws`, legacy WebSocket, or SSE with `Accept: text/event-stream` (POST recommended). JSON and multipart HTTP responses cannot carry subscriptions.

Remote subgraphs default to `graphql-transport-ws`, using the configured HTTP endpoint with `ws` or `wss`. Set `RemoteSubscriptionConfig.endpoint` for a different subscription URL, or choose SSE:

```scala
import caliban.gateway.{ RemoteGraphQLConfig, RemoteSubscriptionConfig }

val config = RemoteGraphQLConfig.default.withSubscription(
  RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse())
)
```

Pass this config when adding the remote subgraph. `Sse(useGet = true)` selects GET instead of POST. For WebSocket authentication, `connectionInit` supplies a static initialization payload; the usual remote header settings also apply.

Each remote subscription opens one upstream connection. `RemoteSubscriptionConfig.connectionTimeout` bounds acknowledgement, pong responses, and writes (30 seconds by default); `keepAliveInterval` controls ping frequency (15 seconds by default). Upstream legacy WebSocket and `@defer` / `@stream` within subscriptions are not supported. Configure downstream keepalives through your adapter's existing settings.

### Consuming from Scala

```scala
import caliban.GraphQLRequest

val events = interpreter.executeStream(
  GraphQLRequest(query = Some("subscription { productChanged { name } }"))
)
```

Each consumption starts a new subscription; cancelling it releases its resources. Authenticate around consumption and provide scoped dependencies on the stream with `provideLayer`. Interpreter middleware only covers setup: `interpreter.mapError` does not map per-event errors. Use `events.map` to transform each response's errors.

### Limits and reconnecting

Configure limits with `GatewaySubscriptionConfig`:

```scala
import caliban.gateway.GatewaySubscriptionConfig
import zio._

val bounded = gateway.withConfig(_.withSubscriptions(
  GatewaySubscriptionConfig(maxActive = 256, bufferSize = 16)
))
```

Defaults are 1,024 active subscriptions, 32 buffered events per subscription, and 30-second setup and event-processing timeouts. The gateway imposes no lifetime or event-size limit; remote messages remain bounded by `RemoteGraphQLConfig.Execution.maxResponseBytes`. The ordinary request timeout does not end subscriptions.

- Capacity exhaustion rejects new subscriptions. Buffer overflow terminates the affected subscription with `SUBSCRIPTION_OVERFLOW` instead of silently dropping events.
- Remote messages exceeding `maxResponseBytes` terminate the subscription with `SUBSCRIPTION_EVENT_TOO_LARGE`.
- Schema reload ends existing subscriptions with retryable `SUBSCRIPTION_SCHEMA_RELOAD`. Clients should resubscribe; failed reloads leave subscriptions running.
- There is no automatic upstream reconnect or event replay. Clients must handle terminal errors and reconnect as appropriate; events during a disconnect may be lost.

### Authentication and monitoring

Authenticate during setup; the gateway evaluates authorization once and captures forwarding headers for the subscription's lifetime. Credential expiry and revocation belong to the WebSocket or authentication layer. Policies are not re-evaluated for each event.

Remote error messages follow the gateway’s `withRemoteErrorMessages` setting; only the `code` extension is retained. Local sources retain Caliban's native behavior: a field resolver failure may produce null without an error entry in that event.

The metrics and tracing wrappers include subscription observations. Shutdown waits for resource cleanup, including uninterruptible finalizers.

Setup and event spans inherit incoming `traceparent` or ambient trace context. Without either, they are independent roots; no separate subscription correlation ID is added.

## Current protocol support

The gateway supports queries, mutations, and subscriptions. It does not support `@defer` or `@stream` incremental responses from subgraphs.

Complete runnable examples are available in the repository's [`gateway-examples`](https://github.com/ghostdogpr/caliban/tree/series/3.x/gateway-examples) project, including local, ordinary remote, mixed-subgraph, and Federation gateways.

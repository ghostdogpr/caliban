# Gateway

Caliban Gateway gives clients one GraphQL endpoint backed by multiple GraphQL services. A gateway can combine:

- ordinary GraphQL services
- Apollo Federation subgraphs
- in-process Caliban APIs

Define the subgraphs, pass them to `Gateway.compose`, and serve the combined API with `QuickAdapter`.

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

At startup, the gateway loads and combines both schemas. If composition fails, the application exits and identifies the subgraph or schema that caused the error.

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

Startup still requires a valid initial interpreter. After startup, the gateway polls ordinary acquired schemas through introspection and Federation schemas through `_service`. Pinned SDL, parsed documents, local graphs, endpoints, and configuration stay fixed. At least one subgraph must acquire its schema remotely.

Each refresh collects all acquired schemas. It applies the acquisition timeout and body-size limit configured for each subgraph. The gateway reuses one acquisition HTTP client for the lifetime of the reloadable interpreter. Each interpreter generation has a separate execution client. If schema collection fails, the gateway keeps the current generation.

The gateway compares the parsed schemas with the active snapshot. The comparison ignores whitespace, comments, source locations, and the order of schema elements. Schema elements include definitions, fields, arguments, enum values, union members, and implemented interfaces. The comparison retains descriptions, directive application order, and input list-value order. The gateway canonicalizes only the fingerprint, not the documents used for composition. If the schemas have not changed, the gateway keeps the interpreter and its warm operation cache.

When a schema changes, the gateway builds a candidate from the collected documents without fetching them again. The candidate must pass the usual build validation. The gateway does not run a separate breaking-change check.

Request admission uses atomic references and no shared lock. If the selected generation has stopped accepting work, the gateway selects again before starting the request. A lease reserved after publication but before the old generation starts draining still belongs to the old generation. An admitted operation finishes on its original generation. The gateway does not retry operation resolution or execution after a replacement race. It checks for shutdown again after reserving a lease. If admission has closed, the gateway releases the lease without executing the request.

The default interval is 30 seconds, with up to 20% jitter in either direction. The delay starts after the refresh cycle and any retirement work finish. Refresh cycles never overlap, so the interval does not guarantee that schemas are at most 30 seconds old. If acquisition or construction fails, the active interpreter keeps serving requests and the gateway retries during the next cycle.

### Draining and resource limits

The old interpreter drains for the duration set by `GatewayConfig.withDrainTimeout`. At most two interpreter generations exist at once. During construction, they are the active and candidate generations. After publication, they are the active and retiring generations. The next refresh waits for retirement to finish.

Retirement has no minimum delay. An idle generation closes immediately. A stuck request can delay the next schema check by the full drain timeout, which is 30 seconds by default, plus the configured polling delay. Uninterruptible work can postpone the check indefinitely. If retirement exceeds the drain timeout, the gateway logs a warning. The active generation keeps serving requests while polling waits for cleanup.

Concurrency limits apply to each interpreter. During an overlap, total request and subgraph concurrency can reach twice the configured limits. Each generation also has its own operation cache. When the drain timeout expires, the gateway requests interruption. Uninterruptible work can hold the old generation indefinitely. The active generation keeps serving requests, but refreshes remain paused.

Closing the owning scope stops request admission and publication. It also cancels refresh work and closes all owned scopes. Active and retiring generations drain at the same time, without resetting the old generation's deadline. Scope closure waits for uninterruptible work to finish.

### Monitoring reloads

`interpreter.lastReloadFailure` returns a `UIO[Option[String]]` containing a bounded summary of the latest failed refresh. A successful check clears the summary, even when the schema has not changed. The gateway does not retain raw schemas, remote messages, response bodies, or exception causes.

Report reload health separately from serving readiness. A failed refresh does not evict a generation that can still serve requests.

The gateway logs activation, failure, recovery, and overdue retirement. It suppresses repeated logs for the same failure. Hot reload adds no administrative HTTP endpoints, manual refresh, metrics, or tracing interfaces.

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

If you omit the SDL, the gateway loads it from the service at startup. Providing SDL directly is useful when introspection is disabled or unavailable. The service endpoint must still be reachable for request execution. Keep provided SDL in sync with the deployed service.

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

The gateway acquires each Federation schema through `_service`. To pin a Federation schema, pass its SDL as the third argument to `Subgraph.federation`.

The gateway reads entity information from the Federation schemas. You do not need to add the `Lookup` configuration described in the next section.

### Progressive field overrides

Federation 2.7 and later can move a field between subgraphs gradually. Add a percentage label to `@override` to let the gateway select the overriding subgraph for that share of requests:

```graphql
type Product {
  inStock: Boolean @override(from: "inventory", label: "percent(10)")
}
```

The gateway resolves `percent(x)` labels itself. For a percentage between 0 and 100, it makes one random decision for each label and request. The selection is not sticky across requests. Fields that use the same label share the decision.

For a custom label, attach a `GatewayWrapper.overrideLabels` wrapper:

```scala
import caliban.GraphQLRequest
import caliban.gateway.{ Gateway, GatewayWrapper }
import zio.Task

def activeLabels(request: GraphQLRequest): Task[Set[String]] = ???

val progressiveOverrides = GatewayWrapper.overrideLabels[Any] { (request, labels) =>
  activeLabels(request).map(_ intersect labels)
}

val gateway = Gateway.compose(products, reviews) @@ progressiveOverrides
```

The wrapper receives the request and the custom labels reached by the selected operation. Return the labels that should use the overriding subgraph. The gateway ignores labels that were not supplied. Without this wrapper, custom labels remain inactive and the gateway uses the original subgraph.

The gateway calls the wrapper once per relevant request, before it checks the operation cache. It does not call the wrapper for percentage-only operations or operations that reach no custom labels. Each active-label combination has its own cached plan, so keep the label lookup cheap and limit the number of combinations that it returns. If the wrapper fails, the gateway returns an internal execution error without contacting a subgraph.

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

You can pass remote, Federation, and local subgraphs to the same `Gateway.compose` call.

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

Describe how the reviews service fetches products:

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

- The correlation map uses key fields to match returned objects. Results must be non-null. The gateway omits missing objects.
- `Argument.key("id")` reads the `id` from the object that the gateway is fetching.
- `Argument.obj(...)` builds an input object for the subgraph.
- `Argument.batch(...)` builds one argument value for each requested object.

Prefer a batch lookup when the subgraph supports it. The gateway can then fetch several objects in one subgraph request.

## Configuring remote services

Use `RemoteGraphQLConfig` to set timeouts, retries, concurrency, headers, or body-size limits for a remote service.

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

The configuration applies only to that subgraph. Each remote service can use different settings.

Retries are disabled by default. When enabled, the gateway only retries requests that are safe to repeat.

Concurrent identical remote queries share one in-flight call by default. The request body and the outbound headers that affect request semantics must match. Mutations never share a call. To disable this behavior for a subgraph, use `.withExecution(_.withInFlightQueryDeduplication(false))`.

If loading the schema at startup requires authentication, configure its headers separately:

```scala
val remoteConfig = RemoteGraphQLConfig.default.withAcquisition(
  _.withTimeout(5.seconds)
    .withHeaders(Header("X-Schema-Token", "schema-secret"))
)
```

Acquisition headers are sent during the initial schema load and every schema refresh. The execution headers in the next section are sent with remote queries, mutations, and subscription setup.

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

Header names are matched case-insensitively. Prefer an explicit allowlist. Use `forwardAllIncomingHeaders` only when every incoming header is safe to send to the subgraph.

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

Clients use the new names. The remote service still receives the original names.

You can rename types, fields, and arguments. You can hide types, fields, optional arguments, and optional input fields. Enum values and input-field names do not change. The gateway reports invalid or conflicting changes at startup.

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

The main settings are:

- `withMaxConcurrentRequests` for how many requests the gateway handles at once
- `withRequestTimeout` for the maximum duration of a client request
- `withDrainTimeout` for the time allowed to finish requests during shutdown

Local Caliban subgraphs run directly within the request budget. Remote subgraphs also have their own concurrency limits.

QuickAdapter has separate HTTP body limits:

```scala
QuickAdapter(interpreter)
  .withMaxRequestBodyBytes(2 * 1024 * 1024)
  .withMaxUploadBodyBytes(32 * 1024 * 1024)
  .withMaxResponseBodyBytes(16 * 1024 * 1024)
  .runServer(4000, "/graphql")
```

## Persisted and trusted documents

Clients usually send the full GraphQL query with each request. With persisted documents, they send an ID and the server looks up the query text. Trusted documents restrict clients to queries that your application has registered.

`OperationResolver` handles this lookup. It takes an incoming request and returns the query text that the gateway parses, validates, and executes. Use `trustedDocuments` for an in-memory registry:

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

The helper uses `product-v1` to find the registered query. It preserves the request's operation name, variables, and extensions. It ignores query text supplied by the client and never registers new documents. A missing, malformed, or empty ID returns `TRUSTED_DOCUMENT_ID_INVALID`. An unknown ID returns `TRUSTED_DOCUMENT_NOT_FOUND` in `extensions.code`. To enforce authorization, add an [operation policy](#authorizing-operations).

For a database or another lookup, use `OperationResolver(resolve)`. The `resolve` function has the type `GraphQLRequest => ZIO[R, Throwable, String]`. It runs on every request before the gateway checks the preparation cache. Use `OperationResolver.uncached(resolve)` to disable prepared-document and plan reuse. Validation still applies. The resolver runs for `executeRequest`, `executeStream`, and `explain(request)`, but not for `check(query)`.

Custom validations set through `Configurator.setValidations` are part of the preparation cache key. Reuse the validation function instances across requests, such as those from one `ExecutionConfiguration`. You can rebuild a list with the same functions. New lambda instances cause cache misses and evictions. The cache remains bounded by weight.

To return a safe message and `extensions.code`, fail a custom resolver with `ZIO.fail(OperationResolver.Rejection(message, code))`. `QuickAdapter` returns these rejections with HTTP 200. The gateway hides unexpected failures.

## Authorizing operations

Use `OperationPolicy.fromClaims` to enforce `@authenticated` and `@requiresScopes` before an operation runs. Your authentication layer must verify the JWT first. This helper only maps trusted claims to scopes.

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

`None` means anonymous. `Some` means authenticated, even when the claim has no scopes. The policy reads claims once per protected execution, including cache hits. Public operations skip the lookup. In `[["read", "tenant"], ["admin"]]`, a user needs both `read` and `tenant`, or needs `admin`. An empty `[]` or `[[]]` requires authentication but no scopes.

The gateway records `@policy` as a deny-only guard, including aliased and namespace-qualified applications. Composition and reload still succeed. Before contacting a subgraph, the gateway rejects an operation that selects a guarded coordinate or depends on one through a lookup or `@requires`. A custom policy cannot override this rejection. Unrelated operations remain available. The same checks apply to `explain(request)`.

The helper checks every protected field that the operation could select, including fields on possible interface implementations. If any check fails, it rejects the whole operation. Denials and claim failures return generic messages.

Schemas with `@authenticated` or `@requiresScopes` require an operation policy at startup. A schema that contains only `@policy` needs no policy configuration. Custom policies can inspect `operation.securityRequirements`, which identify protected types and fields. Use `OperationPolicy.Reject()` unless the rejection reason is safe to return to clients.

## Introspection and remote errors

The gateway exposes the composed schema through normal GraphQL introspection. To disable client introspection at the QuickAdapter boundary:

```scala
import caliban.ExecutionConfiguration

QuickAdapter(interpreter)
  .configure(ExecutionConfiguration(enableIntrospection = false))
  .runServer(4000, "/graphql")
```

This setting controls client access to the combined schema. It does not affect remote schema loading at startup.

The gateway hides remote GraphQL error messages by default. Enable them only when every upstream message is safe for clients:

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

Each `fetch` shows the subgraph name. Here, `products` and `reviews` are the names passed to `Subgraph.graphql`. `$` is the root of the client response, so `$.product` refers to the `product` field at the root. The `(key)` marker identifies a field used as a key for a later lookup. The gateway may reuse a client selection or add the field itself. `after products` means that the fetch depends on the result from `products`.

Use the plan to test a new lookup or find why the gateway sends a field to a specific service.

### Metrics and tracing

Metrics are opt-in:

```scala
import caliban.gateway.{ Gateway, GatewayMetrics }

val gateway = Gateway.compose(products, reviews) @@ GatewayMetrics.wrapper
```

The built-in metrics report requests, routing, subgraph calls, retries, admission counts, operation-cache activity, and subscriptions.

Add OpenTelemetry tracing with the optional tracing module:

```scala
import caliban.gateway.GatewayMetrics
import caliban.gateway.tracing.GatewayTracing

val gateway = Gateway.compose(products, reviews) @@
  (GatewayMetrics.wrapper |+| GatewayTracing.wrapper)
```

The tracing wrapper creates spans for gateway requests and remote calls. `QuickAdapter` propagates incoming trace headers.

## Subscriptions

The gateway supports subscriptions from local Caliban schemas and remote GraphQL services. A subscription can include fields fetched from other subgraphs. Each subscription root field must have one owner. Events arrive in source order.

### Transports

Use the existing Quick or Tapir adapters. Clients can use `graphql-transport-ws`, legacy WebSocket, or SSE with `Accept: text/event-stream`. Use POST for SSE. JSON and multipart HTTP responses cannot carry subscriptions.

Remote subgraphs use `graphql-transport-ws` by default. They use the configured HTTP endpoint with the `ws` or `wss` scheme. Set `RemoteSubscriptionConfig.endpoint` to use a different subscription URL. To use SSE, configure the transport:

```scala
import caliban.gateway.{ RemoteGraphQLConfig, RemoteSubscriptionConfig }

val config = RemoteGraphQLConfig.default.withSubscription(
  RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse())
)
```

Pass this config when you add the remote subgraph. `Sse(useGet = true)` selects GET instead of POST. For WebSocket authentication, use `connectionInit` to supply a static initialization payload. The usual remote header settings also apply.

Each remote subscription opens one upstream connection. `RemoteSubscriptionConfig.connectionTimeout` limits acknowledgement, pong responses, and writes to 30 seconds by default. `keepAliveInterval` sets the delay before each ping and defaults to 15 seconds. The gateway waits for the pong before starting the next delay. The gateway does not support upstream legacy WebSocket. It also does not support `@defer` or `@stream` within subscriptions. Configure downstream keepalives through the adapter.

### Consuming from Scala

```scala
import caliban.GraphQLRequest

val events = interpreter.executeStream(
  GraphQLRequest(query = Some("subscription { productChanged { name } }"))
)
```

Each time you consume the stream, the gateway starts a new subscription. Cancelling it releases its resources. Authenticate around consumption. Provide scoped dependencies on the stream with `provideLayer`. Interpreter middleware covers only setup, so `interpreter.mapError` does not map errors from individual events. Use `events.map` to transform each response's errors.

### Limits and reconnecting

Configure limits with `GatewaySubscriptionConfig`:

```scala
import caliban.gateway.GatewaySubscriptionConfig
import zio._

val bounded = gateway.withConfig(_.withSubscriptions(
  GatewaySubscriptionConfig(maxActive = 256, bufferSize = 16)
))
```

The defaults allow 1,024 active subscriptions and 32 buffered events per subscription. Setup and event-processing timeouts default to 30 seconds. The gateway sets no subscription lifetime or event-size limit. `RemoteGraphQLConfig.Execution.maxResponseBytes` still limits remote messages. The ordinary request timeout does not end subscriptions.

- When the gateway reaches capacity, it rejects new subscriptions. A buffer overflow terminates the affected subscription with `SUBSCRIPTION_OVERFLOW` instead of dropping events.
- Remote messages exceeding `maxResponseBytes` terminate the subscription with `SUBSCRIPTION_EVENT_TOO_LARGE`.
- A schema reload ends existing subscriptions with the retryable `SUBSCRIPTION_SCHEMA_RELOAD` error. Clients must resubscribe. A failed reload leaves subscriptions running.
- The gateway does not reconnect upstream or replay events. Clients must handle terminal errors and reconnect. They may lose events while disconnected.

### Authentication and monitoring

Authenticate during setup. The gateway evaluates authorization once and captures forwarded headers for the subscription's lifetime. The WebSocket or authentication layer handles credential expiry and revocation. The gateway does not evaluate policies again for each event.

Remote error messages follow the gateway's `withRemoteErrorMessages` setting. The gateway retains only the `code` extension. Local sources keep Caliban's behavior. A field resolver failure can produce null without an error entry in that event.

The metrics and tracing wrappers include subscription observations. Shutdown waits for resource cleanup, including uninterruptible finalizers.

Setup and event spans inherit the incoming `traceparent` or ambient trace context. Without that context, they are independent roots. The gateway does not add a subscription correlation ID.

## Current protocol support

The gateway supports queries, mutations, and subscriptions. It does not support `@defer` or `@stream` incremental responses from subgraphs.

The repository's [`gateway-examples`](https://github.com/ghostdogpr/caliban/tree/series/3.x/gateway-examples) project contains runnable examples for local, ordinary remote, mixed-subgraph, and Federation gateways.

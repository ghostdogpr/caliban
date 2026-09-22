# Configuration

Configure each remote service with `RemoteGraphQLConfig` and the whole gateway with `GatewayConfig`. The examples extend the gateway from [Getting Started](../gateway.md).

## Configuring remote services

Use `RemoteGraphQLConfig` to set timeouts, retries, headers, or body-size limits for a remote service.

```scala
import caliban.gateway.{ Gateway, RemoteGraphQLConfig, Subgraph }
import zio._
import zio.http._

val remoteConfig = RemoteGraphQLConfig.default
  .withExecution(
    _.withTimeout(10.seconds)
      .withRetries(2, 100.millis)
  )

val products = Subgraph.graphql(
  "products",
  url"http://products:8080/graphql",
  remoteConfig
)
```

Retries are off by default. `withRetries(2, 100.millis)` permits two retries after the initial attempt, with a fixed 100 ms delay between attempts. Only queries are retried, and only after a transport failure or an HTTP failure with status 502, 503, or 504. Mutations, decoded GraphQL error responses, invalid responses, and body-limit failures are not retried. Subscription connections do not reconnect automatically.

The execution timeout covers header loading, attempts, and retry delays together. The whole request is also bounded by the gateway's request timeout.

Concurrent identical queries to a remote service share one in-flight call when their request bodies and headers match. Mutations never share calls. Disable sharing with `.withExecution(_.withInFlightQueryDeduplication(false))`.

If loading the schema at startup requires authentication, configure its headers separately:

```scala
val remoteConfig = RemoteGraphQLConfig.default.withAcquisition(
  _.withTimeout(5.seconds)
    .withHeaders(Header.Custom("X-Schema-Token", "schema-secret"))
)
```

The gateway sends acquisition headers on the initial schema load and on every refresh. It sends the execution headers covered next with remote queries, mutations, and subscription setup.

### Remote defaults

Change these settings through `withAcquisition` or `withExecution`. Sizes are in bytes. 1 MiB is `1024 * 1024` bytes.

| Setting | Acquisition default | Execution default |
| --- | --- | --- |
| `withTimeout` | 10 seconds | 30 seconds per logical call, including retries |
| `withMaxRequestBytes` | Not configurable | 1 MiB |
| `withMaxResponseBytes` | 16 MiB | 16 MiB |
| `withMaxParsingDepth` | 128 | Not configurable |
| `withMaxRedirects` | 0 | Redirects are rejected |
| `withRetries` | No retries configured here | 0 retries; 100 ms backoff when enabled |
| `withInFlightQueryDeduplication` | Not applicable | Enabled |
| `withHeaders` | No headers | No headers |
| `forwardIncomingHeaders` | Not applicable | No client headers forwarded |

Acquisition redirects, when enabled, do not receive the original request's credentials. Use [subscription settings](subscriptions.md) for connection timeouts and buffering.

### Authentication and request headers

To send the same credentials with every request to a service:

```scala
val config = RemoteGraphQLConfig.default.withExecution(
  _.withHeaders(Header.Authorization.Bearer("service-token"))
)
```

If the token must be loaded or refreshed dynamically, use `withExecutionHeadersZIO`:

```scala
val loadToken: Task[String] = ???

val config = RemoteGraphQLConfig.default.withExecutionHeadersZIO(
  loadToken.map(token => List(Header.Authorization.Bearer(token)))
)
```

Forward selected client headers by name:

```scala
val config = RemoteGraphQLConfig.default.withExecution(
  _.forwardIncomingHeaders("Authorization", "X-Request-ID")
)
```

Header precedence, from lowest to highest, is forwarded client headers, static headers, effectful headers, then transport-owned headers. Higher-precedence values replace lower-precedence values with the same name. Matching ignores header-name case.

Prefer an explicit allowlist. `forwardAllIncomingHeaders` is safe only when every header a client can send is safe to hand a subgraph.

`QuickAdapter` handles forwarded headers automatically. If you build your own HTTP integration, pass the incoming headers with `interpreter.executeRequest(request, headers)`.

To adjust headers across subgraphs or per retry, use the [subgraph and attempt hooks](hooks.md#subgraph-request-headers).

## Gateway limits and shutdown

Configure limits shared by the whole gateway with `withConfig`:

```scala
import zio._

val gateway = Gateway
  .compose(products, reviews)
  .withConfig(
    _.withRequestTimeout(10.seconds)
      .withDrainTimeout(20.seconds)
  )
```

The defaults are:

| `GatewayConfig` method | Default | Purpose |
| --- | --- | --- |
| `withRequestTimeout` | 30 seconds | Whole query or mutation request, including preparation and completion |
| `withDrainTimeout` | 30 seconds | Time to finish accepted work during shutdown or schema replacement |
| `withMaxOperationCacheWeight` | 8 MiB | Total estimated weight of cached operations and plans |
| `withMaxPlanningCandidates` | 8,192 | Alternative routes considered per operation |
| `withMaxPlanningExpansions` | 100,000 | Candidate plans expanded per operation |
| `withPlanningTimeout` | 2 seconds | Time allowed to plan an operation |
| `withMaxOperationCost` | Disabled | Reject operations above a positive cost limit |
| `withReloadPollInterval` | 30 seconds | Delay after each reload cycle |
| `withReloadJitter` | 0.2 | Vary the reload delay by up to 20% in either direction |
| `withRemoteErrorMessages` | `false` | Hide remote GraphQL error messages |
| `withSubscriptions` | See [Subscriptions](subscriptions.md#limits-and-reconnecting) | Active subscriptions, buffers, and timeouts |

Local Caliban subgraphs run within the request budget. Remote subgraphs also have their own call timeouts. Subscription setup and events use separate deadlines.

Exceeding a planning limit rejects the operation before execution. Increase a limit only after inspecting the query and its [plan](planning.md#explain-a-query-plan). Cache weight is an estimate, not a bound on the JVM's total memory use. Use `withoutMaxOperationCost` to disable a previously configured cost limit.

Closing the interpreter's owning scope starts shutdown. Keep that scope open for the server's lifetime, as shown in [Getting Started](../gateway.md#interpreter-lifetime).

### Demand control

`withMaxOperationCost` enables static demand control. The gateway estimates the planned subgraph requests after binding request variables and rejects an operation before contacting any subgraph when its cost exceeds the limit:

```scala
val gateway = Gateway
  .compose(products, reviews)
  .withConfig(_.withMaxOperationCost(1000))
```

Use Federation 2.9 `@cost` to set field and type weights, and `@listSize` to describe list sizes. The gateway also accepts these directives through a `https://specs.apollo.dev/cost/v0.1` schema link.

The estimate includes fields fetched for entity keys and `@requires`. Composite values cost 1 by default, while scalars and enums cost 0. Each planned mutation root fetch adds 10. Lists need `@listSize` to account for their length. Without it, a large list can have a low estimated cost.

### HTTP body limits

QuickAdapter has separate limits for client requests and responses:

```scala
QuickAdapter(interpreter)
  .withMaxRequestBodyBytes(2 * 1024 * 1024)
  .withMaxResponseBodyBytes(16 * 1024 * 1024)
  .runServer(4000, "/graphql")
```

## Hot reload

Use `gateway.reloadable` instead of `gateway.interpreter` to refresh acquired remote schemas without replacing your HTTP adapter:

```scala
import caliban.QuickAdapter
import zio._

val reloadableGateway = gateway.withConfig(
  _.withReloadPollInterval(30.seconds)
    .withReloadJitter(0.2)
)

for {
  interpreter <- reloadableGateway.reloadable
  _           <- QuickAdapter(interpreter).runServer(4000, "/graphql")
} yield ()
```

Startup requires a valid initial schema. With `Gateway.compose`, at least one subgraph must load its schema remotely. The gateway polls ordinary services through introspection and Federation services through `_service`. Pinned SDL, parsed documents, and local APIs stay fixed, as do configured endpoints and other settings.

A gateway built [from a supergraph](subgraphs.md#supergraphs) can reload a file, HTTP, or registry source. Apollo Uplink requires a minimum poll interval of ten seconds, including the shortest delay permitted by jitter.

Each refresh loads schemas under the configured acquisition timeout and response-size limit. If acquisition or composition fails, the current schema keeps serving and the gateway tries again on the next cycle. The gateway does not check for breaking changes during reload. Validate compatibility with your clients before publishing schema changes.

Unchanged schemas keep the current interpreter and operation cache. Formatting and declaration-order changes alone do not trigger replacement.

New requests use the new schema after a successful replacement. Requests already running finish against their original schema. The gateway does not replay them on the new one. Existing subscriptions end with `SUBSCRIPTION_SCHEMA_RELOAD`. Clients must resubscribe.

Polling starts 30 seconds after the previous cycle finishes by default, with up to 20% jitter. Cycles do not overlap. Slow acquisition or draining can therefore delay the next check beyond the configured interval.

### Draining

After replacement, the old interpreter has `withDrainTimeout` to finish requests, 30 seconds by default. The gateway then interrupts remaining work. New requests continue on the new schema while the old interpreter drains.

The next refresh waits for draining to finish. Uninterruptible work can delay refreshes and shutdown beyond the drain timeout. The gateway logs a warning when draining exceeds that timeout.

### Monitoring reloads

`interpreter.lastReloadFailure` returns a safe summary of the latest failed refresh, or `None` after a successful check. The gateway also logs reload failures and recovery.

Report reload health separately from serving readiness. A failed refresh leaves the current schema serving. Expose health endpoints through your HTTP application. The gateway does not add them.

## Introspection

The gateway exposes the composed schema through normal GraphQL introspection. To disable client introspection at the QuickAdapter boundary:

```scala
import caliban.Configurator.ExecutionConfiguration

QuickAdapter(interpreter)
  .configure(ExecutionConfiguration(enableIntrospection = false))
  .runServer(4000, "/graphql")
```

The setting controls client access to the combined schema only. Remote schema loading at startup is unaffected.

## Errors

The gateway hides remote GraphQL error messages by default. Enable them only when every upstream message is safe for clients:

```scala
val gateway = Gateway.compose(products, reviews)
  .withConfig(_.withRemoteErrorMessages(true))
```

Only the `code` extension is passed through, regardless of the message setting.

### Failures and partial responses

A remote call failure affects the fields that depend on that call. Independent fields can still return data. GraphQL nullability applies: a failure on a non-null field propagates null to its nearest nullable parent, which can make the whole `data` value null.

For example, if `Query.product` is nullable and the products service is unavailable, another service can still return `latestReviews`:

```json
{
  "data": {
    "product": null,
    "latestReviews": [{ "body": "Composable and type-safe" }]
  },
  "errors": [
    { "message": "Remote GraphQL request failed.", "path": ["product"] }
  ]
}
```

A per-subgraph timeout has the same field-level behavior. If the whole gateway request exceeds `withRequestTimeout`, the response instead has `data: null` and the error `Gateway request timed out.` QuickAdapter returns HTTP 504 for that deadline and HTTP 503 when the gateway is shutting down. Field execution errors normally return HTTP 200, so clients must inspect `errors` even after a successful HTTP status.

Use [phase hooks](hooks.md) to distinguish transport failures, timeouts, and limit failures without exposing upstream details to clients. Subscription failures have [separate termination rules](subscriptions.md#limits-and-reconnecting).

### Troubleshooting

| Symptom | What to check |
| --- | --- |
| Startup cannot load a schema | Endpoint reachability and acquisition credentials. Ordinary services need introspection unless SDL is pinned; Federation services need `_service` unless SDL is pinned. |
| Startup reports incompatible definitions or multiple owners | The named services' types and fields. Follow the [composition rules](subgraphs.md#composition-rules). |
| A query cannot reach a field in another ordinary service | The target service's [lookup](planning.md#connecting-objects-across-ordinary-services), key fields, and argument mapping. Inspect the plan with `explain`. |
| Startup requires an authorization handler | Install an incoming [authorization hook](hooks.md#authorizing-operations) for `@authenticated` or `@requiresScopes`. |
| Requests fail only for large queries or responses | Gateway planning and cost limits, remote body limits, and QuickAdapter body limits. These are separate settings. |
| Schema changes do not appear | Use `.reloadable`, confirm the schema source is refreshable, and inspect `lastReloadFailure`. Pinned SDL and local APIs do not refresh. |

`gateway.interpreter` and `gateway.reloadable` fail with `GatewayBuildError`. Its `diagnostics` list contains startup details suitable for logs. Request-time GraphQL failures appear in the response's `errors` field.

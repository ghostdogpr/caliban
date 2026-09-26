# Hooks

Use `PhaseHooks` to resolve documents, authorize operations, select progressive overrides, adjust outbound headers, and observe execution. Attach hooks with `gateway.withPhaseHooks(hooks)` or `gateway @@ hooks`. Both append to existing hooks. Combine bundles with `++`.

Use `PhaseHandler.incoming` to change an event, `incomingDiscard` for a check or side effect, and `outgoing` to observe the result. Incoming handlers run in registration order. Outgoing handlers run in reverse order. `PhaseHandler.scoped` keeps resources alive until the outgoing callback finishes.

For example, log the result of each subgraph call:

```scala
import caliban.gateway.{ PhaseHandler, PhaseHooks }
import zio.ZIO

val logged = gateway.withPhaseHooks(
  PhaseHooks.subgraphCall(
    PhaseHandler.outgoing((event, result) =>
      ZIO.logInfo(s"${event.subgraph}: ${result.outcome.label}")
    )
  )
)
```

Hooks are listed in entry order for a query or mutation. `operation` wraps both `preparation` and `execution`.

| Hook | What it covers |
| --- | --- |
| `operation` | The whole request, from preparation to response assembly, including failures. |
| `preparation` | All work before execution, including document resolution, parsing, validation, authorization, and planning. |
| `resolution` | Supplies query text, for example from a persisted document ID. Runs before parsing, even on cache hits. |
| `overrideLabels` | Selects active custom `@override` labels before plan lookup. |
| `cacheAccess` | Cache lookup and preparation on a miss. Skipped when caching is disabled. |
| `authorization` | Allows or rejects a validated operation before execution, even on cache hits. |
| `execution` | Runs the query or mutation plan and assembles the response. |
| `subgraphCall` | One local or remote call, including deduplication and retries. Can change remote headers. |
| `attempt` | One remote request and response. Can change headers. Repeats for each retry. |
| `completion` | Builds the client response from subgraph results or gateway errors. |

Outgoing callbacks run as each phase finishes. Subgraph calls may run in parallel. Failure paths skip later work and may reach `completion` directly.

For subscriptions, use `subscriptionAdmission` to observe acceptance or rejection, `subscriptionSetup` for startup, `subscriptionEvent` for each event, and `subscriptionTerminated` for the termination reason. Remote connections and calls made while processing events also use `subgraphCall` and `attempt`. Successful subscriptions skip `execution`.

Resolution, authorization, and override-label handlers can reject a request. Other hooks observe or modify events without a typed error channel. Hooks count toward request deadlines. `explain(request)` runs resolution and authorization hooks. `check(query)` validates literal query text without them.

## Persisted and trusted documents

Clients usually send the full GraphQL query with each request. With persisted documents, they send an ID and the server looks up the query text. Trusted documents restrict clients to queries that your application has registered.

`PhaseHooks.resolution` handles this lookup before parsing, validation, and cache lookup. Use `PhaseHooks.trustedDocuments` for an in-memory registry:

```scala
import caliban.Value.StringValue
import caliban.gateway.{ Gateway, PhaseHooks }

val documents = Map(
  "product-v1" -> "query Product($id: String!) { product(id: $id) { name } }"
)

val documentsHook = PhaseHooks.trustedDocuments(documents) { request =>
  request.extensions.flatMap(_.get("documentId")).collect {
    case StringValue(id) => id
  }
}

val gateway = Gateway
  .compose(products, reviews)
  .withPhaseHooks(documentsHook)
```

The client can now omit `query`:

```json
{
  "extensions": { "documentId": "product-v1" },
  "operationName": "Product",
  "variables": { "id": "caliban" }
}
```

The helper uses the registered query and ignores client-supplied query text. It preserves the operation name, variables, and extensions, and never registers new documents. Invalid IDs return `TRUSTED_DOCUMENT_ID_INVALID` in `extensions.code`. Unknown IDs return `TRUSTED_DOCUMENT_NOT_FOUND`. Add an [authorization hook](#authorizing-operations) to control who can execute a registered query.

For a database lookup, use `PhaseHooks.resolution(resolve)`, where `resolve` is a `GraphQLRequest => ZIO[R, Throwable, String]`. It replaces query text before [cache lookup](planning.md#operation-cache), even on cache hits. Pass `cacheable = false` when the result must not reuse a cached operation. Validation still applies. To change other request fields, use `PhaseHooks.resolutionHandler(handler)` with a handler over `PhaseHooks.Event.Resolution`.

To return a safe message and `extensions.code`, fail a custom resolver with `ZIO.fail(PhaseHooks.Rejection(message, code))`. `QuickAdapter` returns these rejections as request errors: HTTP 200 for `application/json` clients, or 400 when the client accepts `application/graphql-response+json`. The gateway hides unexpected failures.

## Progressive override labels

For a custom label, attach an override-label hook:

```scala
import caliban.GraphQLRequest
import caliban.gateway.{ Gateway, PhaseHandler, PhaseHooks }
import caliban.gateway.PhaseHooks.Event
import zio.Task

def activeLabels(request: GraphQLRequest): Task[Set[String]] = ???

val progressiveOverrides = PhaseHooks.overrideLabels(
  PhaseHandler.incoming[Any, Event.OverrideLabels, Throwable] { event =>
    activeLabels(event.request).map(labels => event.activate(labels intersect event.reached))
  }
)

val gateway = Gateway.compose(products, reviews) @@ progressiveOverrides
```

Call `event.activate` with the labels that should use the overriding subgraph. `event.reached` contains the custom labels used by this operation. Unactivated labels use the original service. Multiple hooks can add labels.

The hook runs once per request that uses custom labels, before cache lookup. Percentage-only overrides need no hook. Each active-label combination has its own cached plan, so keep the lookup fast and the number of combinations small. A failing hook stops the request before any subgraph call.

## Authorizing operations

Use `PhaseHooks.fromClaims` to enforce `@authenticated` and `@requiresScopes` before an operation runs. Authenticate requests in your HTTP layer and pass the verified claims to this helper.

```scala
import caliban.gateway.{ Gateway, GatewayInterpreter, PhaseHooks }
import zio.{ Task, ZIO, ZLayer }

final case class VerifiedClaims(scope: String)
trait RequestClaims {
  def current: Task[Option[VerifiedClaims]]
}

val authorization = PhaseHooks.fromClaims(
  ZIO.serviceWithZIO[RequestClaims](_.current)
) { claims =>
  claims.scope.split(" ").filter(_.nonEmpty).toSet
}

val secured = Gateway
  .compose(products, reviews)
  .withPhaseHooks(authorization)
```

`None` means anonymous. `Some` means authenticated, even when the claim has no scopes. The hook reads claims once per protected execution, including cache hits. Public operations skip the lookup. In `[["read", "tenant"], ["admin"]]`, a user needs both `read` and `tenant`, or needs `admin`. An empty `[]` or `[[]]` requires authentication but no scopes.

If any selected field fails authorization, the gateway rejects the whole operation before contacting subgraphs. Checks include protected fields on possible interface implementations. Denials and claim failures return generic messages.

`@policy` always denies operations that select the annotated type or field. Custom authorization cannot override it. The gateway does not evaluate external policy rules. Composition rejects a field that depends on a `@policy` field through `@requires` or `@fromContext` unless it declares policies that satisfy them.

Schemas with `@authenticated` or `@requiresScopes` require an incoming `authorization` handler at startup. An outgoing-only observer does not satisfy this requirement. A schema that contains only `@policy` needs no authorization hook.

For custom checks, use `PhaseHooks.authorization(operation => ...)`, returning `ZIO.unit` to allow the operation or failing with `PhaseHooks.Denial()` to deny it. Supply a custom denial reason only if it is safe to return to clients. The operation includes the resolved request, parsed document, validated execution request, and `securityRequirements` identifying protected types and fields.

Authorization runs after validation and planning, including on cache hits. Combine checks with `++`. Every check must succeed, and a denial stops later checks. Return denials with `ZIO.fail`. Thrown exceptions are treated as unexpected failures and their messages are hidden.

### Connecting HTTP authentication

Build `secured.interpreter` once. With QuickAdapter, provide verified `RequestClaims` around the API handler for each HTTP request:

```scala
import caliban.QuickAdapter
import zio.{ IO, Scope }
import zio.http.{ Handler, Request, RequestHandler, Response }

def authenticatedApi(
  interpreter: GatewayInterpreter[RequestClaims],
  verify: Request => IO[Response, Option[VerifiedClaims]]
): RequestHandler[Any, Response] = {
  val api = QuickAdapter(interpreter).handlers.api

  Handler.scoped[Any] {
    Handler.fromFunctionZIO[Request] { request =>
      verify(request).flatMap { verified =>
        api(request).provideSomeLayer[Scope](
          ZLayer.succeed(new RequestClaims {
            def current: Task[Option[VerifiedClaims]] = ZIO.succeed(verified)
          })
        )
      }
    }
  }
}
```

Mount the returned handler on your GraphQL HTTP route. `verify` must validate credentials before returning claims. Return `None` for anonymous requests, or fail with an HTTP response such as 401 for invalid credentials.

Keep the claims layer inside the handler so concurrent requests receive their own user context. For Tapir, use the adapter's [`intercept` layer](../adapters.md#built-in-tapir-adapters). WebSocket subscriptions need [authentication at connection setup](subscriptions.md#authentication-and-monitoring).

## Subgraph request headers

Use remote-service configuration for [static credentials, token loading, and forwarding client headers](configuration.md#authentication-and-request-headers). Use `subgraphCall` to adjust the resulting headers across subgraphs:

```scala
import caliban.gateway.{ Gateway, PhaseHandler, PhaseHooks }
import caliban.gateway.PhaseHooks.Event
import zio.ZIO
import zio.http.Header

val headers = PhaseHooks.subgraphCall(
  PhaseHandler.incoming[Any, Event.SubgraphCall, Nothing] { event =>
    ZIO.succeed(event.copy(headers = Header.Custom("X-Gateway", "caliban") :: event.headers))
  }
)

val gateway = Gateway.compose(products, reviews) @@ headers
```

The headers returned by `subgraphCall` participate in query deduplication and stay the same across retries. Local calls ignore header changes.

Use `attempt` for headers that change per attempt, such as trace context. Return `event.copy(headers = ...)` from its incoming handler. These changes happen after deduplication and do not affect whether calls are shared. Callers that share a call all receive the response to the first caller's attempt headers, so never add credentials or other per-caller headers in `attempt`. Put them in `subgraphCall` or `withExecutionHeadersZIO`.

Subscriptions capture configured and effectful headers once. Both hooks run when opening the connection, which keeps those headers for its lifetime. Later enrichment calls run `subgraphCall` separately and can adjust the captured headers.

## Metrics and tracing

### Exporting metrics

Metrics are opt-in:

```scala
import caliban.gateway.{ Gateway, GatewayMetrics }

val gateway = Gateway.compose(products, reviews) @@ GatewayMetrics.hooks
```

The built-in metrics report request execution, preparation, subgraph calls, retries, cache activity, and subscriptions.

The hooks record ZIO metrics. They do not start an exporter or expose `/metrics`. To export to Prometheus, add a [ZIO Metrics connector](https://zio.dev/zio-metrics-connectors/getting-started):

```scala
libraryDependencies += "dev.zio" %% "zio-metrics-connectors-prometheus" % "2.5.4"
```

Add a metrics route alongside the gateway routes and provide the publisher layers:

```scala
import caliban.QuickAdapter
import zio._
import zio.http._
import zio.metrics.connectors.{ MetricsConfig, prometheus }
import zio.metrics.connectors.prometheus.PrometheusPublisher

val serve = ZIO.scoped {
  for {
    interpreter <- gateway.interpreter
    metrics      = Routes(
      Method.GET / "metrics" -> handler(
        ZIO.serviceWithZIO[PrometheusPublisher](_.get.map(Response.text))
      )
    )
    _           <- Server.serve(QuickAdapter(interpreter).routes("/graphql") ++ metrics)
  } yield ()
}

val application = serve.provide(
  Server.defaultWithPort(4000),
  prometheus.publisherLayer,
  prometheus.prometheusLayer,
  ZLayer.succeed(MetricsConfig(5.seconds))
)
```

Run `application` from your `ZIOAppDefault.run`. Configure Prometheus to scrape `/metrics` on port 4000. The publisher refreshes its snapshot every five seconds. See [ZIO's Prometheus guide](https://zio.dev/zio-metrics-connectors/metrics/prometheus-client/) for exporter details.

`caliban_gateway_requests_total` and `caliban_gateway_request_duration_seconds` cover queries and mutations from execution onward. They also count error responses for preparation failures, timeouts, and shutdown rejections, but the duration excludes preparation time. Track preparation separately with `caliban_gateway_preparation_duration_seconds`. Use `caliban_gateway_subgraph_call_duration_seconds` to find slow services and `caliban_gateway_operation_cache_total` to track cache hits and misses.

### Exporting traces

For OpenTelemetry tracing, add the optional module to `build.sbt`:

```scala
libraryDependencies += "com.github.ghostdogpr" %% "caliban-gateway-tracing" % "3.1.5"
```

Attach its hooks to the gateway:

```scala
import caliban.gateway.GatewayMetrics
import caliban.gateway.tracing.GatewayTracing

val gateway = Gateway.compose(products, reviews) @@
  (GatewayMetrics.hooks ++ GatewayTracing.hooks)
```

`GatewayTracing.hooks` requires a `zio.telemetry.opentelemetry.tracing.Tracing` service while handling requests. If your application uses the OpenTelemetry Java agent, provide a tracer from the globally registered SDK:

```scala
import caliban.QuickAdapter
import zio._
import zio.telemetry.opentelemetry.OpenTelemetry

val serve = ZIO.scoped {
  for {
    interpreter <- gateway.interpreter
    _           <- QuickAdapter(interpreter).runServer(4000, "/graphql")
  } yield ()
}

val application = serve.provide(
  OpenTelemetry.global,
  OpenTelemetry.contextJVM,
  OpenTelemetry.tracing("caliban-gateway")
)
```

Run `application` from your `ZIOAppDefault.run`. Configure the agent's exporter and service name when launching the JVM. Without an initialized SDK and exporter, these layers alone do not send spans anywhere. If you configure the SDK in Scala instead, use `OpenTelemetry.custom` and `OpenTelemetry.contextZIO`, following the [ZIO OpenTelemetry setup guide](https://zio.dev/zio-telemetry/opentelemetry/#setup).

Tracing covers the whole request, including preparation and remote calls. Subscriptions have setup and event spans. `QuickAdapter` propagates incoming trace headers.

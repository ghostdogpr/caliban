# Gateway

`caliban-gateway` is an embedded, code-first GraphQL gateway. It composes ordinary GraphQL services, Federation
subgraphs, and in-process Caliban APIs into a scoped `GatewayRuntime`. The runtime is a `GraphQLInterpreter`, so it can
be passed directly to `QuickAdapter` or any other Caliban HTTP integration.

## Dependencies and construction

```scala
libraryDependencies ++= Seq(
  "com.github.ghostdogpr" %% "caliban-gateway" % "3.1.5",
  "com.github.ghostdogpr" %% "caliban-quick"   % "3.1.5",
  "com.github.ghostdogpr" %% "caliban-gateway-tracing" % "3.1.5" // optional
)
```

Pinned SDL makes startup independent of a live schema endpoint. Omitting SDL acquires an ordinary schema through
introspection or a Federation schema through `_service`.

```scala
import caliban.QuickAdapter
import caliban.gateway._
import sttp.model.Uri
import zio._

val products = Subgraph.graphql(
  "products",
  Uri.unsafeParse("http://products/graphql"),
  "type Query { product(id: ID!): Product } type Product { id: ID!, name: String! }"
)

val reviews = Subgraph.federation(
  "reviews",
  Uri.unsafeParse("http://reviews/graphql") // acquires SDL through Federation _service
)

val program = ZIO.scoped {
  for {
    runtime <- Gateway.compose(products, reviews).build
    _       <- QuickAdapter(runtime).runServer(4000, "/graphql")
  } yield ()
}
```

`Subgraph.local(name, graph)` adds an in-process Caliban `GraphQL[R]`. Ordinary, Federation, and local sources can be
mixed in one composition; Federation is a property of a source, not of the whole graph.

Runnable local, ordinary remote, mixed-source, and Federation applications are available in the repository's
`gateway-examples` project.

## Lifecycle and limits

`Gateway` is an immutable, reusable description. Every `build` creates a new runtime owned by the surrounding
`Scope`. Closing that scope stops new admissions, drains accepted requests for the configured drain timeout, then
interrupts remaining cooperative work. Never let a built runtime escape its scope.

All default work is finite and configurable through `Gateway.withConfig` and `RemoteGraphQLConfig`:

- weighted operation cache, operation text/nesting/node bounds, planning candidates/expansions, and planning timeout;
- concurrent request and per-source call limits;
- one end-to-end request deadline and a shutdown drain deadline;
- schema-acquisition timeout, response-size, parsing-depth, and header bounds;
- source request/response sizes, timeout, replay-safe query retries, and bounded in-flight query deduplication.

`runtime.status` exposes lifecycle, admission, overdue-work, and operation-cache snapshots. The Quick adapter separately
bounds HTTP request and encoded response bodies; its default encoded response limit is 16 MiB.

## Errors, security, and introspection

Remote error messages are untrusted and are redacted by default. Only the `code` extension is retained. Opt in narrowly
with `GatewayConfig.withRemoteErrorDisclosure` or a source-specific `RemoteGraphQLConfig.withErrorDisclosure`; avoid
enabling messages unless every source is trusted.

Federation security directives such as `@authenticated`, `@requiresScopes`, and `@policy` are composed into validated
operation requirements. A graph containing those requirements fails to build unless an `OperationPolicy` is installed.
The policy receives the selected response paths, runtime type conditions, and conjunctive directive requirements. This
fail-closed rule prevents publishing security metadata without enforcing it.

The composed schema includes gateway-owned introspection. Disable it at the Quick boundary with
`QuickAdapter(runtime).configure(ExecutionConfiguration(enableIntrospection = false))`, or apply finer access control in
an `OperationPolicy` after validation. Source schema acquisition is a separate startup concern and uses only the explicit
acquisition headers and limits.

Federation `@composeDirective` metadata is preserved only when declarations and applications agree across sources.
Malformed, unsupported, or security-relevant directive composition fails with deterministic build diagnostics rather
than silently discarding semantics.

## Headers and environments

Execution headers have deterministic precedence: selected incoming headers, static source headers, effectful headers,
then GraphQL transport-owned headers. Forwarding is case-insensitive. Transport headers cannot be configured or
forwarded, and `forwardAllIncomingHeaders` remains an explicit opt-in.

```scala
trait Credentials { def token: UIO[String] }

val remoteConfig: RemoteGraphQLConfig[Credentials] =
  RemoteGraphQLConfig.default.withExecutionHeadersZIO(
    ZIO.serviceWithZIO[Credentials](_.token)
      .map(value => List(sttp.model.Header("Authorization", "Bearer " + value)))
  )
```

Environment requirements compose as intersections. A gateway containing `Subgraph[Credentials]`, a
`OperationPolicy[Authorization]`, and a `GatewayWrapper[Tracing]` builds without those services, while request execution
requires `Credentials with Authorization with Tracing`.

## Wrappers, tracing, and metrics

`GatewayWrapper` is the integration seam for request, routing, source-call, physical-attempt, retry, completion, cache,
admission, deduplication, and overdue events as well as outbound headers. `GatewayWrapper.Event.Attempt` represents every
physical HTTP attempt, including attempt zero; a source call remains the logical operation that can contain several attempts.
`outboundHeaders` changes semantic call identity before deduplication, while `attemptHeaders` adds transport context to each
physical attempt after identity selection.

One `wrap` call receives the event, its effect, and a typed function describing the completed result. Result metadata is
therefore recorded inside the span, metric, logger, or profiler scope that owns the event. Attach and combine wrappers with
the same style as Caliban middleware. Operation types, cache outcomes, admission kinds, and deduplication outcomes are closed
types rather than free-form labels, and a request deadline is reported as `Outcome.Timeout` after its completion event:

```scala
import caliban.gateway.GatewayMetrics
import caliban.gateway.tracing.GatewayTracing

val observed = Gateway.compose(products, reviews) @@
  (GatewayMetrics.wrapper |+| GatewayTracing.wrapper)
```

The optional OpenTelemetry wrapper lives in `caliban-gateway-tracing`, creates server/internal/client spans, extracts and injects W3C
trace context, and records bounded operation/source metadata. Raw GraphQL text and variables are not present in wrapper
events and are not captured by default. Gateway code does not automatically log expected GraphQL or remote errors.

Metrics are also a wrapper and are not collected unless `GatewayMetrics.wrapper` is attached. With no wrappers installed,
the gateway uses its direct cache, admission, source, and execution paths without telemetry clocks, metric-registry updates,
or metric-label allocations.

ZIO metric snapshots use these names:

| Metric | Bounded labels |
| --- | --- |
| `caliban_gateway_requests_total` | `outcome=success|error` |
| `caliban_gateway_request_duration_seconds` | `outcome`, `operation_type=query|mutation|subscription|unknown` |
| `caliban_gateway_requests_active` | none |
| `caliban_gateway_routing_duration_seconds` | `outcome` |
| `caliban_gateway_source_calls_total` | configured `source` name |
| `caliban_gateway_source_call_duration_seconds` | configured `source`, `outcome` |
| `caliban_gateway_source_calls_active` | configured `source` |
| `caliban_gateway_source_attempts_total` | configured `source`, `outcome` |
| `caliban_gateway_source_attempt_duration_seconds` | configured `source`, `outcome` |
| `caliban_gateway_source_attempts_active` | configured `source` |
| `caliban_gateway_source_request_body_size_bytes` | configured `source` |
| `caliban_gateway_source_response_body_size_bytes` | configured `source` |
| `caliban_gateway_retries_total` | configured `source` name |
| `caliban_gateway_operation_cache_total` | `result=hit|miss|wait` |
| `caliban_gateway_admission_total` | `kind=request|source` |
| `caliban_gateway_admission_wait_duration_seconds` | `kind=request|source`, `outcome` |
| `caliban_gateway_admission_active` | `kind=request|source` |
| `caliban_gateway_admission_waiting` | `kind=request|source` |
| `caliban_gateway_in_flight_deduplication_total` | `result=start|join|wait` |
| `caliban_gateway_overdue_requests_total` | none |

Source labels come from the finite set of names in one built gateway; no query text, field path, error message, or header
value is used as a label.

## Migration and deferred features

Migrate incrementally: start with one pinned ordinary source, verify `explain` output and HTTP behavior, add explicit
ordinary lookups or Federation sources, then introduce policies, forwarded headers, and schema acquisition. Treat
composition diagnostics and `explain` output as review artifacts. A Caliban local API can replace a remote source without
changing the runtime or HTTP integration.

This release supports queries and mutations over unary GraphQL-over-HTTP. Subscriptions, incremental delivery,
standalone routing, serialized graph packages, hot reload, Composite Schemas conformance, non-GraphQL source protocols,
and a general authorization/plugin product remain deferred. Existing Apollo Router or Gateway configuration is not
consumed directly; express source SDL, endpoints, header policy, limits, and authorization explicitly in Scala.

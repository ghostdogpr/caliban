# Choose embedded errors, resilience, observability, and extension contracts

Type: `grilling`
Status: `resolved`
Blocked by: 02, 04, 09, 10, 11, 12

## Question

What stable contracts should govern GraphQL versus source failures, retries and retry safety, cancellation, timeouts, telemetry, propagated context, sensitive data, limits, operation resolution, and user extensions in the embedded engine? Resolve which policies are built in, configurable, hooked, or deliberately deferred. HTTP status mapping, probes, exporters, process signals, standalone configuration, and deployment concerns are out of scope.

## Decisions

### Outcome and source-error boundaries

- The optimized execution surface distinguishes `RequestError`, `Executed`, and `GatewayFailure`. Request errors are client operation, operation-resolution, coercion, policy, or optional structural-limit rejections before execution. Executed results include valid source GraphQL errors, source failures integrated at field boundaries, partial data, and any deadline result that can still be completed reliably. Gateway failures cover admission rejection, draining, deadline expiry before a reliable result, and masked engine failure.
- Caller interruption remains ZIO interruption and produces no encoded response. The result classification is semantic adapter metadata, not an HTTP-status policy.
- A valid source GraphQL error retains its message, rewritten client path, and allowlisted extensions (`code` by default). Source locations and other extensions are omitted. Typed global configuration with per-source overrides may mask messages and change the extension allowlist; there is no effectful callback per error.
- Transport, protocol, timeout, and source-body-limit failures receive safe gateway-authored messages and stable codes at the call's merge boundary. Endpoint URIs, bodies, source names, stack traces, and throwables are not exposed to clients by default. Local Caliban errors retain Caliban's masking behavior.

### Telemetry direction

- Keep `caliban-gateway` telemetry-backend-neutral. Its stable instrumentation contract covers coarse request, frontend/cache/planning, source-call, integration, and lifecycle start/finish signals with a statically cheap no-op path. Sinks are non-blocking, bounded, and unable to fail or backpressure requests.
- Instrumentation fields are bounded semantic values, never raw queries, variables, headers, bodies, arbitrary attribute maps, HTTP objects, or engine representations. ZIO-native metrics may be emitted where they fit without another dependency.
- Distributed traces are required, but their implementation is deliberately scheduled at the end after the main execution features and measurements. The core contracts must preserve ZIO/FiberRef trace context and leave propagation/instrumentation seams intact from the start. Add an OpenTelemetry adapter module only when its extra dependency and real implementation are added; do not create a placeholder module.

### Resilience scope

- Circuit breaking is deferred. Hive and Cosmo provide built-in disabled-by-default breakers, while Apollo recommends external infrastructure and Fusion delegates to its host HTTP resilience pipeline; there is no majority for an engine-owned breaker.
- Do not publish breaker configuration or a placeholder abstraction. The internal remote-source boundary can add one later without changing planning semantics. Initial resilience remains the already-agreed opt-in, bounded, replay-safe remote-query retry policy; there is no hedging, adaptive routing, provider failover, or fallback-source execution.

### Operation resolution

- Publish one narrow build-installed `OperationResolver[-R]`. The default requires ordinary query text. A custom resolver may inspect operation identifiers/extensions, use `R` to retrieve or register a document, and return canonical query text or a typed request error.
- Resolution runs after admission, supplied-query byte checks, and iterative variable/extension structure bounds, but before parsing, within the request deadline. It cannot rewrite variables, operation name, schema, validation, or plans. Parsed/prepared cache identity continues to derive from resolved text and every schema/planning input.
- APQ, manifests, safelists, and external stores remain later implementations of this seam rather than engine concepts.

### Limit classes

- Always-finite resource limits cover query-text bytes, parser tokens/AST nodes, variable nodes/depth, planner explored states/plan nodes, per-source body bytes, total request-owned response memory, final encoded bytes, admission/queue size, and cache weights. Callers may configure a large finite bound but not disable these safety limits.
- Operation depth, alias count, root-field count, directive count, and similar organizational restrictions are configurable structural policies and may be disabled.
- Client-input and operation-limit violations are request errors before source dispatch. A source-body violation is a source failure at its merge boundary. Exhausting the final response budget is a gateway failure because projection cannot trust or expose partial internal state.
- Business cost control, rate limiting, and per-tenant quotas remain deferred.

### Stable gateway-authored errors

- Only gateway-authored errors receive gateway-owned stable `extensions.code` values. Caliban parsing/validation errors and valid source GraphQL errors keep their existing extensions rather than being rewritten into gateway codes.
- The initial coarse codes are `GATEWAY_OVERLOADED`, `GATEWAY_DRAINING`, `GATEWAY_TIMEOUT`, `GATEWAY_RESPONSE_TOO_LARGE`, `INTERNAL_SERVER_ERROR`, `SOURCE_REQUEST_ERROR`, `SOURCE_TRANSPORT_ERROR`, `SOURCE_TIMEOUT`, `SOURCE_PROTOCOL_ERROR`, `SOURCE_RESPONSE_TOO_LARGE`, `OPERATION_NOT_FOUND`, `OPERATION_NOT_ALLOWED`, and `LIMIT_EXCEEDED`.
- Model the set internally as a Scala enum and render stable uppercase strings. Codes may be added, but an existing code's meaning cannot change. Do not copy every vendor- or transport-specific competitor code.

### Retry policy

- `RemoteSubgraphConfig` may opt into a typed retry policy containing maximum attempts (including the initial attempt), maximum elapsed retry time, base/max delay, full-jitter exponential backoff, and typed retry conditions.
- Enabling the source policy treats downstream GraphQL query calls as replay-safe according to GraphQL operation semantics. Mutations, local calls, valid GraphQL responses, invalid requests/protocol, TLS certificate failures, and gateway defects are never retried.
- Default enabled-policy conditions are connection failures, attempt timeouts, and HTTP `500`, `502`, `503`, and `504`. Additional statuses such as `429` require explicit configuration; `Retry-After` is honored only within the remaining bounded budgets.
- One source permit covers every attempt and backoff. Maximum attempts, elapsed retry time, the source-call deadline, and the parent request deadline all bound the sequence, and every failed response body is released before another attempt.

### Deadlines and cancellation

- `GatewayExecutionConfig` defines one finite default request timeout. Execution overloads accept immutable `ExecutionOptions(timeout: Option[Duration])`; a request option may only shorten the configured default. The inherited `GraphQLInterpreter.executeRequest` uses the configured default.
- Per-source timeouts remain source configuration and may only narrow the request deadline. The semantic budgets never restart: request time includes admission, operation resolution, frontend/cache/single-flight, planning, permits, retries, integration, and projection; source time includes readiness, permit wait, attempts/backoff, body ingestion, protocol validation, and transfer of an owned result to the coordinator, but ends before coordinator integration mutates response state.
- Caller/client interruption and runtime-scope force-close interrupt active work and produce no response. A request deadline atomically disables late result delivery, marks the request overdue while it remains active, and produces `GATEWAY_TIMEOUT` once structured request work has exited. Cooperative work exits promptly; an uninterruptible user-provided effect—including local Caliban execution, operation resolution, operation policy, or effectful header policy—remains request-owned and can delay the timeout response, drain, or scope close because the JVM cannot safely detach or forcibly terminate it. A source deadline produces `SOURCE_TIMEOUT` at its merge boundary while independent work continues.
- Semantic early completion interrupts children that cannot contribute and returns the valid result. Draining lets accepted requests continue and rejects new requests with `GATEWAY_DRAINING`.
- Telemetry receives bounded cancellation reasons. There is no public cancellation callback, and interruption is never converted into ordinary source failure through blanket cause handling.

### Trace propagation and telemetry data

- Preserve active ZIO/FiberRef trace context across frontend/planning work, source-call child fibers, retries, and local Caliban execution from the first implementation, even though the tracing adapter is implemented last.
- The eventual adapter extracts validated W3C `traceparent`/`tracestate` at the server boundary and injects the active context after ordinary outbound header policy. Incoming trace headers are not blindly forwarded and cannot override the active span. Baggage propagation is disabled by default and later uses an explicit allowlist.
- Bounded default telemetry fields include operation type, source name, outcome/error code, durations, attempts, byte counts, cache result, plan statistics, and lifecycle state. Operation name/hash are not metric labels; after length bounding they may be trace attributes.
- Raw queries, variables, headers, bodies, upstream error messages, and response values are never captured by default. A trusted in-process sink may receive an internal `Cause` for local diagnostics, but adapters do not export it automatically or derive labels from its messages. Raw-document/variable capture is not an initial option.

### Operation policy and extension failures

- Publish one build-installed `OperationPolicy[-R]`. After planned-operation lookup and variable binding but before source dispatch, it receives a stable read-only view of operation type/name, selected client schema coordinates, preserved security metadata, and coerced inputs required for its decision. It may allow or reject the whole operation but cannot alter selections, visibility, variables, plans, source choice, or response data.
- A graph containing composed `@authenticated`, `@requiresScopes`, or `@policy` metadata fails `build` unless an operation policy is installed. There is no initial built-in authorization engine or field/schema filtering.
- Installing an operation resolver or policy widens the gateway's ZIO environment requirement through intersection types.
- Operation not-found/rejection and policy denial are request errors; denial uses `OPERATION_NOT_ALLOWED`. Resolver/policy backing-service unavailability is a gateway failure. Unexpected resolver/policy defects are masked gateway failures with the cause retained internally. Interruption remains interruption.
- Effectful outbound-header failure is a source-scoped failure using the additional stable `SOURCE_REQUEST_ERROR` code. Telemetry failures are isolated and cannot alter a request result. Local Caliban effect failures retain Caliban semantics.

### Instrumentation granularity

- The semantic span hierarchy is one request span, one planning span only on a plan-cache miss, one logical source-call span from readiness through permit/retries/ingestion/handoff, and build/composition spans outside request execution.
- Parsing, validation, cache hits, integration, projection, and retry attempts are span events or measurements. Never create a span per entity, field, mapping instruction, plan node, cache lookup, or retry attempt.
- Remote attempts inject their logical source-call context, so downstream attempt spans are siblings under that call. Local Caliban execution uses the source-call context directly.

### Configuration resolution

- Explicit source values replace corresponding global defaults. Per-request options may only narrow. Independent safety bounds resolve to the strictest value, and a source cannot raise a graph-wide hard cap.
- `Disabled` is explicit and `None` means inherit. Invalid or contradictory values fail `build`; valid but ineffective values produce deterministic build warnings.
- No operational option is merged dynamically from environment variables or untyped maps.

### Source extensions and retry preparation

- Drop top-level `extensions` from source GraphQL responses initially, including one-source graphs. This does not affect allowlisted extensions on individual GraphQL errors. A later client-response extension contract must be namespaced and deterministic.
- Evaluate effectful outbound headers and serialize the downstream GraphQL request once per logical source call. Reuse the immutable result across retries.
- Every attempt creates a fresh transport request/resource lease, injects current trace context after ordinary headers, acquires/releases its own response body, records attempt telemetry, and classifies the outcome. Input mapping, entity batching, header effects, and user hooks do not rerun per attempt.

### Runtime policy, extension composition, and observability surface

- Freeze operational policy at `build`. Changing retry, timeout, limits, error policy, resolver, or operation policy requires a new runtime. Per-request execution options only narrow allowed controls. Resolver/policy implementations may consult dynamic services through `R`, but their identity and semantics do not mutate.
- Install exactly one operation resolver and one operation policy. Users compose them explicitly: resolver fallback proceeds only on `NotFound`, while rejection/unavailability stops; policy conjunction requires every policy to allow and stops deterministically on the first rejection/failure. The engine does not manage ordered plugin lists.
- Keep the low-level instrumentation/tracing interface internal initially. Public observability is the stable ZIO metric catalog plus the end-phase supported OpenTelemetry adapter. Do not publish generic span handles, context tokens, event ADTs, callbacks, or a mutable runtime metrics snapshot before measurement establishes them.
- The stable metric catalog covers request count/duration/in-flight/admission/overdue, planning duration and cache behavior/weight, source duration/permit wait/attempts/bytes/outcome, integration/projection duration and output bytes, cancellation/deadline requested, user-provided effects still active after interruption, runtime state, and dropped telemetry. Labels are bounded to operation type, source, outcome/code, and cache kind.
- Automatically log only unexpected gateway defects and finalizer/resource-release failures through `ZIO.logErrorCause` with safe bounded annotations. Expected request/source/GraphQL errors, retries, limits, denials, and build diagnostics are not logged by the library.
- Successful build warnings are exposed as deterministically ordered `GatewayRuntime.buildDiagnostics: Chunk[CompositionDiagnostic]`. Errors still fail `build`; warnings are neither logged automatically nor wrapped around the runtime result.

### Bounded Caliban parsing

- Add a generally useful bounded `Parser.parseQuery(query, ParserLimits(...))` overload to Caliban core. It enforces token, nesting, and AST-node budgets while parsing, before an oversized AST is allocated. The existing Caliban API remains compatible; gateway execution always uses finite limits.
- Check supplied query bytes and inspect already-materialized variables and extensions iteratively before invoking `OperationResolver`; check resolved query bytes before parsing and coerce variables only after operation selection. Do not create a gateway-only lexer pre-scan or rely solely on post-parse AST counting. The acceptance oracle measures the bounded parser path.

## Policy placement

| Concern | Built in | Configurable | Named hook/adapter | Deferred |
| --- | --- | --- | --- | --- |
| Outcome and error mapping | Three outcome classes, safe gateway-authored errors, stable coarse codes | Global/per-source source-error masking and extension allowlist | None | Vendor-specific wording/code profiles |
| Deadlines/cancellation | Finite request budget, narrowing source budgets, structured interruption | Global/source timeouts; per-request shortening | None | Streaming-specific lifetime policy |
| Resilience | Bounded query-only retry mechanism, disabled by default | Per-remote-source attempts/time/backoff/conditions | None | Circuit breaking, hedging, adaptive/fallback routing |
| Limits | Always-finite resource limits and bounded admission | Numeric safety bounds; optional structural restrictions | None | Business cost, rate, and tenant policy |
| Operation input | Query-text resolver | Resolver selection on the description | `OperationResolver[-R]` | APQ/manifests/safelist/store implementations after the core path |
| Security metadata | Fail-closed activation | Policy selection on the description | `OperationPolicy[-R]` | Built-in authorization, field filtering, dynamic schemas |
| Observability | Stable ZIO metrics, safe defect logging, trace-ready context propagation | Metric/tracing adapter settings when implemented | Supported OpenTelemetry adapter implemented last | Public generic telemetry callbacks and raw-data capture |
| Response metadata | Gateway error extensions only | Source error-extension allowlist | None | Merging source top-level extensions; gateway client extensions |

## Contract sketch

```scala
enum GraphQLResponseKind {
  case RequestError
  case Executed
  case GatewayFailure
}

final case class ExecutionOptions(timeout: Option[Duration] = None)

enum GatewayErrorCode {
  case GatewayOverloaded
  case GatewayDraining
  case GatewayTimeout
  case GatewayResponseTooLarge
  case InternalServerError
  case SourceRequestError
  case SourceTransportError
  case SourceTimeout
  case SourceProtocolError
  case SourceResponseTooLarge
  case OperationNotFound
  case OperationNotAllowed
  case LimitExceeded

  def value: String // stable uppercase wire value
}

trait OperationResolver[-R] {
  def resolve(input: OperationInput): ZIO[R, OperationResolutionFailure, ResolvedOperation]
}

enum OperationResolutionFailure {
  case NotFound
  case Rejected(message: String)
  case Unavailable(cause: Option[Throwable])
}

trait OperationPolicy[-R] {
  def evaluate(input: PolicyRequest): ZIO[R, PolicyFailure, Unit]
}

enum PolicyFailure {
  case Denied(message: String)
  case Unavailable(cause: Option[Throwable])
}

final case class RetryPolicy(
  maxAttempts: Int,
  maxElapsed: Duration,
  baseDelay: Duration,
  maxDelay: Duration,
  retryOn: Set[RetryCondition]
)

trait GatewayRuntime[-R] extends GraphQLInterpreter[R, CalibanError] {
  def executeRequest(request: GraphQLRequest, options: ExecutionOptions): URIO[R, GraphQLResponse[CalibanError]]
  def executeEncoded(request: GraphQLRequest): URIO[R, EncodedGraphQLResponse]
  def executeEncoded(request: GraphQLRequest, options: ExecutionOptions): URIO[R, EncodedGraphQLResponse]
  def buildDiagnostics: Chunk[CompositionDiagnostic]
}
```

The sketch is semantic. The implementation may refine names and compact representations without weakening the failure, lifetime, safety, or extension boundaries above. Numeric defaults and regression budgets belong to the acceptance-oracle ticket.

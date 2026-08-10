# Review the consolidated gateway architecture and implementation handoff

Type: `grilling`
Status: `resolved`
Blocked by: 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15

## Question

Does the consolidated embedded-gateway architecture make every hard-to-reverse boundary, invariant, public contract, compatibility obligation, performance gate, module responsibility, test oracle, implementation phase, and risk explicit enough that implementation can begin without rediscovering design decisions? Review concrete code-first remote/Federation/local scenarios and either approve the handoff or surface the remaining decisions as new tickets.

## Canonical implementation asset

[Caliban Gateway Implementation Handoff](../IMPLEMENTATION-HANDOFF.md) is the self-contained document an implementation agent reads first. This ticket records the final review and rationale; the handoff document is the executable implementation brief.

The current handoff and files in [`tickets/`](../tickets/) supersede every sequencing, timing, and publication statement in this resolved decision. Use the material below only for architectural rationale and scenario expectations.

## Accepted handoff decisions

### Fresh implementation boundary

Implementation starts on a fresh branch from the then-current `series/3.x`, not from `wip_gateway`. The existing gateway branch and its `SuperGraph`/`SubGraph`, ZQuery, and `ResponseValue` implementation remain historical evidence only. There is no compatibility layer for that unpublished prototype. Individual tests or schema-conversion ideas may be reused only after review against this architecture.

### First executable milestone

The first production milestone is a complete code-first Federation path: compose a small representative graph, prepare and plan a basic operation containing an entity transition, execute remote calls, and return a semantically correct structured result. Follow it with an ordinary-remote plus local-Caliban mixed fixture so the composition, planner, executor, and response model do not become Federation-specific.

After these two walking skeletons, expand Federation support breadth-first through the current Federation Gateway Audit instead of finishing one internal subsystem in isolation.

### Production and reference paths

The production engine uses the planned-operation DAG and indexed response store from the beginning. It does not ship a temporary recursive `ResponseValue` architecture that would later need replacement. A deliberately small, obviously correct reference planner/executor may live in tests and provide differential/property oracles for production representations and scheduling.

### Handoff granularity

The final handoff groups context-sized tickets into product milestones, with each ticket's blockers defining exact execution order and safe parallelism. It does not prescribe numeric defaults before measurement.

### Walking-skeleton contracts

The first executable milestone implements Caliban-compatible structured execution over the production frontend, plan, coordinator, and response store. The optimized `executeEncoded` capability follows the response-store correctness oracle and reuses the same engine; only the compatibility boundary materializes `GraphQLResponse`.

Scoped resource ownership, structured interruption, one finite request deadline, and a finite remote-response byte cap are foundations of that milestone. Exhaustive parser and remote-protocol hardening, retry policy, operation resolver/policy, detailed metrics, and graceful drain follow the core semantic path without weakening its lifetime and cancellation invariants.

The Federation Gateway Audit and GraphQL Gateways Benchmark both expose the embedded runtime through Caliban's Quick Adapter. Quick must use the runtime's direct encoded unary path rather than materializing and re-encoding a `GraphQLResponse`; this is production behavior for Quick users, not a benchmark-only shortcut. The integration must not make `caliban-gateway` depend on zio-http or create a separate gateway HTTP artifact.

Add the Federation audit and GraphQL Gateways Benchmark adapters only at their current tickets, after the native code-first engine and production Quick encoded path exist. Profiling drives optimization after broad correctness and operational closure. OpenTelemetry tracing remains the final dependency-bearing implementation work.

### Optimized Quick Adapter integration

Add a generally useful optional unary capability to Caliban core: an `EncodedGraphQLInterpreter[-R, +E]` extends `GraphQLInterpreter[R, E]` and accepts a `GraphQLRequest` plus a negotiated `GraphQLResponseFormat`. It returns caller-owned encoded bytes together with bounded semantic outcome metadata sufficient for an HTTP adapter to preserve its status and media-type behavior. `GatewayRuntime` implements this capability as its native execution surface.

Quick Adapter recognizes this capability at construction. For ordinary unary HTTP requests it negotiates `application/json` versus `application/graphql-response+json`, invokes encoded execution, and builds the response without materializing or re-encoding a `GraphQLResponse`. Its configuration wrappers must surround both structured and encoded execution. Existing interpreters are unchanged, and uploads, subscriptions, and incremental results retain the structured path until a streaming encoding contract is designed.

Only Quick Adapter adopts this capability initially. The interface lives in core so gateway retains no zio-http dependency and other adapters can adopt it later without depending on gateway.

## Consolidated implementation handoff

### Final review decisions

- **Schema acquisition:** `build` keeps the environment `Scope` only. Acquisition accepts static headers; dynamic credentials are resolved before constructing the description or supplied via pinned SDL. Effectful `HeaderPolicy[R]` is runtime-only.
- **Incoming headers:** core owns a narrow `IncomingHeaders` FiberRef context. Quick populates it around either execution path; direct runtime overloads accept it explicitly and inherited interpreter calls default to empty. Merely forwarding incoming values does not widen `R`.
- **Quick configuration:** the handler retains the encoded capability before plain-interpreter decorators can erase it. Introspection, GET mutation, skip-validation, validations, and execution wrapping apply equally. Non-default validation bypasses plan caching without a stable discriminator; Quick does not replace gateway scheduling.
- **Benchmark gate:** keep the current upstream workload recognizable, but use a maintained comparison profile with cross-request response/in-flight subgraph-request deduplication off for all, setup excluded, semantic checking of every measured response, normalized resources/logging/readiness, and all four supported competitors. Required within-operation entity deduplication/coalescing and plan caches remain enabled. Unmodified upstream defaults are informational.
- **Existing modules/harnesses:** `caliban-tools` plus a gateway-owned sttp backend is an intentional production dependency; `caliban-federation` is test/example-only; `caliban-stitching` remains independent. Audit and benchmark adapters are non-published projects modeled on `apollo-compatibility`.
- **Tracing:** preserve trace context in core and add the real optional `caliban-gateway-tracing` dependency-bearing module only in the final implementation phase. Do not invert the existing tracing module's dependency direction.
- **Public descriptions:** use final immutable private-constructor classes with fluent copy methods, not opaque public types or public case classes.
- **Scala versions:** start on the repository's current JVM Scala matrix and keep common code Scala 2-compatible when inexpensive. Do not sacrifice a substantive Scala 3 correctness, API, maintainability, or measured-performance benefit merely to cross-build; provisionally verify the foundation, keep publication disabled, and narrow only for concrete recorded evidence re-confirmed on real engine code before Ticket 53.

### Second review decisions

- **Shared-source public shape:** put fluent operations directly on the final `Subgraph` and `Gateway` classes; remove Scala-version-specific extension facades and the one-case `GatewayDescriptionError`. `Gateway.from` accepts `NonEmptyChunk`.
- **Core request context:** `IncomingHeaders` and its FiberRef live in Caliban core. Quick installs the fallback; an explicit runtime argument wins. Quick also bounds the raw body before decoding and gateway bounds supplied query text before calling a resolver.
- **Structured/encoded HTTP parity:** core owns a generic request/executed/server outcome and a structured `CalibanError.ResponseError`. Both Quick paths use the same typed `405` plus `Allow: POST` mutation-over-GET response, `400` GraphQL-response request error, `200` executed, and `503`/`504`/`500` gateway-failure mapping; SSE remains structured. The finite unary body limit and `400` to `405` change are reviewed Quick adapter behavior changes requiring release notes. Gateway v1 emits no cache-control directive because cross-source cache-policy aggregation is deferred; the generic encoded field remains for ordinary interpreters and future work.
- **Timeout honesty:** source timeouts end at validated-result handoff before coordinator mutation. A request deadline atomically disables late result delivery, marks the admitted request overdue while its tree remains active, and interrupts the request tree. Cooperative work returns the timeout envelope promptly; any still-uninterruptible user-provided effect remains request-owned with its environment, acquired permits/resources, and accounting until it exits, so it may delay the response. This includes local Caliban execution, operation resolution, operation policy, and effectful header policy; no request child or callback is detached or reparented.
- **Remote ownership:** v1 gateway creates/owns/closes its pooled backend; no public sttp injection or borrowed-backend claim. `SchemaSource` alone owns an acquisition-endpoint override. Acquisition is independently bounded and redirects default off.
- **Heterogeneous metadata:** transformations are subgraph-local and preserve extracted Federation capabilities through coordinate translation. Explicit metadata is always authored against the transformed gateway-visible schema independent of fluent call order, then translated inversely for source execution. A typed `ArgumentMapping` supports leaf paths, compound input objects, and automatic native-batch list lifting. Every ordinary provider must declare a shared field shareable.
- **Cross-build/publication:** the foundation provisionally verifies the matrix, Ticket 34 re-confirms it on real engine code, and publication remains disabled until Ticket 53 completes every release gate.
- **Moving external inputs:** audit, benchmark, competitors, and gate configuration resolve through a checked-in harness version file; refresh is a reviewed commit and each run still records a full manifest.
- **Performance exception:** 85% remains the standing release gate. Only an expiring checked-in maintainer exception with profiles, measured gap, rejected remedies, and green correctness/operational suites can permit a release below it.

### Target and source decisions

Build a new JVM `caliban-gateway` library from the then-current `series/3.x`, cross-built where the implementation and dependencies permit. It is an embedded, code-first, scoped gateway for ordinary remote GraphQL, Federation-enabled remote GraphQL, local Caliban, and mixtures of those source kinds. Deeper rationale remains in the resolved tickets for [module ownership](05-module-boundaries.md), [composition](06-composition-and-artifact.md), [operation preparation](07-operation-front-end.md), [planning](08-planning-model.md), [execution](09-execution-engine.md), [response assembly](10-response-assembly.md), [execution sources](11-execution-sources.md), [runtime lifecycle](12-embedded-runtime-lifecycle.md), [public API](13-public-api-and-configuration.md), [operational semantics](15-operational-semantics.md), and [acceptance](14-acceptance-oracle.md). The canonical handoff and this final-review section supersede earlier ticket wording where the review changed a decision.

The old `wip_gateway` history is not an implementation base or compatibility target. The first implementation change adds the module afresh; it does not carry `SuperGraph`, `SubGraph`, ZQuery routing, `Extend`, or the old `ResponseValue` execution model into the new branch.

### Repository and dependency changes

| Area | Required change |
| --- | --- |
| Caliban core | Add bounded parser limits; generic response outcome/structured server error; optional unary `EncodedGraphQLInterpreter`; and `IncomingHeaders` FiberRef context. Keep existing parser/interpreter method signatures source-compatible and cross-built; explicitly review the new `CalibanError` sealed subtype's exhaustivity impact. |
| Quick Adapter | Select the encoded capability when present, preserve existing structured behavior otherwise, and keep configuration, media negotiation, GET semantics, and HTTP status mapping equivalent across both paths. |
| `caliban-gateway` | Add one JVM module containing public descriptions/runtime and all private composition, planning, execution, source, and response machinery; start on the repository Scala matrix, keep publication disabled until Ticket 53, and narrow only for recorded evidence. |
| Remote transport | Begin with one gateway-created/owned scoped pooled sttp client4 JVM backend behind a private source-transport seam. Schema acquisition and execution share it; no public backend injection. A measured replacement changes no public interface. |
| Test support | Use test-only Quick Adapter, audit, benchmark, deterministic source, and fault-injection dependencies. They do not create published gateway HTTP modules. |
| Tracing | Add the real OpenTelemetry integration last in optional `caliban-gateway-tracing`; do not add a placeholder dependency to gateway core or make the existing tracing module depend on gateway. |

Public API lives under `caliban.gateway`. Composition, frontend, topology/planning, execution, source adapters, and response machinery are private gateway packages. Their representations may evolve without compatibility commitments. Only a real dependency or compatibility boundary creates another published artifact.

### Non-negotiable engine invariants

1. `Gateway.compose` is pure; scoped `build` creates one immutable graph generation and either returns a complete runtime or releases all acquired resources.
2. Runtime environment requirements remain in `R` and are supplied per execution. Build does not capture local-graph, header-policy, resolver, or policy services.
3. Every cross-source transition is explicit. Same names never infer identity, lookup, ownership, shareability, or failover.
4. A cached planned operation is statically valid, variable-independent, complete, deterministic, and verified before execution. Runtime never discovers a route or replans after failure.
5. One request-local coordinator owns response integration and mutable request state. Source fibers return owned results; they never mutate the response store.
6. Source buffers, entity batches, response-store arrays, and projection buffers cannot outlive the request. The direct encoded result is caller-owned after execution completes.
7. Valid source GraphQL results, including partial data and errors, remain results. Transport/protocol/limit failures are integrated at their planned field boundary; caller interruption remains interruption.
8. Admission, parsing, schema/source bodies, response memory, output, concurrency, caches, and deadlines are always finitely bounded. Request options may only narrow configured bounds. No gateway limit claims to bound the duration of arbitrary uninterruptible user code.
9. Scope close interrupts gateway-owned and request work. Short atomic handoffs may be masked, but waits, source calls, callbacks, and finalization are not placed in broad uninterruptible regions. Any user-provided effect that ignores interruption remains request-scoped, active, and observable until it actually exits; its request is also overdue only after that request's deadline has fired. Response completion, drain, and scope close may therefore wait rather than detach it or release borrowed services underneath it.
10. `executeEncoded` and `GraphQLInterpreter.executeRequest` use one engine and have equivalent GraphQL semantics. Quick's optimized path must not change content negotiation, method handling, or error status behavior.

### Implementation sequence

The current [`IMPLEMENTATION-HANDOFF.md`](../IMPLEMENTATION-HANDOFF.md) and numbered files in [`tickets/`](../tickets/) are the sole implementation sequence. This resolved decision preserves architectural rationale and scenario oracles only; its former slice schedule was removed after the plan was reorganized around a structured code-first MVP.

### Canonical scenario checks

| Scenario | Required behavior |
| --- | --- |
| One ordinary remote graph | One prepared source call; no Federation assumptions; normal GraphQL errors and null completion. |
| Federation Products to Reviews | One explicit entity transition, stable deduplication/fan-out, alias-aware result/error correlation. |
| Remote Products plus local Pricing | One planner and coordinator run remote and local calls concurrently after Products; no local serialization round trip. |
| Ordinary native batch lookup | Declared ordered or key correlation governs reordered, missing, and null results; position is never guessed. |
| Two top-level mutations | The complete first routed subtree finishes before the second starts, while nested work within one root may be concurrent. |
| Partial source error | Valid partial data and rewritten errors integrate; nullability propagates only to the nearest nullable boundary; independent work survives. |
| Source timeout | Before validated-result handoff the call becomes a safe source failure at its merge boundary; after handoff coordinator integration is governed only by the overall deadline, which never restarts. |
| Caller interruption or scope close | Cooperative work is interrupted/released; any still-uninterruptible user-provided effect remains request-scoped, active, and observable, with its environment, acquired permits/resources, and accounting retained until it exits. It is overdue only if its request deadline has also fired, and no response is fabricated. |
| Drain race | Admission and active registration are atomic: the request is either active and allowed to finish or rejected as draining. |
| Quick encoded execution | The bytes come directly from projection, while media type, status, GET rules, and GraphQL semantics match the structured path. |

### Principal risks and containment

- **Federation composition breadth:** treat the audit as the breadth-first backlog and keep original source coordinates in every diagnostic; do not encode Apollo JOIN artifacts as the internal model.
- **Planner correctness:** retain a rich immutable planning graph, verify before lowering, and differentially test small cases against the reference instead of debugging dense arrays directly.
- **Response ownership and null/error semantics:** centralize mutation in one coordinator, use explicit document leases, and property-test projection against structured expected results under varied completion orders.
- **Quick encoded parity:** test the same requests against encoded and structured interpreters for both response media types and HTTP methods before using the path in benchmarks.
- **JVM transport performance:** keep transport private, measure pooled sttp/JDK behavior early, and change backend or specialize ingestion only when profiles identify it as limiting.
- **Local Caliban overhead:** begin with the normal interpreter for semantic fidelity; add a generally useful prepared local-call seam to core only if the benchmarked mixed/local path requires it.
- **Cancellation/finalization bugs:** use scoped child fibers and explicit ownership first, narrow masking to atomic handoffs, and fault-test every suspension boundary.
- **Premature optimization:** retain semantic explanations, reference tests, and acceptance gates so compact representations can change without changing public behavior.

### Handoff completion condition

Implementation may begin when the user approves this consolidation without another architectural ticket. Numeric defaults, private class/file layout, concrete sttp backend tuning, and optimization choices are deliberately implementation-time measurements behind already fixed seams. Standalone routing, serialized artifacts, hot reload, CLI/native packaging, subscriptions, incremental delivery, uploads, other protocols, Composite Schemas conformance, and the old prototype API remain outside this handoff.

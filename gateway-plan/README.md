# Caliban Gateway Plan

## Destination

A decision-complete architecture and implementation specification for a production-capable, ZIO-native, embedded GraphQL gateway based on Caliban. It composes heterogeneous ordinary GraphQL, Federation, and local Caliban sources in code and includes compatibility and performance acceptance gates sufficient to begin implementation without unresolved architectural decisions.

## How to use this plan

- The canonical entry point is the [implementation handoff](IMPLEMENTATION-HANDOFF.md). Read it completely before beginning implementation.
- To assign work, give the agent both the handoff and one file from [tickets](tickets/). The ticket bounds the change; the handoff owns cross-cutting contracts and invariants.
- Follow the handoff milestones and each ticket's blockers. Ticket numbers group the work, but blockers control the exact order when work within a milestone can proceed in parallel. Use [decisions](decisions/) and [research](research/) only when the ticket or handoff needs deeper rationale.
- Treat assigned ticket numbers as stable identifiers. Reprioritize by changing blockers or milestone prose rather than renumbering existing tickets; append a new numbered ticket when later evidence exposes additional work.

## Design summary

- Treat GraphQL, GraphQL-over-HTTP, Federation, and JOIN specifications as authoritative. Where they are ambiguous, study Apollo Router, Hive Router, Cosmo Router, and Hot Chocolate Fusion and bias toward the majority behavior.
- Historical comparative source snapshots are recorded in [SOURCES.md](SOURCES.md). Revisit decisions against current competitor versions when implementation evidence requires it; do not treat the research snapshots as release targets.
- Exploit Scala and ZIO directly. Start on Caliban's current JVM Scala matrix and preserve Scala 2 cross-building when inexpensive; use Scala 3-only features when they deliver a concrete benefit that cannot reasonably be isolated. Reuse Caliban wherever it satisfies the required semantics and performance; replace or build specialized paths where measurement shows it cannot.
- The existing `gateway` module is optional evidence and imposes no compatibility constraint.
- The product in this plan is only the embedded `caliban-gateway` library. Serialized packages, a standalone router, an offline CLI, hot reload, and their module/package decisions require a separate design effort after the in-memory model is implemented and measured.
- One embedded graph may mix ordinary remote GraphQL, Federation-enabled remote GraphQL, and in-process Caliban sources. Unique roots coexist automatically; every cross-source transition needs explicit identity and recall metadata.
- Initial protocol delivery covers queries and mutations over GraphQL-over-HTTP. Streaming concerns must be designed for, with subscriptions and incremental delivery staged next.
- Initial public acceptance targets the current Federation Gateway Audit for compatibility and the current GraphQL Gateways Benchmark for performance. Resolve selected versions in a checked-in harness version file and each run manifest, refresh through reviewed commits before release, and target correct useful throughput within roughly 15% of the leading current router; latency and memory are reported without an initial hard threshold. An evidence-backed expiring maintainer exception is the only release escape hatch.

## Decisions so far

<!-- Resolved ticket pointers are appended here. -->

- [How do leading gateways divide composition, artifacts, planning, execution, and reload?](decisions/01-leading-gateway-architecture.md) — Reuse the competitors' immutable graph-generation and generation-owned-cache boundaries in the embedded runtime; retain their artifact/reload findings only as research for a later standalone effort.
- [Which GraphQL and Federation semantics define the compatibility baseline?](decisions/02-compatibility-baseline.md) — Target GraphQL September 2025, GraphQL-over-HTTP JSON, core Federation 2 through entity interfaces, and JOIN v0.3 for unary v1; stage streaming and advanced Federation features explicitly.
- [Which architecture choices dominate gateway hot-path performance?](decisions/03-performance-architecture.md) — Use schema-scoped single-flight plan caches, primitive-indexed dependency execution, pooled transports, and a specialized raw-capable response store; reuse Caliban semantics behind measured seams.
- [Which operational and extensibility capabilities form the production baseline?](decisions/04-production-baseline.md) — Put semantic lifecycle, limits, instrumentation, and typed hooks in the engine; put config, delivery, probes, drain, exporters, and packaging in the host, with persisted operations and conservative resilience as optional modules.
- [What do the cross-gateway audit and benchmark establish as acceptance evidence?](decisions/17-cross-gateway-audit-and-benchmark.md) — Research established the strengths and limitations of both external projects at historical snapshots. The later acceptance decision uses their current upstream shapes as recognizable targets instead of building the broader custom forks proposed here.
- [Choose the gateway module boundaries and ownership model](decisions/05-module-boundaries.md) — Implement one deep `caliban-gateway` library, keeping pipeline stages private and adding published modules only when a real dependency, runtime target, or compatibility surface requires them; prior router/CLI names are not commitments in this map.
- [Choose the embedded composition contract and source normalization](decisions/06-composition-and-artifact.md) — Use one code-first `Gateway.compose` over heterogeneous sources, automatic unique-root coexistence, and explicit source-owned key/lookup metadata for every cross-source transition; defer serialization until the engine model is proven.
- [Choose the execution-source and transport contract](decisions/11-execution-sources.md) — Expose only built-in source constructors in v1; internally prepare source-specific calls ahead of execution, preserve ZIO context and environments, batch only within one request, and keep remote transport, local execution, and result representation specialized behind the engine.
- [Choose the operation parsing, validation, and normalization strategy](decisions/07-operation-front-end.md) — Reuse Caliban parsing and validation semantics, produce a compact variable-independent `PreparedOperation`, and cache the resulting `PlannedOperation` per graph generation; keep parsed documents schema-independent and bind request variables through prepared slots on every execution.
- [Choose the distributed planning model and algorithm](decisions/08-planning-model.md) — Compile one deterministic, cost-selected plan for all source kinds; lower rich planner state into a verified primitive-indexed `SourceCall`/`Condition` DAG with compiled mappings, dynamic entity batches, strict mutation fences, and a stable semantic explanation.
- [Choose the execution engine and concurrency semantics](decisions/09-execution-engine.md) — Run a plan through one scoped request-local coordinator with source-call child fibers, single-owner integration, item-level partial-failure eligibility, bounded admission and per-source permits, inclusive deadlines, and conservative replay-safe retries.
- [Choose the response representation and assembly algorithm](decisions/10-response-assembly.md) — Assemble into a request-owned primitive-indexed hybrid store, materializing routing/nullability values while retaining final-only JSON as source-buffer references; compile null completion and error-path mapping, and project directly in client order without a second JSON tree.
- [Choose embedded runtime ownership and shutdown semantics](decisions/12-embedded-runtime-lifecycle.md) — Build one scoped, concurrently shared runtime with gateway-owned transport/resources, request-supplied environments, runtime-owned weighted caches and single-flight, atomic admission/overdue accounting, structured ownership of every uninterruptible user-provided effect, and a `Running → Draining → Closed` lifecycle.
- [Choose the public embedded Scala API and configuration model](decisions/13-public-api-and-configuration.md) — Expose one immutable, code-first `Gateway.compose` description and scoped `GatewayRuntime` that is Caliban-interpreter compatible while retaining a direct encoded path; use typed source/configuration metadata, ZIO environment intersections, and narrow extension surfaces without exposing engine internals.
- [Choose embedded errors, resilience, observability, and extension contracts](decisions/15-operational-semantics.md) — Classify request, executed, and gateway outcomes explicitly; enforce finite deadlines and resource limits; keep retries conservative; expose only operation-resolution and fail-closed policy hooks; provide bounded metrics now and trace-ready OpenTelemetry integration last.
- [Define the compatibility and performance acceptance oracle](decisions/14-acceptance-oracle.md) — Target all in-scope cases in the current Federation Gateway Audit through the code-first adapter and at least 85% of the leading current correct throughput in the GraphQL Gateways Benchmark; record exact run versions, keep extra project coverage narrow, and defer a custom benchmark suite.
- [Review the consolidated gateway architecture and implementation handoff](decisions/16-architecture-handoff.md) — Approved the canonical handoff after self-review/grilling passes resolved core request/HTTP metadata, Quick compatibility changes, ordinary methods, typed compound lookup mappings, transformed-coordinate semantics, gateway-owned transport/acquisition, result-handoff timeouts, honest structured handling of uninterruptible user effects, overdue status, reproducible external versions, benchmark exception policy, tracing, and conditional Scala 2 cross-building.

## Not yet specified

- Subscription and incremental-delivery execution need a later milestone design once unary execution and cancellation semantics are fixed.

## Out of scope

- Claiming conformance with the preliminary GraphQL Composite Schemas specification; the initial heterogeneous model deliberately borrows its key, lookup, mapping, shareability, and satisfiability concepts.
- gRPC, REST, or other non-GraphQL remote execution protocols in the initial implementation.
- Standalone artifact loading, dynamic reload, and production control-plane composition.
- A built-in authorization policy system, response-cache product, traffic-shaping product, or plugin marketplace in the first production milestone.
- Compatibility with the existing `gateway` module's public API or implementation.
- Native composition CLI packaging, execution-artifact serialization, a standalone router, hot reload, and offline CLI design depend on the proven embedded model and belong to a later design effort.

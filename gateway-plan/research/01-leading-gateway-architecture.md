# Leading gateway architecture: composition, generations, planning, execution, and reload

Research date: 2026-08-07

## Scope and method

This note compares the pinned source snapshots for Apollo Router, Hive Router, Cosmo Router, and Hot Chocolate Fusion. It treats source code, first-party repository documentation, and formats owned by those projects as primary evidence.

The central question is not whether a gateway can route across many federation-enabled subgraphs—it must—but where schema composition ends and request routing begins, what crosses that boundary, and how the runtime replaces one graph generation with another without mixing their state.

## Executive answer

The four implementations agree on the important lifecycle:

1. **Compose outside the request path.** Production routing consumes an already composed artifact. Development workflows may watch source schemas and repeatedly produce that artifact, but request execution does not introspect and compose subgraphs.
2. **Build a complete immutable runtime generation before publication.** A generation contains the API/schema view, routing metadata or planner substrate, source clients/executors, policy-derived state, and schema-sensitive caches. A request selects one generation and stays on it.
3. **Plan operations at runtime and cache the result per generation.** None of the four ships every possible query plan in the composition artifact. The artifact makes planning possible; each normalized client operation is planned lazily (or prewarmed from known operations) and placed in a bounded cache coupled to the schema/planner generation.
4. **Publish the generation in one step and retain the last valid generation on build failure.** Apollo, Hive, and Cosmo make this explicit in reload code. Fusion also constructs and warms the replacement before assigning it, though its update loop has weaker visible failure recovery.
5. **Stop new work on the old generation, let ordinary in-flight work drain, then retire generation resources.** Long-lived streams are not allowed to hold an obsolete graph forever: Apollo and Hive explicitly terminate subscriptions on schema update, Cosmo cancels old graph-server contexts after draining ordinary requests, and Fusion disposes the prior executor after a configured grace delay.

The main non-majority split is the artifact itself. Apollo and Hive consume interoperable Federation/JOIN supergraph SDL and derive runtime structures at load. Cosmo emits a versioned router execution configuration containing engine and subgraph configuration. Fusion emits a versioned `.far` archive containing a composite execution schema plus settings. This is a **2–2 split**, not evidence for either a bare-SDL-only design or a proprietary-only design.

For the Scala gateway, the strongest synthesis is: keep standards-compatible Federation/JOIN SDL as an accepted interchange and diagnostic form, but make the production deployment unit a versioned, checksummed execution artifact that also contains a normalized routing IR and source descriptors. Both inputs should compile through exactly one `GraphGeneration` builder. Publish an immutable generation atomically; lease it once per request; keep all schema-sensitive caches inside it; retire it only after unary request leases drain; and explicitly terminate/reconnect streaming operations on schema change. This preserves interoperability while moving repeatable, expensive work out of startup and reload where measurements justify it.

## Comparison matrix

| Concern | Apollo Router | Hive Router | Cosmo Router | Hot Chocolate Fusion | Majority / conclusion |
| --- | --- | --- | --- | --- | --- |
| Production composition | GraphOS/Rover, not in-process router composition [A1] | External producer; router loads supergraph SDL [H1][H2] | TypeScript control plane/composition builds `RouterConfig` [C1] | Nitro CLI composes source schemas to `.far` [F1] | **4/4 offline/control-plane composition** |
| Runtime artifact | Federation/JOIN supergraph SDL [A1] | Federation/JOIN supergraph SDL [H1][H2] | Versioned protobuf/JSON router execution configuration with composed/client SDL, field config, subgraphs, compatibility version [C1] | Versioned archive containing an execution schema and settings; archive advertises supported gateway formats [F1][F2] | **2–2 artifact richness split; 4/4 composed artifact** |
| Load-time derived state | Parsed schema, query planner, pipeline/plugins, subgraph services [A2][A3] | `Supergraph`, planner/public schema snapshot, executors, auth metadata, caches [H2][H3] | Complete `graphServer`/`graphMux`, transports, engine, caches, telemetry [C2][C3] | Schema-scoped DI provider, planner/compiler, pipeline, clients, plan cache [F4][F5] | **4/4 complete generation before serving** |
| Publication | HTTP service factory/server restart after successful construction [A2] | One `ArcSwap` value containing snapshot and runtime [H2] | One `atomic.Pointer` state containing mux and graph server [C2] | Replace registered executor after build and warmup [F4] | **4/4 generation-level replacement; 3/4 visibly atomic primitive/factory handoff** |
| Invalid reload | Keep old pipeline and retry [A2] | Log failure; do not update `ArcSwap` [H2] | Keep old server [C4] | Assignment occurs only after build/warmup, but an exception can fault the visible fire-and-forget update loop [F4] | **4/4 last valid remains serving; Fusion recovery is a caveat** |
| Query planning | Lazy, cached, deduplicated; optional warmup/reuse [A3] | Lazy from normalized operation, cache insertion coalesces work [H4] | Runtime engine planner with cache tied to engine config [C3] | Lazy planner with explicit single-flight and schema-scoped cache [F5] | **4/4 request-time planning with generation-owned cache** |
| Plan execution | Typed recursive plan tree: sequence, parallel, fetch, flatten, condition, defer [A4] | Typed recursive plan tree and parallel/sequence executor [H4] | Engine/planner per `graphMux`; execution config describes data sources and fields [C1][C3] | Operation plan/compiler/executor pipeline per request executor [F4][F5] | **4/4 separate planning and execution stages** |
| Cache ownership | `CachingQueryPlanner` owns schema and cache; replacement can prewarm from previous operations [A3] | Validation/normalization/plan caches live on `RouterSupergraphRuntime` and die with it [H2] | All operation caches live on the graph mux because plans are engine-config-specific [C3] | Plan cache is a schema-service singleton, therefore executor-generation scoped [F5] | **4/4 schema-sensitive cache scoped to generation** |
| Ordinary in-flight requests | Server/factory rollover preserves graceful connection handling [A2][A6] | Request holds snapshot/runtime; retirement does not invalidate ordinary in-flight work [H2][H3] | Atomic swap prevents new old-generation requests, then waits for old in-flight counters [C2][C3] | Prior executor gets an eviction grace interval before disposal [F4] | **4/4 drain/grace rather than immediate cancellation** |
| Long-lived streams | Active subscriptions terminated with schema-reload error [A5] | Retirement token terminates subscription producer [H3][H5] | WebSocket/hijacked connections close when old graph context is cancelled after drain [C2][C3] | Prior executor disposed after grace; streams retain request resources until result disposal [F4][F6] | **3/4 explicit schema-change termination; do not let old streams pin a generation indefinitely** |

## Shared architectural boundaries

### 1. Composition and routing are separate products

Apollo is the most explicit: Router does not support `serviceList`/`IntrospectAndCompose`; GraphOS or Rover creates a supergraph schema, and separation avoids a composition failure causing router downtime [A1]. Hive Router begins with a supergraph source and parses/builds planner state from received SDL rather than discovering subgraphs [H1][H2]. Cosmo's control-plane worker serializes successful composition into a structured router artifact and includes a compatibility version before the Go router sees it [C1]. Fusion's `compose` command turns source schemas into an archive, while `run` and gateway registration consume that archive [F1].

This boundary is a GraphQL-domain decision, not a Rust/Go/.NET accident. Composition validates the global contract and can fail for reasons unrelated to a particular request. Keeping it outside the live request router makes promotion, rollback, audit, signatures, and compatibility checks possible.

### 2. The unit of consistency is a graph generation

All schema-derived and routing-derived values must change together. Hive documents this invariant directly: its configured slot contains the schema owner, snapshot, and runtime in one `ArcSwap`, specifically so a request cannot see a mismatched generation [H2]. Cosmo stores the HTTP mux and owning graph server in one atomic state for the same reason [C2]. Apollo constructs the schema and entire router-service factory before restarting the server on it [A2]. Fusion creates schema services, planner, pipeline, and executor, warms them, and only then replaces the registered executor [F4].

A generation is more than a schema AST. It includes source endpoint/capability configuration, planner indexes, client pools/transports, authorization or other schema-derived metadata, and cache identity. Allowing any of those to update independently creates a mixed-generation request and invalid plans even if each individual reference is thread-safe.

### 3. The composition artifact is not the operation plan

Each system does additional work after artifact load and again for each previously unseen operation. Apollo's supergraph service invokes its caching planner before its execution service, and its executor walks a plan tree with explicit sequence, parallel, flatten, fetch, condition, subscription, and defer nodes [A3][A4]. Hive builds its `Planner` when constructing `Supergraph`, then computes a plan from the normalized operation on a cache miss and executes typed parallel/sequence nodes [H3][H4]. Cosmo's artifact contains an engine configuration, but each graph mux still owns an execution-plan cache explicitly coupled to that engine configuration [C3]. Fusion builds an `OperationPlanner` per schema services scope and its middleware single-flights and caches operation plans [F5].

Therefore composition should emit enough normalized routing information to make planning fast and deterministic, but it should not be conflated with operation planning. Persisted operations can be preplanned or prewarmed as an optimization, not as the only execution model.

### 4. Schema-sensitive caches belong to the generation

The strongest unanimous rule is that validation, normalization, and planning caches cannot be globally keyed only by operation text. Apollo's caching planner carries the schema, subgraph schemas, planner-config hash, and cache as one object [A3]. Hive places validate, normalize, plan, and demand-control state directly on `RouterSupergraphRuntime` [H2]. Cosmo explains that it creates a plan cache for each operation planner because a cache shared across engine configurations can return invalid plans [C3]. Fusion registers its plan cache as a singleton in **schema services**, so replacement builds a new cache with the new executor [F5].

Cross-reload reuse is deliberately conservative. Apollo can inspect the previous cache to prewarm and has an explicitly experimental plan-reuse option [A3]. Cosmo preserves expensive operation inputs/fallback state and can reuse whole unchanged feature-flag muxes, rather than treating arbitrary old plans as valid under a changed engine configuration [C3][C5]. Hive's ordinary path drops the old runtime caches after generation retirement [H2]. Fusion creates new schema services and a new cache [F4][F5]. The majority-safe behavior is to carry forward **operation popularity/input keys** and replan against the new generation; direct plan reuse requires proof that the artifact digest, planner build/options, source capabilities, and policy inputs are identical.

### 5. Reload is build → validate/warm → publish → retire

Apollo parses and creates the new pipeline before the server rollover; if creation fails while the old server exists, it logs that it is still running the previous configuration and schedules retry [A2]. Hive parses SDL, runs hooks, creates `Supergraph`, builds the router runtime, and stores the new atomic value only on success [H2]. Cosmo returns from `newGraphServer` on any build error and explicitly keeps the old server; on success it atomically swaps state [C2][C4]. Fusion's file provider hashes and parses changed archives, ignores unreadable updates, and its manager builds and warms a new executor before assignment [F3][F4].

The majority design never mutates the live generation in place. It creates a candidate off to the side and makes publication a minimal, non-failing operation. Readiness should be generation-aware: startup is unready until the first valid generation, while a failed later candidate leaves readiness and traffic on the last-known-good generation.

### 6. Retirement distinguishes finite and long-lived work

For finite requests, replacement blocks new selection of the old generation but preserves the state already selected by in-flight requests. Hive states this precisely: snapshots keep schema data alive, and retirement does not affect ordinary in-flight work [H3]. Cosmo swaps first and then waits for per-mux in-flight counters to reach zero before shutting down resources [C2][C3]. Apollo gracefully rolls the HTTP server/service factory and retains connection-stop signals [A2][A6]. Fusion uses a configured eviction delay before disposing the old executor [F4].

Streaming requires a separate policy because waiting for natural completion could retain a generation forever. Apollo sends a schema-reload termination error to every active subscription [A5]. Hive embeds a retirement token in the snapshot and selects between the next stream event and retirement, emitting `SUBSCRIPTION_SCHEMA_RELOAD` [H3][H5]. Cosmo excludes WebSocket/hijacked connections from ordinary request drain and cancels the graph-server context after drain, closing them [C2][C3]. Fusion's examined manager exposes only timeout-based executor disposal; this is less explicit than the other three and should not be copied as the semantic contract [F4][F6].

## Important divergences

### Artifact richness: no majority

Apollo and Hive prioritize a standard, inspectable supergraph SDL that carries JOIN metadata. This makes them broadly compatible with composition producers, but the runtime must parse the schema and construct planner indexes during every generation load [A1][H3]. Cosmo and Fusion shift more packaging responsibility to composition. Cosmo's artifact includes composed and client schemas, field configuration, subgraph data, a schema version id, and router compatibility version [C1]. Fusion archives declare their format version, supported gateway formats, included sources, execution schema, and JSON settings [F2].

The useful decision is not “text versus binary.” It is whether the artifact has:

- a canonical semantic version and minimum runtime compatibility;
- a content digest and graph/version identity;
- the public/API schema;
- normalized routing metadata independent of one protocol implementation;
- source descriptors and declared capabilities;
- enough provenance and diagnostics to explain composition;
- an upgrade story that permits old and new artifact readers to coexist.

Scala can support both camps with one compiler boundary: `Supergraph SDL -> Routing IR` for ecosystem interoperability and `Versioned execution package -> Routing IR` for fast, controlled production deployment. The runtime below that boundary should not care which envelope supplied the IR.

### Replacement precision

Hive has the clearest ownership model: a separate owner publishes a retirement token when dropped; request snapshots retain data without retaining ownership; generation caches disappear when active references finish [H2][H3]. Cosmo has the clearest explicit drain counter and atomic hot-path pointer, but it must manually coordinate reused muxes, shutdown flags, transports, and contexts [C2][C3]. Apollo replaces a large Tower service factory/server pipeline and uses `Arc` plus graceful server machinery [A2][A6]. Fusion relies on managed DI scopes and a fixed eviction timeout [F4][F6].

For ZIO, a direct translation should use a scoped immutable generation plus an explicit lease/drain protocol, not a fixed sleep. ZIO scopes and interruption can express candidate acquisition, atomic publication, per-request leases, retirement notification, bounded drain, and finalizers without copying any language's incidental ownership machinery.

### Multi-graph and feature variants

Hive can select a plugin-provided supergraph per request and caches up to ten associated runtimes [H2]. Cosmo can retain several feature-flag graph muxes and reuse unchanged muxes across a reload [C3]. Apollo's primary state machine is one active supergraph/pipeline, with progressive behavior represented inside planning/configuration. Fusion's manager is keyed by schema name but each request executor remains one internally consistent schema generation [A2][F4].

This capability is not required for the initial Caliban gateway. The relevant invariant to preserve is that any future graph/contract/feature selection returns a **complete generation handle**; individual schema, planner, or source-client components must never be selected independently.

## Language-contingent choices versus domain choices

### Domain choices to copy

- Offline/control-plane composition and a promoted runtime artifact.
- Explicit artifact compatibility/version identity.
- A complete immutable graph generation as the publication and request-consistency unit.
- Lazy runtime operation planning with bounded, miss-deduplicating, generation-owned caches.
- Candidate build and warmup before a single publication step.
- Last-known-good serving on candidate failure.
- Ordinary request drain plus explicit long-lived-stream retirement.
- Cache warmup from known/popular operation inputs; no blind cross-schema plan reuse.

### Implementation-language choices not to copy literally

- **Rust (Apollo/Hive):** `Arc`, `ArcSwap`, ownership-drop notifications, Tower service cloning, Tokio cancellation tokens, and recursive boxed futures are natural Rust mechanisms [A2][A4][H2][H4]. The invariants matter; the exact types do not.
- **Go (Cosmo):** `atomic.Pointer`, `context.CancelFunc`, manual atomic request counters, explicit transport closing, and a sentinel mux avoid Go interface/nil overhead and compensate for manual lifecycle management [C2][C3]. They are not GraphQL requirements.
- **.NET (Fusion):** schema-scoped dependency-injection containers, `IOptionsMonitor`, `FileSystemWatcher`, `IObservable`, GC-backed result lifetimes, and delayed async disposal fit ASP.NET and the CLR [F3][F4][F6]. A fixed eviction delay is especially language/framework-contingent and weaker than a lease count.

Scala 3/ZIO strengths map cleanly to the shared domain architecture:

- immutable enums and opaque identifiers for routing IR and generation identity;
- `ZIO.acquireRelease`/`Scope` for source clients and generation resources;
- `Ref.Synchronized` or a small atomic publication cell for last-known-good state;
- `Promise`/memoization for single-flight plan misses;
- fibers and structured interruption for parallel plan nodes, cancellation, and bounded retirement;
- `Chunk`, specialized arrays, and carefully chosen mutable builders inside otherwise immutable compiled artifacts for hot-path performance;
- a typed source capability algebra, with remote GraphQL and local Caliban as the first two interpreters, without abstracting ZIO itself.

## Recommended architectural decision for the map

Adopt the four-gateway majority lifecycle and record the artifact-format split explicitly:

1. `compose` is an offline library/CLI/control-plane capability. It produces a validated, versioned artifact; development may invoke it automatically, but the request router never composes subgraphs.
2. Define a protocol-neutral compiled `RoutingGraph` IR. Federation/JOIN input and the gateway's packaged artifact both compile into this same IR. Preserve the original composed SDL and composition provenance for interoperability and diagnostics.
3. Build an immutable, scoped `GraphGeneration` containing `RoutingGraph`, API schema, planner indexes, execution-source registry, transport/client resources, policy-derived metadata, and generation-owned caches.
4. Validate compatibility, construct all resources, and optionally prewarm known operations before atomic publication. Candidate failure must never disturb the current generation.
5. Acquire exactly one generation lease at request admission and carry it through parse/validate/normalize/plan/execute/merge. No later stage rereads “current schema.”
6. Keep parse caches global only where their values are genuinely schema-independent. Validation, normalization results that embed schema positions, query plans, coercion programs, and response projections are generation-owned unless their cache keys include the complete semantic generation identity.
7. On reload, stop new leases on the old generation, signal stream retirement, let finite leases drain to a configurable deadline, interrupt remaining work if required, then close source clients and drop caches. Track current, candidate, retiring generations, lease counts, and retirement duration in metrics.
8. Warm a new generation by replaying operation documents/hashes selected from the old generation or a persisted-operation manifest. Reuse a compiled plan only when a proof-bearing cache key shows identical artifact digest, planner version/options, source capabilities, and relevant policy inputs.

This leaves the future Composite Schemas and gRPC work at the correct seam: they add artifact front ends and execution-source capabilities; they do not require a new lifecycle, cache model, or publication mechanism.

## Primary-source references

All local paths below are relative to `gateway-plan/`. Each official link is pinned to the exact reviewed commit.

### Apollo Router — `ce52c982afedb6636e915a2affeb4a27cfbbd53a`

- **[A1] Composition boundary and supergraph loading:** `sources/apollo-router/docs/source/routing/migration/from-gateway.mdx:54-73`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/docs/source/routing/migration/from-gateway.mdx#L54-L73)
- **[A2] Candidate build, last-known-good failure, server rollover:** `sources/apollo-router/apollo-router/src/state_machine.rs:390-485,551-735`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/src/state_machine.rs#L390-L485)
- **[A3] Schema-bound deduplicating planner cache and prior-cache warmup:** `sources/apollo-router/apollo-router/src/query_planner/caching_query_planner.rs:85-208`; `sources/apollo-router/apollo-router/src/router_factory.rs:270-312`. [Official pinned cache source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/src/query_planner/caching_query_planner.rs#L85-L208)
- **[A4] Typed plan-tree execution:** `sources/apollo-router/apollo-router/src/query_planner/execution.rs:131-455`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/src/query_planner/execution.rs#L131-L455)
- **[A5] Subscription termination on schema/config update:** `sources/apollo-router/docs/source/routing/operations/subscriptions/configuration.mdx:442-460`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/docs/source/routing/operations/subscriptions/configuration.mdx#L442-L460)
- **[A6] Graceful HTTP-server restart and connection tracking:** `sources/apollo-router/apollo-router/src/http_server_factory.rs:45-167`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/src/http_server_factory.rs#L45-L167)

### Hive Router — `0299232a3e039e2b3cbe2cfb9dbc952f687ab79c`

- **[H1] Router consumes a configured supergraph file:** `sources/hive-router/README.md:1-45`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/README.md#L1-L45)
- **[H2] Generation contents, atomic publication, build-before-store, failure behavior, and polling:** `sources/hive-router/bin/router/src/schema_state.rs:61-169,191-319,328-519,545-595`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/bin/router/src/schema_state.rs#L61-L169)
- **[H3] Schema owner/snapshot/retirement model and planner construction:** `sources/hive-router/lib/executor/src/plugins/hooks/on_supergraph_load.rs:35-169`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/executor/src/plugins/hooks/on_supergraph_load.rs#L35-L169)
- **[H4] Cached request-time planning and typed plan execution:** `sources/hive-router/bin/router/src/pipeline/query_plan.rs:79-150`; `sources/hive-router/lib/executor/src/execution/plan.rs:821-862`. [Official pinned planning source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/bin/router/src/pipeline/query_plan.rs#L79-L150)
- **[H5] Subscription producer observes generation retirement:** `sources/hive-router/bin/router/src/pipeline/mod.rs:625-670`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/bin/router/src/pipeline/mod.rs#L625-L670)

### Cosmo Router — `5edbee289ba54cab1f2e3639b231f1747ead8aa6`

- **[C1] Composition emits router execution configuration with compatibility and schema identity:** `sources/cosmo/controlplane/src/core/composition/composeGraphs.worker.ts:191-245`. [Official pinned source](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/controlplane/src/core/composition/composeGraphs.worker.ts#L191-L245)
- **[C2] Atomic server generation swap and graceful old-server shutdown:** `sources/cosmo/router/core/http_server.go:20-50,101-148`. [Official pinned source](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/http_server.go#L20-L148)
- **[C3] Generation structure, compatibility check, generation-owned caches, and in-flight drain:** `sources/cosmo/router/core/graph_server.go:80-175,680-747,2155-2308`. [Official pinned source](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/graph_server.go#L80-L175)
- **[C4] Failed candidate keeps old graph server:** `sources/cosmo/router/core/router.go:639-652`. [Official pinned source](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/router.go#L639-L652)
- **[C5] Reload-persistent expensive-operation warmup inputs:** `sources/cosmo/router/core/reload_persistent_state.go:11-52`. [Official pinned source](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/reload_persistent_state.go#L11-L52)

### Hot Chocolate Fusion — `00c61af25908319ee277377652191a5aa8c2f60e`

- **[F1] Offline composition command and `.far` output:** `sources/hotchocolate-fusion/website/content/docs/fusion/cli.md:45-98`. [Official pinned source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/website/content/docs/fusion/cli.md#L45-L98)
- **[F2] Archive and gateway-format version metadata:** `sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Packaging/ArchiveMetadata.cs:5-29`; `sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Packaging/GatewayConfiguration.cs:5-40`. [Official pinned metadata source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Packaging/ArchiveMetadata.cs#L5-L29)
- **[F3] File/archive watcher, hashing, parsing, and invalid-update retention:** `sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Configuration/FileSystemFusionConfigurationProvider.cs:16-95,127-195`. [Official pinned source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Execution/Configuration/FileSystemFusionConfigurationProvider.cs#L16-L195)
- **[F4] Executor construction/warmup, replacement, and grace-period eviction:** `sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/FusionRequestExecutorManager.cs:120-220,598-684`. [Official pinned source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/FusionRequestExecutorManager.cs#L120-L220)
- **[F5] Schema-scoped operation planner/cache and single-flight cache misses:** `sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/FusionRequestExecutorManager.cs:443-470`; `sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/Pipeline/OperationPlanCacheMiddleware.cs:10-140`. [Official pinned middleware source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/Pipeline/OperationPlanCacheMiddleware.cs#L10-L140)
- **[F6] Request/stream result resource lifetime:** `sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/FusionRequestExecutor.cs:56-149`. [Official pinned source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/FusionRequestExecutor.cs#L56-L149)

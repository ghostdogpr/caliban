# Performance architecture of the Caliban graph router

## Question and answer

Which architecture choices dominate the hot path, and where can Caliban be reused without putting the agreed performance gate at risk?

The strongest cross-router conclusion is that a gateway should not be implemented as “run a normal GraphQL server once per fetch and merge generic Scala values.” The competitive implementations all make the same larger moves:

1. compile schema-derived state once per immutable schema generation;
2. cache the parsed/normalized/validated operation and especially the executable plan, with the schema generation and behavior-affecting options in its identity;
3. coalesce concurrent cache misses (single-flight) while keeping a hit free of the miss-coordination lock;
4. execute an explicit dependency graph, starting all ready fetches concurrently and merging deterministically;
5. use a response representation built for lookup, projection, raw JSON preservation, and in-place/arena-backed assembly rather than a general application ADT;
6. pool or reuse HTTP connections and tune HTTP/2 independently per subgraph;
7. measure allocations, cache contention, overload behavior, and correctness in addition to requests/second.

Caliban's parser, validation/coercion logic, schema model, jsoniter codecs, ZIO runtime, local interpreter, and federation subgraph support are credible reuse candidates. Caliban's current `ResponseValue` representation and ordinary interpreter pipeline are not yet justified as the remote-routing hot path: they use immutable `List`/`Map`, linear object lookup until a lazy map is built, deep merges that allocate maps and collections, and full object decoding. The design should therefore put adapters around reusable Caliban semantics and reserve a specialized gateway operation/plan/response representation. The parser and validator should be reused first, but kept behind a measurable seam; the specialized response store should be assumed necessary unless benchmarks disprove it.

This is a source audit, not a benchmark result. It identifies hypotheses and gates that must be measured on the JVM.

## Source basis

All comparative claims below come from the locally pinned primary-source corpus.

| System | Pinned commit | Upstream |
|---|---|---|
| Apollo Router | `ce52c982afedb6636e915a2affeb4a27cfbbd53a` | [apollographql/router](https://github.com/apollographql/router/tree/ce52c982afedb6636e915a2affeb4a27cfbbd53a) |
| Hive Router | `0299232a3e039e2b3cbe2cfb9dbc952f687ab79c` | [graphql-hive/router](https://github.com/graphql-hive/router/tree/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c) |
| Cosmo Router | `5edbee289ba54cab1f2e3639b231f1747ead8aa6` | [wundergraph/cosmo](https://github.com/wundergraph/cosmo/tree/5edbee289ba54cab1f2e3639b231f1747ead8aa6) |
| Hot Chocolate Fusion | `00c61af25908319ee277377652191a5aa8c2f60e` | [ChilliCream/graphql-platform](https://github.com/ChilliCream/graphql-platform/tree/00c61af25908319ee277377652191a5aa8c2f60e) |
| Caliban | `27516febcd8200c03ec82a0d24188d451922f916` | [ghostdogpr/caliban](https://github.com/ghostdogpr/caliban/tree/27516febcd8200c03ec82a0d24188d451922f916) |

Local paths in citations are relative to the repository root. Comparative paths start at `gateway-plan/sources/` and are exact for the commits above.

For readability, later prose sometimes contracts a repeated prefix to `...`. The authoritative exact local evidence paths are:

- Apollo: `gateway-plan/sources/apollo-router/apollo-router/src/cache/mod.rs`, `gateway-plan/sources/apollo-router/apollo-router/src/query_planner/caching_query_planner.rs`, `gateway-plan/sources/apollo-router/apollo-router/src/query_planner/execution.rs`, `gateway-plan/sources/apollo-router/apollo-router/src/router_factory.rs`, `gateway-plan/sources/apollo-router/apollo-router/src/graphql/response.rs`, `gateway-plan/sources/apollo-router/apollo-router/src/json_ext.rs`, `gateway-plan/sources/apollo-router/apollo-router/src/allocator.rs`, `gateway-plan/sources/apollo-router/apollo-router/Cargo.toml`, `gateway-plan/sources/apollo-router/apollo-router-benchmarks/benches/query_plan_cache_concurrency.rs`, `gateway-plan/sources/apollo-router/apollo-router-benchmarks/benches/memory_use.rs`, `gateway-plan/sources/apollo-router/apollo-router/benches/huge_requests.rs`, and `gateway-plan/sources/apollo-router/apollo-router/benches/deeply_nested.rs`.
- Hive: `gateway-plan/sources/hive-router/bin/router/src/shared_state.rs`, `gateway-plan/sources/hive-router/bin/router/src/pipeline/parser.rs`, `gateway-plan/sources/hive-router/bin/router/src/schema_state.rs`, `gateway-plan/sources/hive-router/bin/router/src/pipeline/query_plan.rs`, `gateway-plan/sources/hive-router/lib/query-planner/benches/qp_benches.rs`, `gateway-plan/sources/hive-router/lib/executor/src/response/value.rs`, `gateway-plan/sources/hive-router/lib/executor/src/execution/plan.rs`, `gateway-plan/sources/hive-router/lib/executor/src/projection/plan.rs`, `gateway-plan/sources/hive-router/lib/executor/benches/executor_benches.rs`, `gateway-plan/sources/hive-router/Cargo.toml`, `gateway-plan/sources/hive-router/lib/executor/Cargo.toml`, `gateway-plan/sources/hive-router/bin/router/Cargo.toml`, `gateway-plan/sources/hive-router/bench/run-benchmark.sh`, `gateway-plan/sources/hive-router/bench/wrk.lua`, and `gateway-plan/sources/hive-router/bench/ci-detect-regression.sh`.
- Cosmo: `gateway-plan/sources/cosmo/router/core/graph_server.go`, `gateway-plan/sources/cosmo/router/core/operation_planner.go`, `gateway-plan/sources/cosmo/router/internal/planningbenchmark/benchmark_test.go`, `gateway-plan/sources/cosmo/router/core/operation_processor.go`, `gateway-plan/sources/cosmo/router/core/context.go`, `gateway-plan/sources/cosmo/router/core/defer_response_writer.go`, `gateway-plan/sources/cosmo/router/core/router.go`, `gateway-plan/sources/cosmo/router/internal/exporter/exporter.go`, and `gateway-plan/sources/cosmo/router/bench/bench.js`.
- Fusion: `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/Pipeline/OperationPlanCacheMiddleware.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/FusionDocumentCache.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/FusionRequestExecutorManager.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Text/Json/SourceResultDocument.Parse.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Text/Json/SourceResultDocument.Text.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Text/Json/SourceResultDocument.TryGetProperty.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/ExecutionState.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/OperationPlanContextPool.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Text/Json/PathSegmentPool.cs`, `gateway-plan/sources/hotchocolate-fusion/website/content/docs/fusion/performance-tuning.md`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/benchmarks/k6/eShop.Gateway/Program.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/benchmarks/k6/eShop.Gateway/MemoryArenaEventListener.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/benchmarks/k6/eShop.Gateway/BufferPoolDiagnostics.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/benchmarks/k6/eShop.Gateway/PathSegmentPoolDiagnostics.cs`, `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/benchmarks/k6/run-single-benchmark.sh`, and `gateway-plan/sources/hotchocolate-fusion/src/HotChocolate/Fusion/benchmarks/k6/compare-gateway-performance.sh`.

## Comparative findings by stage

### Parse, normalize, validate, and plan

The majority architecture is a staged compiler whose expensive products are immutable and schema-scoped.

- **Apollo** wraps the planner in a schema-aware LRU cache. Its key includes query/planning inputs such as a hash of planner configuration; its cache can also have Redis behind the in-memory layer. The cache implementation explicitly has a lock-free-with-respect-to-single-flight hit path: it checks in-memory storage before acquiring the `wait_map` mutex, while misses use a broadcast-based deduplication protocol. This change has a dedicated concurrency sweep benchmark from 1 to 64 tasks because the old design acquired a mutex, created a broadcast sender, and spawned cleanup work even on hits. See local `.../apollo-router/apollo-router/src/cache/mod.rs`, `.../query_planner/caching_query_planner.rs`, and `.../apollo-router-benchmarks/benches/query_plan_cache_concurrency.rs`; upstream [cache implementation](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/src/cache/mod.rs), [caching planner](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/src/query_planner/caching_query_planner.rs), and [cache-concurrency benchmark](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router-benchmarks/benches/query_plan_cache_concurrency.rs).
- **Hive** separates parse, validate, normalize, and plan caches. A router-wide parse cache hashes query text with XXH3, while validate/normalize/plan caches live on `RouterSupergraphRuntime`, so replacing the selected schema generation naturally drops incompatible entries once users release it. The plan key combines a normalized operation hash with stable override context. Moka's asynchronous `entry(...).or_try_insert_with(...)` API coalesces the cache computation. See local `.../hive-router/bin/router/src/shared_state.rs`, `.../pipeline/parser.rs`, `.../schema_state.rs`, and `.../pipeline/query_plan.rs`; upstream [schema-scoped caches](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/bin/router/src/schema_state.rs) and [query-plan cache path](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/bin/router/src/pipeline/query_plan.rs).
- **Cosmo** has distinct Ristretto caches for persisted operations, normalization, variable normalization, variable remapping, validation, complexity, operation hashes, and prepared plans. A plan cache belongs to the engine/planner configuration; planning uses Go `singleflight`, and an optional slow-plan fallback retains plans selected by planning cost when the main admission cache evicts them. See local `.../cosmo/router/core/graph_server.go` and `.../core/operation_planner.go`; upstream [cache construction](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/graph_server.go) and [plan lookup/single-flight](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/operation_planner.go).
- **Fusion** installs both a parsed-document cache and an operation-plan cache. `OperationPlanCacheMiddleware` uses a `ConcurrentDictionary` of lazy task-completion sources so exactly one request plans an uncached operation and followers await it; the plan is inserted before the in-flight marker is removed, avoiding a gap. Planner instances and their caches are schema services, so they are generation-scoped. See local `.../hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/Pipeline/OperationPlanCacheMiddleware.cs`, `.../Execution/FusionDocumentCache.cs`, and `.../Execution/FusionRequestExecutorManager.cs`; upstream [single-flight plan middleware](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/Pipeline/OperationPlanCacheMiddleware.cs).

The planning algorithms differ, but all emit an explicit execution structure rather than interpreting ownership rules during each response. Hive's planner benchmark exposes its stages: walk the normalized operation over a graph, find the best path combination, construct a fetch graph, then lower it to a query plan. It also includes a “many plans” fixture, recognizing combinatorial planning as a special risk. Cosmo's planning benchmark times a prepared operation, reports allocations, and deliberately excludes parsing/preparation from the measured plan phase. See local `.../hive-router/lib/query-planner/benches/qp_benches.rs` and `.../cosmo/router/internal/planningbenchmark/benchmark_test.go`; upstream [Hive planner benchmark](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/query-planner/benches/qp_benches.rs) and [Cosmo planner benchmark](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/internal/planningbenchmark/benchmark_test.go).

**Decision for the Scala gateway:** use one immutable `SchemaGeneration` containing the compiled supergraph topology, source contracts, validator indices, planner state, plan cache, and source clients. A reload publishes a new generation atomically. Cache identity is `(generation identity, canonical operation identity, operation name, planning-affecting options such as override labels and authorization filtering)`. Parsing may be shared across generations only if the cached object contains no schema-derived references. Cache misses must use keyed single-flight; cache hits must not enter the miss lock or allocate coordination objects. Cache admission must be bounded by estimated weight, not merely entry count, because operation and plan sizes vary substantially.

### Operation and schema representations

All four competitors invest in representations tailored to repeated reads.

- Apollo keeps validated compiler schemas and subgraph schemas in shared `Arc`s and plans in shared immutable objects. Its request/response JSON uses `serde_json_bytes`: object keys are byte strings rather than eagerly allocated UTF-8 `String`s. Local `.../apollo-router/apollo-router/src/router_factory.rs` and `.../graphql/response.rs`; upstream [response model](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/src/graphql/response.rs).
- Hive's executor has a custom `Value<'a>`: strings and raw JSON are `Cow<'a, str>`, arrays are vectors, and objects are sorted vectors of borrowed keys. Field access can then use binary search. Subgraph JSON is parsed with `sonic-rs`; unescaped strings remain borrowed. A request-local bump arena holds rewritten entity representations, and XXH3/AHash are used in entity deduplication and hot maps. Local `.../hive-router/lib/executor/src/response/value.rs` and `.../execution/plan.rs`; upstream [custom value](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/executor/src/response/value.rs) and [entity preparation](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/executor/src/execution/plan.rs).
- Cosmo retains request variables/extensions as `json.RawMessage` until needed and uses `astjson`/`jsonparser` for mutation and lookup rather than decoding all JSON into `map[string]any`. Its execution engine is built around prepared plans from graphql-go-tools. Local `.../cosmo/router/core/operation_processor.go`; upstream [operation processor](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/operation_processor.go).
- Fusion has the most specialized response store in this corpus. `SourceResultDocument` adopts a single received byte buffer without copying when it fits, otherwise ingests into geometric arena chunks. It parses with `Utf8JsonReader` into a compact metadata database whose locations point back into raw UTF-8 storage. Property APIs operate on `ReadOnlySpan<byte>`, small temporary arrays use stack allocation, larger ones rent from `ArrayPool`, and raw values can be returned without materializing strings. Local `.../hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Text/Json/SourceResultDocument.Parse.cs`, `.Text.cs`, and `.TryGetProperty.cs`; upstream [source result parser](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Execution/Text/Json/SourceResultDocument.Parse.cs).

The relevant majority is not one exact data structure; it is **a distinct wire/result representation that minimizes decoding and preserves raw bytes, plus compact schema/plan IDs instead of repeated names**.

**Decision for the Scala gateway:** distinguish at least three representations:

1. `CalibanDocument`, used at the compatibility boundary for GraphQL syntax and validation;
2. a compiled gateway operation/plan containing integer IDs, compact arrays, precompiled response paths, variable projections, input/output rewrite programs, and immutable source-operation byte templates;
3. a request-owned response store backed by pooled byte chunks and compact nodes, capable of borrowed/raw custom scalar values and deterministic projection/serialization.

Do not expose the response store publicly. Translate to/from Caliban `InputValue`/`ResponseValue` only at local-Caliban and public extension seams. This preserves Scala ergonomics without forcing the hot path through allocation-heavy general values.

### Execution and concurrency

The common execution model is dependency-driven concurrency with sequential merging at safe points.

- Apollo's plan has `Sequence`, `Parallel`, `Flatten`, `Fetch`, `Condition`, subscription, and defer nodes. Parallel children are put in `FuturesUnordered`; results are merged as they complete. Client cancellation is checked before a fetch, so abandoned requests avoid downstream work. Local `.../apollo-router/apollo-router/src/query_planner/execution.rs`; upstream [plan execution](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/src/query_planner/execution.rs).
- Hive also models sequence/parallel/condition/fetch/flatten/batch-fetch. It prepares each parallel job synchronously while it can borrow current response data, then executes the returned futures concurrently. Entity representations are hashed and deduplicated, and empty representation sets skip the network call. Local `.../hive-router/lib/executor/src/execution/plan.rs`; upstream [executor](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/executor/src/execution/plan.rs).
- Cosmo lowers to prepared resolve/fetch trees and relies on the graphql-go-tools engine. The router keeps operation normalization, plan, variables, and transport state distinct; the plan analysis code recognizes sequence and parallel fetch-tree nodes. Local `.../cosmo/router/core/context.go` and `.../core/operation_planner.go`; upstream [query-plan state](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/context.go).
- Fusion builds a dependency DAG. `ExecutionState` stores remaining dependency counts and node state in reusable primitive arrays, queues completed results and pending merges, starts ready nodes, and serializes merge application. It uses a bitset for failed/skipped nodes and returns arrays to `ArrayPool`. Local `.../hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/ExecutionState.cs`; upstream [execution scheduler](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/ExecutionState.cs).

**Decision for the Scala gateway:** compile plans to a DAG of primitive-indexed nodes and dependencies. At runtime keep ready/dependency state in request-owned arrays; launch all ready remote/local fetches using ZIO structured concurrency, but have one deterministic response-store owner apply completed merges. Cancellation and deadline should be inherited through the ZIO scope; no detached fibers. Queries allow parallel ready nodes, mutations preserve root-field seriality, and source-level concurrency limits provide backpressure. Do not fork a fiber merely to perform a small pure rewrite or merge; use direct loops for local CPU work and fibers for actual asynchronous boundaries. ZIO's scoped resources, interruption, `foreachPar`, semaphores, and fiber dumps are strengths, but the number of fibers per operation and scheduling overhead must be benchmarked against a callback/queue scheduler.

### JSON, projection, and response assembly

Response assembly is likely the dominant gateway-owned cost once network time is subtracted, especially for entity joins and large responses.

- Apollo parses incoming bytes into a byte-string-keyed value tree, then performs schema-aware deep merges and applies rewrites/selection logic. This is robust but still materializes a tree. Local `.../apollo-router/apollo-router/src/graphql/response.rs`, `.../query_planner/execution.rs`, and `.../json_ext.rs`.
- Hive uses a borrowed custom tree, sorted object vectors, raw JSON nodes for custom scalars, precomputed field-projection plans, direct buffered construction of `_entities` variables, and a bump arena for temporary rewrites. Its executor benchmark isolates projection of a pre-parsed response. Local `.../hive-router/lib/executor/src/projection/plan.rs`, `.../response/value.rs`, `.../execution/plan.rs`, and `.../benches/executor_benches.rs`; upstream [projection benchmark](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/executor/benches/executor_benches.rs).
- Cosmo's `astjson` use and raw messages avoid turning every scalar and extension into Go interface values. Its response writers include paths explicitly intended to write large JSON payloads without another copy. Local `.../cosmo/router/core/operation_processor.go` and `.../core/defer_response_writer.go`; upstream [defer response writer](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/defer_response_writer.go).
- Fusion couples raw UTF-8 storage to a compact metadata index and an arena lifecycle, allowing projection and serialization to copy raw scalar slices while only structurally rebuilding the selected response. Its benchmark gateway includes event listeners specifically for memory arenas, buffer pools, and path-segment pools. Local `.../Fusion.Execution/Text/Json/`, `.../benchmarks/k6/eShop.Gateway/MemoryArenaEventListener.cs`, `BufferPoolDiagnostics.cs`, and `PathSegmentPoolDiagnostics.cs`.

**Decision for the Scala gateway:** the response-store prototype is a release-blocking investigation, not an optional optimization. It should compare:

- current Caliban `ResponseValue` via jsoniter;
- a mutable array/object node arena with integer key IDs and pooled byte chunks;
- a raw-byte-plus-token-index design inspired by Fusion;
- selective materialization, preserving untouched custom scalars and leaf subtrees as raw slices.

The store must support fast path lookup by precompiled key/path ID, stable GraphQL field order at final projection, null propagation, error-path rewriting, list fan-out, entity representation extraction/deduplication, incremental ownership later, and safe lifetime management. A raw slice may never outlive its owning response buffer. Pooled buffers must be size-capped and cleared so large or reference-retaining buffers are not accidentally kept forever.

### HTTP and network stacks

Every competitor uses long-lived pooled transports; HTTP client construction is control-plane work, not per request.

- Apollo is Axum/Hyper/Tower on Tokio and uses Hyper/Reqwest with Rustls for clients. Local `.../apollo-router/apollo-router/Cargo.toml`.
- Hive uses ntex for the inbound server and Hyper/Hyper-util/Rustls for outbound calls, including HTTP/2 features. Local `.../hive-router/Cargo.toml`, `.../lib/executor/Cargo.toml`, and `.../bin/router/Cargo.toml`.
- Cosmo uses Go `net/http` and builds reusable transports with configurable per-host totals, idle pools, dial/header/TLS timeouts, keepalive, and `ForceAttemptHTTP2`; defaults include high `MaxConnsPerHost` and bounded idle pools. Local `.../cosmo/router/core/router.go`; upstream [transport construction](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/router.go).
- Fusion uses `IHttpClientFactory`/`SocketsHttpHandler`; its own performance fixture sets `MaxConnectionsPerServer = 256` and enables multiple HTTP/2 connections. Official tuning guidance calls out TLS ALPN, explicit h2c, and the effect of HTTP/2 stream limits. Local `.../hotchocolate-fusion/website/content/docs/fusion/performance-tuning.md` and `.../benchmarks/k6/eShop.Gateway/Program.cs`; upstream [performance tuning](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/website/content/docs/fusion/performance-tuning.md).

**Decision for the Scala gateway:** define a small `ExecutionSource` interface in the engine, but make the reference server and remote GraphQL source use a single chosen native asynchronous stack with shared per-origin pools. Do not route the core hot path through Tapir/STTP abstractions until measured. The first benchmark should compare ZIO HTTP's Netty server/client, a tuned Netty client used directly behind ZIO, and—only if competitive—STTP backends already present in Caliban. Required knobs are HTTP/1.1 and HTTP/2/h2c policy, max connections and streams, idle connection policy, connect/TLS/header/body/deadline timeouts, decompression, request-body size, and per-source concurrency. Client response bodies should enter the response store from byte chunks with the fewest possible copies.

### Allocation and memory controls

The competitors repeatedly choose explicit allocation control rather than relying on the general allocator alone.

- Apollo shares immutable data with `Arc`, uses byte strings, tracks memory through the allocator, has a benchmark that records physical/virtual memory as distinct queries fill caches, and uses `heaptrack` for huge-request peak RSS. Local `.../apollo-router/src/allocator.rs`, `.../apollo-router-benchmarks/benches/memory_use.rs`, and `.../apollo-router/benches/huge_requests.rs`.
- Hive uses borrowed `Cow` values, vectors with known capacity, raw JSON, a bump arena, hash-based entity deduplication, and Arc-held schema/plan data.
- Cosmo uses multiple bounded admission caches and `sync.Pool` in high-volume exporters; its pool code explicitly clears reference slots and refuses to retain oversized backing arrays, illustrating the GC hazard of careless pooling. Local `.../cosmo/router/internal/exporter/exporter.go`; upstream [pool discipline](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/internal/exporter/exporter.go).
- Fusion pools whole `OperationPlanContext` objects, primitive scheduler arrays, arena segments, builders, and path segments. The pool grows through configured levels and trims when demand falls. Local `.../Fusion.Execution/Execution/OperationPlanContextPool.cs`, `OperationPlanContext.Pooling.cs`, and `Text/Json/PathSegmentPool.cs`; upstream [operation context pool](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/OperationPlanContextPool.cs).

**Decision for the Scala gateway:** start with immutable generation state and request-confined mutable state. Prefer primitive arrays, `ArraySeq`/arrays, byte chunks, and integer IDs in the execution core. Pool only after allocation profiles identify sustained churn, and cap retained capacity. Never pool arbitrary `ResponseValue` graphs. Record allocated bytes/op, allocation count/op where JFR permits, live-set after full GC, cache weight, direct-buffer memory, and peak RSS. Evaluate G1 and ZGC on the supported JDK rather than baking in a collector assumption.

## Caliban reuse audit

The audit is based on Caliban commit `27516febcd8200c03ec82a0d24188d451922f916`.

| Component | Evidence | Initial posture | Measurement / limitation |
|---|---|---|---|
| GraphQL parser | `core/src/main/scala/caliban/parsing/Parser.scala` uses FastParse and returns Caliban's `Document`; JMH exists at `benchmarks/src/main/scala/caliban/ParserBenchmark.scala`. [Parser](https://github.com/ghostdogpr/caliban/blob/27516febcd8200c03ec82a0d24188d451922f916/core/src/main/scala/caliban/parsing/Parser.scala) | Reuse behind `OperationFrontend`; do not fork grammar/diagnostics prematurely. | Measure parse time and bytes allocated for small, persisted, introspection, deeply nested, and adversarial documents. It starts from `String` and builds general case-class/list ASTs; a UTF-8 parser or compact AST may win materially. |
| Validation and variable coercion | `core/src/main/scala/caliban/validation/Validator.scala` and `core/src/main/scala/caliban/parsing/VariablesCoercer.scala`; JMH separates validation, coercion, and `prepare`. [Validator](https://github.com/ghostdogpr/caliban/blob/27516febcd8200c03ec82a0d24188d451922f916/core/src/main/scala/caliban/validation/Validator.scala) | Reuse semantics and tests; adapt composed-schema metadata into a Caliban-compatible validation view. | Cache successful prepared operations. Measure whether repeated list/map traversal and construction dominate misses. Gateway-specific Federation/JOIN planning validation remains separate. |
| Schema and introspection model | `core/src/main/scala/caliban/schema/`, `introspection/adt/`, and `Introspector.scala`. | Reuse for the public client-schema boundary and local Caliban integration. | Planner topology should be a compiled gateway index with integer IDs, not repeated traversal of introspection objects. Measure conversion/build time only on reload. |
| JSON codecs | `core/src/main/scala/caliban/interop/jsoniter/jsoniter.scala` has hand-written loops, specialized number handling, low-case-count dispatch, and existing JMH at `benchmarks/src/main/scala/caliban/json/JsonEncodingBenchmark.scala`. [jsoniter codec](https://github.com/ghostdogpr/caliban/blob/27516febcd8200c03ec82a0d24188d451922f916/core/src/main/scala/caliban/interop/jsoniter/jsoniter.scala) | Reuse for API/local-source boundaries and as the baseline encoder. | Full decoding materializes strings/lists/maps; compare against the response-store parser and a direct streaming final writer. TreeMap input-object decoding is correctness/security motivated but could be expensive for large variables. |
| `InputValue` / `ResponseValue` | `core/src/main/scala/caliban/Value.scala`; objects hold `List[(String, ResponseValue)]`, `get` builds a lazy map, `at` searches fields linearly, and `deepMerge` builds maps/sets/new collections. [Value model](https://github.com/ghostdogpr/caliban/blob/27516febcd8200c03ec82a0d24188d451922f916/core/src/main/scala/caliban/Value.scala) | Keep as an interop value, not the assumed routing store. | Benchmark parse → representations → entity extraction → N-way merge → projection → encode. Reuse only if within the gate on allocations and tail latency. |
| Local execution | `core/src/main/scala/caliban/GraphQL.scala`, `GraphQLInterpreter.scala`, and `execution/Executor.scala`. | Treat a local Caliban graph as a first-class `ExecutionSource`; invoke an already-built interpreter/compiled local operation. | The normal interpreter parses, coerces, validates, and prepares every call unless wrappers intercept it. The gateway must avoid serializing an internal request to HTTP/JSON and should avoid duplicating front-end work. A dedicated prepared-operation API may be required. |
| ZIO / ZQuery execution | `core/src/main/scala/caliban/execution/QueryExecution.scala` supports sequential, parallel, batched, and mixed strategies; executor uses scoped ZIO/ZQuery and streams. [QueryExecution](https://github.com/ghostdogpr/caliban/blob/27516febcd8200c03ec82a0d24188d451922f916/core/src/main/scala/caliban/execution/QueryExecution.scala) | Reuse ZIO as the gateway runtime and ZQuery inside local graphs. | Do not represent every remote plan node as an ordinary Caliban resolver or ZQuery request. Measure a fiber-per-fetch DAG against a compact completion-queue scheduler; cap parallelism. |
| Federation subgraph support | `federation/src/main/scala/caliban/federation/` implements subgraph-facing Federation helpers/directives/entity resolvers. | Reuse for local Caliban subgraph contracts and compliance fixtures. | It is not supergraph composition or router planning. The router needs separate JOIN ingestion, topology, planner, and merge machinery. |
| HTTP adapters and client libraries | `interop/tapir/`, `adapters/http4s/`, `adapters/akka-http/`, `adapters/pekko-http/`, plus STTP dependencies. | Preserve optional integration at host boundaries. | A gateway makes more outbound calls and moves more bytes than a normal server. Select the reference stack only after full-duplex pooled transport benchmarks; abstraction layers are not presumed free. |
| Existing benchmarks | `benchmarks/` provides JMH for parsing, validation/coercion, field preparation, JSON encoding, and local execution. | Extend/reuse fixtures and JMH setup. | Existing suites benchmark a GraphQL server, not routing. Add gateway micro, pipeline, and end-to-end suites without comparing raw numbers from different harnesses. |

The current `gateway` module is intentionally not treated as a compatibility or architecture baseline. This note makes no claim that it should be retained.

## Benchmark methodology required to make the decisions

No published number from these repositories is directly comparable: fixtures, revisions, hardware, runtime, telemetry, connection settings, and load models differ. Their methodology nevertheless supplies useful practices.

- Apollo uses Criterion microbenchmarks, an explicit concurrency sweep for cache-hit contention, a process memory-growth fixture, `heaptrack` peak RSS for very large requests, and correctness assertions in its end-to-end stress fixtures. Local `.../apollo-router-benchmarks/benches/` and `.../apollo-router/benches/`.
- Hive uses Criterion for parser/planner/executor stages and `wrk` for a release-mode full stack. The runner selects threads/connections/duration, checks response status, GraphQL errors and response shape, writes a normalized summary, and its CI script fails beyond a 5% throughput regression. Local `.../hive-router/lib/*/benches/`, `.../bench/run-benchmark.sh`, `.../bench/wrk.lua`, and `.../bench/ci-detect-regression.sh`; upstream [benchmark runner](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/bench/run-benchmark.sh).
- Cosmo's Go planner benchmark reports allocations and isolates prepared planning; its k6 scenarios cover a complex many-subgraph operation and assert success/no GraphQL errors. Local `.../cosmo/router/internal/planningbenchmark/` and `.../router/bench/`.
- Fusion's k6 harness separates constant 50-VU and ramping 0–500–0 modes, pins load-generator/gateway/source CPUs, runs each case three times and selects a median, and reports throughput, error rate, and latency percentiles. It includes simple, deep-recursion, and variable-batching scenarios plus arena/pool diagnostics. Local `.../hotchocolate-fusion/src/HotChocolate/Fusion/benchmarks/k6/run-single-benchmark.sh` and `compare-gateway-performance.sh`; upstream [single-benchmark runner](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/benchmarks/k6/run-single-benchmark.sh).

The Scala gateway should have three layers of measurement:

### 1. JMH component benchmarks

Use forked JVMs, fixed JDK/vendor/version and CPU governor, sufficient warmup to observe compiled steady state, profilers (`gc`, JFR/async-profiler), and Blackholes. Record throughput/latency and allocation rate.

- operation parse by byte size/shape;
- validation and variable coercion, separately;
- normalization and plan generation, including a combinatorial “many choices” graph;
- cache hit at concurrency 1/2/4/8/16/32/64 and a same-key miss storm;
- query key hashing/canonicalization;
- source-request construction and representation deduplication;
- JSON parse, path lookup, merge, projection, and final encoding for 1 KiB, 32 KiB, 1 MiB, deep, wide, escaped-string, and custom-scalar-heavy responses;
- scheduler overhead by plan node count and ready width;
- local-Caliban prepared execution versus ordinary interpreter invocation.

### 2. In-process pipeline benchmarks

Run a built engine against deterministic in-memory `ExecutionSource`s. This removes socket variance but includes parse/validate/cache/plan/scheduling/merge/encode. Test cold and warm operation paths separately, with correctness checked byte-semantically against expected GraphQL responses. Workloads: passthrough standalone graph, single federated fetch, parallel independent fetches, two- and four-hop entity joins, duplicate entities, null/error propagation, large response, and error-heavy response.

### 3. Isolated end-to-end competition

Build one shared supergraph, operation corpus, source implementation, container limits, connection/TLS mode, telemetry policy, and load driver for all five routers. Run release artifacts on dedicated CPU sets; place the load generator and sources on disjoint CPUs as Fusion does. Warm the JVM and all operation/plan caches before steady-state samples, but also report cold-start and post-schema-reload recovery separately. Run at fixed offered loads and a saturation ramp; use at least five independent process runs, report median plus spread, and randomize router order.

For each of passthrough, single-fetch, parallel-fetch, entity-join, and error-heavy workloads report:

- achieved requests/s and useful responses/s;
- p50/p90/p95/p99/p99.9 end-to-end latency and router-owned overhead (total minus downstream wait when traceable);
- error/timeout/cancellation counts and response correctness;
- CPU-seconds/request, peak RSS, post-GC live set, allocation bytes/request, GC pause/time percentage, direct-buffer memory;
- downstream requests/client request, bytes in/out, connections created/reused, and pool wait;
- cache hit/miss/admission/eviction, plan latency, and same-key miss coalescing;
- queue time and active executions at overload.

The agreed acceptance gate—within roughly 15% of the leading compared router's throughput and tail latency after JVM warmup—must apply per representative workload, not to an average that hides a catastrophic join or large-response case. Correctness failures invalidate a sample. The project should additionally adopt a tighter self-regression gate (for example 5%, following Hive) once noise on its CI hardware is characterized. Allocation and memory ceilings should be set from the first competitive baseline rather than guessed now.

**Public benchmark qualification.** The pinned GraphQL Gateways Benchmark can seed only the heavy entity-join/large-response workload above. Its checked-in comparison repeats one identical query, omits Hot Chocolate, uses older and differently configured gateway builds, includes measured-process `setup()` traffic in k6 HTTP aggregates, incompletely isolates source/monitor processes, and can report zero failed HTTP requests while its GraphQL correctness checks fail. Reuse its deterministic Rust subgraphs, operation, raw metrics, and affinity/lifecycle ideas in a fork; do not use its leaderboard for the 15% gate until the five-workload, aligned-version, repeated-run, correctness-weighted, JVM-aware controls above are present. See [Cross-gateway audit and benchmark as acceptance evidence](17-cross-gateway-audit-and-benchmark.md).

## Decisions that remain measurement-dependent

These are the seams a later prototype/performance ticket must settle; this research does not select a winner without data.

1. **Parser/operation AST:** Caliban FastParse and existing AST versus a UTF-8/compact gateway parser. Default to Caliban unless parse+allocation measurements exceed the gate on cache misses or persisted-operation ingestion.
2. **Validation representation:** Caliban `RootType` validation directly versus generated integer-index validation tables while retaining Caliban-compatible diagnostics.
3. **Response store:** Caliban `ResponseValue`, mutable indexed arena, or raw-byte/token-index store. This is the highest-priority prototype.
4. **Final writer:** convert the store to `ResponseValue` then jsoniter, teach jsoniter a gateway-store codec, or stream directly to pooled byte chunks.
5. **Execution scheduler:** one ZIO fiber per asynchronous fetch wave versus a compact DAG scheduler with a completion queue and fewer fibers.
6. **Remote transport:** ZIO HTTP/Netty, direct Netty under ZIO, or an existing STTP backend; benchmark HTTP/1.1 and HTTP/2/h2c, large bodies, cancellation, and pool saturation.
7. **Cache implementation:** Caffeine (or equivalent weighted JVM cache) with keyed promises versus a purpose-built striped cache. Required properties are bounded weight, hit-path scalability, schema ownership, single-flight, metrics, and no blocking on compute work.
8. **Pooling:** which request state, arrays, and byte chunks are worth pooling under G1/ZGC after escape-analysis/JFR evidence; pooling is rejected when it increases retained memory or contention.
9. **Local Caliban fast path:** whether Caliban needs a new public prepared-operation execution API so a local source avoids duplicate parsing/validation and JSON round trips.
10. **Ahead-of-time operation preparation:** persisted/known operation manifests and schema reload prewarming, including how much of the prior generation's hot set to replan before publication.

## Architecture consequence

Performance concerns should shape module seams now, without prematurely implementing specialized machinery:

```text
GraphQL HTTP bytes
  -> OperationFrontend (Caliban first; replaceable by measurement)
  -> PreparedOperation / PlanCache (schema-generation scoped, weighted, single-flight)
  -> Planner (immutable compiled topology -> primitive-index DAG)
  -> Executor (ZIO scope, bounded ready-node concurrency)
       -> RemoteGraphQLSource (pooled byte-oriented HTTP)
       -> LocalCalibanSource (prepared in-process execution)
  -> ResponseStore (request-owned, indexed, raw-value capable)
  -> ProjectionWriter (GraphQL order/errors/nullability -> byte chunks)
```

The public engine API should expose semantic requests, sources, plans/diagnostics, and streams—not the internal arena, cache implementation, or HTTP library. That keeps Composite Schemas and future gRPC sources feasible while allowing the Federation/GraphQL v1 hot path to specialize aggressively. Scala's strengths are useful here: opaque IDs prevent cross-generation/type confusion at zero runtime cost; enums and exhaustive matches define plan nodes; immutable generation values make reload publication safe; ZIO provides structured cancellation, scoped lifetimes, backpressure, telemetry, and local-source composition. Performance-critical loops should still use arrays, while loops, specialized primitives, and request-confined mutation where profiles justify them.

The release posture is therefore: **reuse Caliban for semantics and local execution; do not couple gateway planning and response assembly to Caliban's general interpreter/value representation; promote specialized replacements only through the shared benchmark gate.**

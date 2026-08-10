# Production operational baseline and subsystem placement

Research date: 2026-08-07

## Scope and method

This note compares the pinned Apollo Router, Hive Router, Cosmo Router, and Hot Chocolate Fusion snapshots recorded in `SOURCES.md`. It uses only first-party repository source, documentation, tests, configuration schemas, and deployment artifacts.

The question is deliberately broader than “which features exist?” It asks which capabilities are common enough to be a production baseline, which are product-specific, and where each capability belongs in a dual product: a reusable Scala/ZIO engine and a thin standalone router.

## Executive decision

The majority baseline is strong and should be present before the standalone router is called production-capable:

- one typed configuration model, exposed programmatically by the embeddable engine and through strict file/environment/CLI decoding by the host;
- file and provider-based artifact delivery, candidate validation, atomic hot reload, last-known-good retention, and observable reload failures;
- distinct liveness and readiness, with readiness requiring an active valid graph generation;
- signal-aware graceful shutdown that stops admission, drains finite work to a deadline, interrupts what remains, closes generation resources, and flushes telemetry;
- structured logs plus OpenTelemetry traces and metrics, including context propagation to subgraphs and low-cardinality generation, plan-cache, execution, source-fetch, and reload signals;
- explicit allowlist-oriented header propagation, router-wide and per-source deadlines, client-disconnect cancellation, bounded concurrency, and hard parser/request/planner limits;
- optional persisted-operation and APQ support with pluggable storage;
- stable, coarse-grained in-process extension seams that preserve streaming and cancellation.

The baseline does **not** justify retrying arbitrary subgraph operations automatically. Only Cosmo exposes broad configurable execution retries; Hive and Fusion visibly retry artifact/control-plane work but not ordinary GraphQL fetches, and Apollo does not expose a comparable general subgraph-retry policy. The Scala remote-GraphQL module should therefore provide an opt-in, strictly bounded, deadline-aware retry policy for operations proven replay-safe; it must default off and never retry mutations merely because the transport failed.

The reusable engine owns semantic deadlines/cancellation, generation lifecycle, guardrails, instrumentation events, and extension contracts. The standalone host owns HTTP admission, config decoding, artifact acquisition, probes, signals, exporters, and packaging. Persisted-operation stores, external artifact providers, and protocol-specific resilience are modules. Dynamic plugin loading, external coprocessing, a control plane, response caching, rate limiting, authorization policy, and a traffic-shaping product remain later milestones.

## Comparative findings

| Concern | Apollo Router | Hive Router | Cosmo Router | Hot Chocolate Fusion | Majority conclusion |
| --- | --- | --- | --- | --- | --- |
| Configuration | Typed Rust model generated into a validated YAML schema; file/env/CLI entry points [A1] | Strict typed YAML/JSON model, env overrides, generated JSON Schema [H1] | Typed YAML/env model plus maintained JSON Schema [C1] | Code-first builder/DI and ordinary ASP.NET configuration [F1] | **3 stock-router file configs + 1 host-builder model.** One typed domain model, with both programmatic and standalone decoding surfaces. |
| Artifact delivery and reload | Uplink polling or watched file; build candidate, retain old config on failure, bounded reload retry [A2] | File/Hive/GraphOS/storage loaders with polling; failed loads leave published runtime unchanged [H2] | Control-plane/file/storage inputs and polling; failed candidate explicitly keeps old server [C2] | `.far` file watcher, Nitro subscription, or custom provider; invalid file updates are ignored [F2] | **4/4 provider abstraction, hot replacement, last-known-good.** Delivery belongs to host/provider modules; atomic generation publication belongs to engine runtime. |
| Health/readiness | Dedicated liveness/readiness; readiness can reflect load shedding [A3] | `/health` is process health and `/readiness` requires an available supergraph [H3] | Dedicated health/liveness/readiness paths, ready only after graph server activation [C3] | Inherits ASP.NET hosting; Fusion itself is a library/template, not a stock daemon [F1] | **3/3 standalone routers expose probes; framework-hosted Fusion delegates them.** The Scala standalone host must expose both. |
| Graceful shutdown | SIGTERM stops admission and drains active requests [A4] | Stops background tasks, flushes telemetry, then invokes plugin shutdown hooks [H4] | Configurable deadline, HTTP shutdown, generation drain, context cancellation [C3] | Standard ASP.NET host lifecycle around a gateway library [F1] | **Universal host concern.** Engine supplies scoped drain/finalization; host owns signals and exit behavior. |
| Observability | Structured logging; extensive OTel/Prometheus tracing and metrics configuration [A5] | OTel tracing/metrics plus Hive reporting, resource attributes, and client identity [H5] | OTel traces/metrics, Prometheus, access logs, runtime/engine/cache/circuit-breaker metrics [C1] | Diagnostic listener API and optional built-in OTel integration [F3] | **4/4 instrumentation; 3 stock routers configure exporters.** Core emits events/spans; host configures exporters and logs. |
| Header propagation | Global and per-subgraph insert/remove/propagate rules [A6] | Ordered global/per-subgraph request and response rules; always strips hop-by-hop headers [H6] | Global/per-subgraph/router request and response rules [C1] | Standard ASP.NET header-propagation middleware on the named Fusion client [F4] | **4/4 explicit and configurable.** Do not forward all headers implicitly; use safe defaults and ordered allowlist rules. |
| Timeouts and cancellation | Router and per-subgraph timeouts; client cancellation behavior is explicit [A7] | End-to-end router timeout and global/per-subgraph source timeouts abort work [H7] | Router/server and per-subgraph transport timeouts flow through Go contexts [C1][C3] | One execution timeout covers queueing and execution and cancels work [F5] | **4/4.** One request deadline must propagate through planning and all source calls; source budgets may narrow but never extend it. |
| Execution retries | No general user-facing subgraph retry policy; reload retries are explicit [A2] | Retries control-plane/artifact fetches, not normal GraphQL execution [H2] | Configurable jittered retry, circuit breaker, attempts, duration, and expression for subgraph calls [C4] | Host `HttpClient` can be customized; repository-visible retries focus on Nitro/config acquisition [F2] | **No majority for operation retries.** Baseline is opt-in, replay-safe, bounded retries in the remote source module; default off. Artifact-fetch retries are baseline host behavior. |
| Persisted operations | APQ plus managed/local persisted-query manifests and safelisting [A8] | Persisted documents from file/Hive/storage with watching, require-id, and cache controls [H8] | Persisted operations, APQ, safelist, manifests, warmup, storage providers [C1][C5] | Persisted/trusted document pipelines and deterministic persisted routes [F6] | **4/4 optional production feature.** Ship as an optional module using a small engine operation-resolver SPI. |
| Limits | HTTP/parser/aliases/depth/height/root/recursion and source response sizes [A9] | Body/header/token/directive/alias/depth limits plus demand control [H9] | Body/header, parser, operation-name, depth, complexity/cost, rate and concurrency controls [C1] | Parser, validation, planner, request-size, timeout, and concurrency guardrails [F7] | **4/4 layered limits.** Cheap hard safety limits are baseline; business cost/rate policy is later/optional. |
| Extension hooks | Built-ins/native plugins, Rhai lifecycle scripts, optional HTTP coprocessor [A10] | Typed lifecycle plugin hooks plus optional coprocessor [H10] | Compile-time modules around router and origin requests; separate gRPC data-source plugins [C6] | DI, request pipeline middleware, HTTP/socket interceptors, diagnostic listeners [F8] | **4/4 extensible, no common ABI.** Offer typed in-process Scala/ZIO hooks first; dynamic/external plugin systems later. |
| Deployment packaging | Executable, OCI image, Helm chart [A11] | Executable/install script and OCI image [H11] | Executable, OCI image, Helm deployment [C7] | NuGet library and `graphql-gateway` ASP.NET project template [F1] | **Dual distribution matches the corpus.** Publish Maven engine modules plus a runnable distribution and OCI image; Helm can follow the stable host config. |

## What belongs where

### Core engine — mandatory in the first production milestone

The core must remain embeddable and independent of any server framework, config-file format, telemetry backend, or artifact registry. It owns:

1. **Request lifetime.** Every execution receives one ZIO scope and one deadline/cancellation signal. Cancellation from client disconnect, timeout, generation retirement, or host shutdown interrupts planning and all child source fibers. Finalizers close response bodies and release source permits.
2. **Generation lifecycle.** Candidate build/validation, immutable generation publication, request leasing, last-known-good retention, finite-request drain, and finalization are library behavior even when the engine is embedded.
3. **Semantic guardrails.** Parser/token/AST limits, validation depth/alias/directive/root-field limits, planner time/work-queue/expanded-node limits, plan-cache bounds, execution concurrency admission, and response-size/accounting hooks. HTTP header/body byte limits remain host/transport concerns.
4. **Instrumentation contract.** Stable low-allocation events around parse, validate, normalize, plan/cache, execute, source fetch, merge, generation reload, and retirement. The no-op implementation must be near-zero cost. Attribute values derived from operation text, variables, errors, or headers are redacted/off by default, and source/operation labels must be bounded.
5. **Typed extension contract.** Coarse hooks may inspect or replace a request before analysis, observe/short-circuit before execution, wrap source dispatch, and observe the final result. Hooks receive the same scope and cancellation as the request. They must not force body buffering or untyped mutable context on the normal path.
6. **Operation resolution seam.** A request may contain text or an operation identifier. Core asks an `OperationResolver` before parsing; the default accepts text, while persisted-operation modules supply lookup/safelist behavior without contaminating planning or execution.

This placement follows the shared invariant rather than language mechanics. In Scala, ZIO structured concurrency and scopes should make cancellation and resource ownership stronger than ad hoc callbacks, while opaque ids, enums, immutable configuration values, and compile-time module wiring keep public contracts explicit.

### Remote GraphQL execution-source module — mandatory, but not universal core

Protocol-specific policy belongs beside the remote GraphQL source implementation:

- pooled HTTP client and connection settings;
- ordered global and per-subgraph header rules with hop-by-hop stripping;
- trace-context injection;
- per-source timeout narrower than the request deadline;
- request/response byte ceilings;
- optional bounded concurrency and circuit breaker;
- optional retries only when the operation is proven replay-safe, the failure is retryable before a response is committed, attempts and elapsed time are bounded, jitter/backoff is used, and the original deadline still applies.

The default retry policy is disabled. Queries may opt in. Mutations require an explicit future idempotency contract rather than an operation-name heuristic. Local Caliban sources share core cancellation and instrumentation but do not inherit HTTP header or retry policy.

### Standalone router host — mandatory in the first production milestone

The host owns environment-facing mechanics:

- strict YAML/HOCON configuration plus environment and CLI overrides, all decoded into the same immutable typed options accepted by the engine builder;
- unknown-key rejection, cross-field validation, secret redaction, a config-print/validate command, and generated reference/schema documentation;
- artifact sources for an explicit local file and a generic polling/provider SPI; checksums/version identity, bounded fetch retry with jitter, asynchronous candidate build, and reload telemetry;
- GraphQL-over-HTTP listener, request/header byte limits, TLS and connection settings;
- separate liveness and readiness endpoints on a configurable management listener;
- signal handling, readiness withdrawal, admission stop, bounded drain, interruption after the deadline, resource closure, telemetry flush, and deterministic exit status;
- structured JSON logs, OTLP trace/metric export, Prometheus scrape endpoint, trace propagation, and JVM/runtime metrics;
- a runnable JVM distribution and OCI image with non-root defaults and documented Kubernetes probe/shutdown settings.

Readiness semantics are precise: the process is live once the management server can answer; it is ready only while accepting traffic with an active valid generation. A failed reload leaves the current generation serving and readiness true, but records a failed-candidate metric/log. Shutdown sets readiness false before stopping admission. Exporter or control-plane failure must not fail GraphQL requests while a valid local generation exists.

### Optional modules shipped with v1

- **Persisted operations/APQ:** in-memory implementation plus a storage SPI, manifest file provider, safelist/require-id mode, negative caching, and bounded cache. Redis/S3/vendor registries are separate adapters, not engine dependencies.
- **Artifact providers:** watched atomic file and generic HTTP/poll provider are sufficient for the stock host; vendor-specific Hive/GraphOS/Nitro/Cosmo providers can be independent integrations.
- **Resilience:** concurrency limiter, circuit breaker, and the conservative replay-safe retry policy live with remote GraphQL. They are opt-in policy, not implicit engine semantics.
- **In-process integrations:** authentication/context setup, custom telemetry, and policy hooks are Scala dependencies assembled at build time. Their lifecycle is scoped and typed.

### Later milestones

The comparison shows useful capabilities but no need to put them on the first critical path:

- dynamic plugin discovery, binary ABI stability, WASM, scripts, or external HTTP/gRPC coprocessing;
- live reload of the router's own configuration (artifact reload remains baseline);
- built-in authorization policy, rate limiting, response/entity caching, and a full traffic-shaping product;
- vendor control plane, fleet management, usage reporting, and managed persisted-operation registry;
- adaptive load-shedding readiness, advanced cost/demand control, and per-tenant policy;
- Helm chart/operator after the standalone configuration and lifecycle contracts stabilize;
- subscriptions and incremental delivery, whose connection drain and generation retirement semantics are designed separately in the next milestone.

## Required operational invariants and acceptance checks

These are the minimum consequences of the decision, not optional polish:

1. Starting without a usable artifact keeps readiness at `503`; liveness remains `200` once the host itself is running.
2. A corrupt, incompatible, or resource-exhausting replacement never changes the active generation. A later valid artifact can still activate.
3. One request never mixes generations, and one deadline/cancellation tree reaches every parallel source fetch.
4. Client disconnect and request timeout cancel outstanding remote calls and release permits/body resources; no detached fiber survives request scope.
5. SIGTERM withdraws readiness before admission stops, lets accepted unary requests finish to a configurable deadline, interrupts survivors, closes source pools, and flushes telemetry within a second bounded deadline.
6. Hop-by-hop headers are never forwarded. Credential/cookie forwarding requires explicit rules, and telemetry never records them by default.
7. A mutation is never retried automatically. An opted-in replay-safe query cannot exceed its attempt, elapsed-time, or parent-deadline budget.
8. Oversized HTTP input is rejected before GraphQL parsing; token/AST limits precede validation; validation and planner guardrails precede source dispatch.
9. OTel trace context reaches remote subgraphs; plan-cache hit/miss, source latency/outcome, active generation, reload outcome, in-flight count, cancellation reason, and shutdown drain are observable without unbounded labels.
10. The same engine behavior can be constructed programmatically without the standalone host, and host config decoding has conformance tests against the programmatic options.

## Meaningful divergences not to flatten

### Fusion is a framework-hosted gateway

Fusion's `graphql-gateway` template is intentionally a few ASP.NET builder calls rather than a preconfigured daemon [F1]. By inference, generic process lifecycle and probes are responsibilities of the chosen ASP.NET host rather than Fusion's execution package. That validates the embeddable-engine half of the Scala product, but it does not remove the need for a stock operational host: Apollo, Hive, and Cosmo users get probes, config, exporters, and packaging without writing an application. The Scala deliverable should support both modes over the same options and lifecycle.

### Retry policy is not a shared GraphQL semantic

Cosmo defaults a configurable subgraph retry structure with an expression over status, connection, and timeout failures [C4]. The other implementations are much more visible about retrying artifact/control-plane acquisition than replaying arbitrary GraphQL fetches [A2][H2][F2]. Retrying can amplify overload and mutations are not generally idempotent. A library mechanism is useful, but enabling it is an operational decision constrained by operation semantics and the original deadline.

### Extension mechanisms reflect implementation language

Rust registries and scripts, Go compile-time modules, and .NET DI/middleware are not interoperable standards [A10][H10][C6][F8]. What is common is a staged request/source lifecycle and scoped cleanup. Scala should copy that semantic shape, using typed ZIO layers/aspects and immutable context, without promising a dynamic ABI or inserting allocation-heavy interception at every plan node.

## Primary-source references

All local paths are relative to `gateway-plan/`. Official links are pinned to the exact reviewed commits.

### Apollo Router — `ce52c982afedb6636e915a2affeb4a27cfbbd53a`

- **[A1] Typed validated configuration model:** `sources/apollo-router/apollo-router/src/configuration/mod.rs:144-215,249-319`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/src/configuration/mod.rs#L144-L215)
- **[A2] Hot reload, last-known-good behavior, and bounded reload retry:** `sources/apollo-router/docs/source/routing/configuration/hot-reload-schema.mdx:9-55`; `sources/apollo-router/apollo-router/src/state_machine.rs:551-735`. [Official pinned reload documentation](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/docs/source/routing/configuration/hot-reload-schema.mdx#L9-L55)
- **[A3] Health, liveness, and load-aware readiness:** `sources/apollo-router/docs/source/routing/self-hosted/health-checks.mdx:9-109`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/docs/source/routing/self-hosted/health-checks.mdx#L9-L109)
- **[A4] SIGTERM admission stop and request drain:** `sources/apollo-router/docs/source/routing/self-hosted/containerization/kubernetes/other-considerations.mdx:55-59`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/docs/source/routing/self-hosted/containerization/kubernetes/other-considerations.mdx#L55-L59)
- **[A5] Logging, OTLP/Prometheus metrics and tracing:** `sources/apollo-router/docs/shared/config/telemetry.mdx:1-220`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/docs/shared/config/telemetry.mdx#L1-L220)
- **[A6] Global/per-subgraph header rules:** `sources/apollo-router/docs/shared/config/headers.mdx:1-12`; `sources/apollo-router/docs/source/routing/configuration/yaml.mdx:828-1040`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/docs/source/routing/configuration/yaml.mdx#L828-L1040)
- **[A7] Router/subgraph timeouts and client cancellation:** `sources/apollo-router/docs/source/routing/performance/traffic-shaping.mdx:13-96`; `sources/apollo-router/docs/source/routing/configuration/yaml.mdx:544-574`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/docs/source/routing/performance/traffic-shaping.mdx#L13-L96)
- **[A8] Persisted queries, APQ, safelist and prewarm:** `sources/apollo-router/docs/shared/config/persisted_queries.mdx:1-13`; `sources/apollo-router/docs/shared/config/apq.mdx:1-28`. [Official pinned persisted-query config](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/docs/shared/config/persisted_queries.mdx#L1-L13)
- **[A9] Layered request/parser/operation limits:** `sources/apollo-router/docs/shared/config/limits.mdx:1-23`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/docs/shared/config/limits.mdx#L1-L23)
- **[A10] Rhai, coprocessor, native/custom-binary lifecycle extensions:** `sources/apollo-router/docs/source/routing/customization/overview.mdx:9-99`. [Official pinned source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/docs/source/routing/customization/overview.mdx#L9-L99)
- **[A11] OCI and Helm packaging:** `sources/apollo-router/dockerfiles/Dockerfile.router`; `sources/apollo-router/helm/chart/router/Chart.yaml`. [Official pinned Dockerfile](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/dockerfiles/Dockerfile.router)

### Hive Router — `0299232a3e039e2b3cbe2cfb9dbc952f687ab79c`

- **[H1] Strict router configuration and generated schema:** `sources/hive-router/lib/router-config/src/lib.rs:54-147,231-307`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/router-config/src/lib.rs#L54-L147)
- **[H2] File/registry/storage sources, polling, artifact retry and last-known-good loop:** `sources/hive-router/lib/router-config/src/supergraph.rs:9-175`; `sources/hive-router/bin/router/src/schema_state.rs:545-595`. [Official pinned source configuration](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/router-config/src/supergraph.rs#L9-L175)
- **[H3] Process health and graph-aware readiness:** `sources/hive-router/bin/router/src/http_utils/probes.rs:7-21`; `sources/hive-router/bin/router/src/schema_state.rs:322-327`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/bin/router/src/http_utils/probes.rs#L7-L21)
- **[H4] Background-task stop, telemetry flush, plugin shutdown:** `sources/hive-router/bin/router/src/lib.rs:506-534`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/bin/router/src/lib.rs#L506-L534)
- **[H5] OTel/Hive tracing and metrics configuration:** `sources/hive-router/lib/router-config/src/telemetry.rs:1-102`; `sources/hive-router/bin/router/src/telemetry.rs:215-291`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/router-config/src/telemetry.rs#L1-L102)
- **[H6] Ordered safe header propagation:** `sources/hive-router/lib/router-config/src/headers.rs:1-156`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/router-config/src/headers.rs#L1-L156)
- **[H7] End-to-end and per-subgraph timeouts:** `sources/hive-router/lib/router-config/src/traffic_shaping.rs:8-230`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/router-config/src/traffic_shaping.rs#L8-L230)
- **[H8] Persisted-document providers, watching, caches and require-id:** `sources/hive-router/lib/router-config/src/persisted_documents.rs:13-226`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/router-config/src/persisted_documents.rs#L13-L226)
- **[H9] Body/header and GraphQL structural limits:** `sources/hive-router/lib/router-config/src/limits.rs:1-101`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/router-config/src/limits.rs#L1-L101)
- **[H10] Typed lifecycle hooks and shutdown/reload hooks:** `sources/hive-router/lib/executor/src/plugins/plugin_trait.rs:1-120,430-605`. [Official pinned source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/executor/src/plugins/plugin_trait.rs#L430-L605)
- **[H11] Standalone container packaging:** `sources/hive-router/docker/router.Dockerfile`; `sources/hive-router/install.sh`. [Official pinned Dockerfile](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/docker/router.Dockerfile)

### Cosmo Router — `5edbee289ba54cab1f2e3639b231f1747ead8aa6`

- **[C1] Typed YAML/env configuration covering telemetry, headers, traffic, probes, limits, persisted operations and plugins:** `sources/cosmo/router/pkg/config/config.go:1-340,500-585,1098-1260,1467-1550`; `sources/cosmo/router/pkg/config/config.schema.json`. [Official pinned configuration](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/pkg/config/config.go#L1467-L1550)
- **[C2] Artifact providers and last-successful polling state:** `sources/cosmo/router/pkg/controlplane/configpoller/split_config_poller.go:30-245`; `sources/cosmo/router/core/router.go:639-652`. [Official pinned poller](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/pkg/controlplane/configpoller/split_config_poller.go#L30-L245)
- **[C3] Liveness/readiness, atomic graph swap, HTTP shutdown and generation drain:** `sources/cosmo/router/core/http_server.go:20-188`; `sources/cosmo/router/core/supervisor.go:102-176`. [Official pinned HTTP server](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/http_server.go#L20-L188)
- **[C4] Configurable bounded subgraph retry and circuit breaker:** `sources/cosmo/router/pkg/config/config.go:221-310`; `sources/cosmo/router/core/retry_builder.go:1-153`. [Official pinned retry configuration](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/pkg/config/config.go#L221-L310)
- **[C5] Persisted operations/APQ stores, manifests, safelist and warmup:** `sources/cosmo/router/pkg/config/config.go:1116-1254`; `sources/cosmo/router/core/cache_warmup.go:1-220`. [Official pinned configuration](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/pkg/config/config.go#L1116-L1254)
- **[C6] Compile-time router/origin module lifecycle:** `sources/cosmo/router/core/modules.go:20-178`. [Official pinned source](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/core/modules.go#L20-L178)
- **[C7] Router OCI and Helm packaging:** `sources/cosmo/router/Dockerfile`; `sources/cosmo/helm/cosmo/Chart.yaml`. [Official pinned Dockerfile](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/Dockerfile)

### Hot Chocolate Fusion — `00c61af25908319ee277377652191a5aa8c2f60e`

- **[F1] Embeddable ASP.NET gateway template and host integration:** `sources/hotchocolate-fusion/templates/gateway/Program.cs:1-13`; `sources/hotchocolate-fusion/website/content/docs/fusion/migration/coming-from-apollo-federation.md:240-255`. [Official pinned template](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/templates/gateway/Program.cs#L1-L13)
- **[F2] File/Nitro/custom artifact providers, hot swap and invalid-update retention:** `sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/Configuration/FileSystemFusionConfigurationProvider.cs:16-195`; `sources/hotchocolate-fusion/website/content/docs/fusion/deployment-and-ci-cd.md:6-45,292-335`. [Official pinned file provider](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Execution/Configuration/FileSystemFusionConfigurationProvider.cs#L16-L195)
- **[F3] Diagnostic events and built-in OpenTelemetry integration:** `sources/hotchocolate-fusion/website/content/docs/hotchocolate/server/instrumentation.md:1-260`. [Official pinned source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/website/content/docs/hotchocolate/server/instrumentation.md#L1-L260)
- **[F4] ASP.NET header propagation on Fusion's named HTTP client:** `sources/hotchocolate-fusion/website/content/docs/fusion/migration/coming-from-apollo-federation.md:559-595`. [Official pinned source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/website/content/docs/fusion/migration/coming-from-apollo-federation.md#L559-L595)
- **[F5] Concurrency and uniform execution cancellation:** `sources/hotchocolate-fusion/website/content/docs/fusion/performance-tuning.md:156-214`. [Official pinned source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/website/content/docs/fusion/performance-tuning.md#L156-L214)
- **[F6] Persisted/trusted document pipeline and routes:** `sources/hotchocolate-fusion/website/content/docs/fusion/guides/first-party-api.md:14-107`; `sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/DependencyInjection/CoreFusionGatewayBuilderExtensions.Pipeline.cs:92-234`. [Official pinned guide](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/website/content/docs/fusion/guides/first-party-api.md#L14-L107)
- **[F7] Parser, validation, planner, request and execution limits:** `sources/hotchocolate-fusion/website/content/docs/fusion/request-limits.md:6-166`. [Official pinned source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/website/content/docs/fusion/request-limits.md#L6-L166)
- **[F8] Composable request pipeline and HTTP/socket interceptors:** `sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.Execution/DependencyInjection/CoreFusionGatewayBuilderExtensions.Pipeline.cs:1-234`; `sources/hotchocolate-fusion/src/HotChocolate/Fusion/src/Fusion.AspNetCore/DependencyInjection/AspNetCoreFusionGatewayBuilderExtensions.HttpRequestInterceptor.cs:1-145`. [Official pinned pipeline](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Execution/DependencyInjection/CoreFusionGatewayBuilderExtensions.Pipeline.cs#L1-L234)

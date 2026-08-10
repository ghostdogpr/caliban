# Cross-gateway audit and benchmark as acceptance evidence

## Decision

The two repositories are valuable **inputs** to acceptance, not acceptance oracles in their current form.

- Adopt the Federation Gateway Audit's 46 suites, 199 operations, source schemas, deterministic fixture data, and exact expected `data` values as a seeded Federation execution corpus. Add two Caliban adapters: one that imports the audit's Apollo-composed JOIN supergraph to isolate router execution, and one that consumes the suite's subgraph schemas and runs the Caliban composer before execution.
- Fork the audit runner before using it as a release gate. The fork must assert the complete relevant GraphQL response and HTTP envelope, make absence of errors explicit, preserve stable named case IDs, remove the ineffective retry, retain raw failures, and add composition, ordinary GraphQL, remote/local/mixed-source, reload, cancellation, and protocol cases.
- Adopt the GraphQL Gateways Benchmark's Rust subgraphs, four-source Federation graph, heavy nested query, process-group lifecycle, CPU-affinity idea, k6 raw output, and resource sampling as one **heavy entity-join seed workload**.
- Fork the benchmark before comparing the Scala router. Add the other four agreed workloads, Hot Chocolate and Caliban adapters, aligned gateway revisions and configurations, an open-model offered-load test, repeated randomized runs, JVM warmup, isolated CPUs for router/load/sources, correctness-weighted useful throughput, allocation/GC metrics, and a run manifest. Exclude k6 setup traffic from measured statistics.
- Never use either checked-in leaderboard as the release gate. Audit percentages are neither normative coverage nor current self-consistent results, and benchmark rankings describe one repeated query on one run/configuration. A sample with a wrong response is invalid regardless of HTTP 200 or RPS.

This qualifies, but does not reverse, the resolved compatibility and performance decisions. The normative specifications remain above implementation majority. The audit becomes a seed corpus beneath that normative layer; the benchmark becomes one scenario beneath the five-workload, JVM-aware performance gate.

## Source basis

| Repository | Pinned commit | Official source |
|---|---|---|
| Federation Gateway Audit | `7956ca1cabd08e02b1baee91e17457ee0847d784` | [the-guild-org/graphql-federation-gateway-audit](https://github.com/the-guild-org/graphql-federation-gateway-audit/tree/7956ca1cabd08e02b1baee91e17457ee0847d784) |
| GraphQL Gateways Benchmark | `84b62bab2267ae7b31d4bf18a80e7691cffdf5ba` | [graphql-hive/graphql-gateways-benchmark](https://github.com/graphql-hive/graphql-gateways-benchmark/tree/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba) |

All local paths below are exact and relative to the Caliban repository root.

Principal audit evidence:

- `gateway-plan/sources/federation-gateway-audit/README.md`
- `gateway-plan/sources/federation-gateway-audit/REPORT.md`
- `gateway-plan/sources/federation-gateway-audit/Makefile`
- `gateway-plan/sources/federation-gateway-audit/package.json`
- `gateway-plan/sources/federation-gateway-audit/src/index.ts`
- `gateway-plan/sources/federation-gateway-audit/src/test.ts`
- `gateway-plan/sources/federation-gateway-audit/src/testkit.ts`
- `gateway-plan/sources/federation-gateway-audit/src/subgraph.ts`
- `gateway-plan/sources/federation-gateway-audit/src/supergraph.ts`
- `gateway-plan/sources/federation-gateway-audit/src/cli.ts`
- `gateway-plan/sources/federation-gateway-audit/src/summary.ts`
- `gateway-plan/sources/federation-gateway-audit/src/test-suites/`
- `gateway-plan/sources/federation-gateway-audit/gateways/`
- `gateway-plan/sources/federation-gateway-audit/.github/workflows/ci.yaml`

Principal benchmark evidence:

- `gateway-plan/sources/graphql-gateways-benchmark/README.md`
- `gateway-plan/sources/graphql-gateways-benchmark/k6.js`
- `gateway-plan/sources/graphql-gateways-benchmark/test.sh`
- `gateway-plan/sources/graphql-gateways-benchmark/monitor.sh`
- `gateway-plan/sources/graphql-gateways-benchmark/toolkit/main.rs`
- `gateway-plan/sources/graphql-gateways-benchmark/subgraphs/main.rs`
- `gateway-plan/sources/graphql-gateways-benchmark/subgraphs/accounts.rs`
- `gateway-plan/sources/graphql-gateways-benchmark/subgraphs/inventory.rs`
- `gateway-plan/sources/graphql-gateways-benchmark/subgraphs/products.rs`
- `gateway-plan/sources/graphql-gateways-benchmark/subgraphs/reviews.rs`
- `gateway-plan/sources/graphql-gateways-benchmark/gateways/`
- `gateway-plan/sources/graphql-gateways-benchmark/.github/workflows/benchmark.template.yaml`
- `gateway-plan/sources/graphql-gateways-benchmark/.github/workflows/federation-v1.workflow.yaml`
- `gateway-plan/sources/graphql-gateways-benchmark/.github/workflows/decide-runner.template.yaml`
- `gateway-plan/sources/graphql-gateways-benchmark/constant-vus-over-time_result.md`
- `gateway-plan/sources/graphql-gateways-benchmark/ramping-vus_result.md`

## What the Federation Gateway Audit establishes

### Corpus and execution topology

The audit currently registers 46 suite directories and contains 199 `createTest` calls. Its useful center of gravity is distributed execution: Federation 1/2 external/extension spellings; entity keys and multi-hop calls; `@requires` including arguments, fragments, chains, conflicts, and cycles; nested `@provides`; `@override`; `@inaccessible`; interface objects; abstract types; distributed unions/interfaces; input and enum intersection; shared roots; aliases/`__typename`; conditional inclusion; null keys; and top-level mutations. The authoritative inventory is the explicit import list in `gateway-plan/sources/federation-gateway-audit/src/index.ts` and the schemas and operations beneath `gateway-plan/sources/federation-gateway-audit/src/test-suites/` ([suite registry](https://github.com/the-guild-org/graphql-federation-gateway-audit/blob/7956ca1cabd08e02b1baee91e17457ee0847d784/src/index.ts), [suite tree](https://github.com/the-guild-org/graphql-federation-gateway-audit/tree/7956ca1cabd08e02b1baee91e17457ee0847d784/src/test-suites)).

Each suite exposes GraphQL Yoga subgraphs built with Apollo `buildSubgraphSchema`. The audit lazily starts the schema/server and exposes both GET and POST endpoints; errors are unmasked. For the normal adapters, the audit composes those schemas with `@apollo/composition` 2.14.1 and serves a per-suite supergraph SDL whose routing URLs point back to the fixture server. Thus a normal audit run tests **execution of an Apollo-composed artifact**, not the candidate gateway's composer. See `gateway-plan/sources/federation-gateway-audit/src/subgraph.ts`, `gateway-plan/sources/federation-gateway-audit/src/supergraph.ts`, and `gateway-plan/sources/federation-gateway-audit/package.json` ([subgraph host](https://github.com/the-guild-org/graphql-federation-gateway-audit/blob/7956ca1cabd08e02b1baee91e17457ee0847d784/src/subgraph.ts), [supergraph composition](https://github.com/the-guild-org/graphql-federation-gateway-audit/blob/7956ca1cabd08e02b1baee91e17457ee0847d784/src/supergraph.ts), [versions](https://github.com/the-guild-org/graphql-federation-gateway-audit/blob/7956ca1cabd08e02b1baee91e17457ee0847d784/package.json)).

The Hot Chocolate adapter demonstrates the complementary route. It downloads each suite's subgraph schema/URL list, invokes Fusion composition, and runs the generated archive. It also applies suite-specific compatibility options for non-resolvable interface objects, global node resolution, and partial-union routing. This is evidence that a non-JOIN implementation can reuse the fixtures, but also that an adapter can change semantics by selecting product-specific flags. See `gateway-plan/sources/federation-gateway-audit/gateways/hot-chocolate-fusion/run.sh`, `gateway-plan/sources/federation-gateway-audit/gateways/hot-chocolate-fusion/Program.cs`, and `gateway-plan/sources/federation-gateway-audit/gateways/hot-chocolate-fusion/HotChocolate.Fusion.AuditGateway.csproj` ([Fusion adapter](https://github.com/the-guild-org/graphql-federation-gateway-audit/blob/7956ca1cabd08e02b1baee91e17457ee0847d784/gateways/hot-chocolate-fusion/run.sh)).

For Caliban, both modes are required:

1. **JOIN-import mode:** fetch `/suite/supergraph`, import it, and test planner/executor behavior independently of Caliban composition.
2. **Native-composition mode:** fetch `/suite/subgraphs`, run the Caliban composer, activate the resulting artifact, and execute the same operation. This detects composition differences hidden by JOIN-import mode.

The same fixtures should later be parameterized so a fixture subgraph may be remote or an in-process Caliban source. The audit itself only supplies remote Yoga/Apollo subgraphs, so it cannot establish ordinary single-source GraphQL or mixed local/remote compatibility.

### What its assertion actually means

`createTest` stores only a query string and expected `{ data?, errors?: boolean }`. The runner POSTs JSON `{query}`, parses any JSON response regardless of HTTP status or media type, compares `data` deeply, and—only when `errors` is explicitly boolean—compares whether the returned errors array is non-empty. Twelve of 199 cases specify `errors: true`; zero specify `errors: false`. Consequently, 187 cases ignore unexpected errors as long as `data` equals the expected value. Error messages, codes, locations, paths, extensions, count, ordering, and partial-data relationships are never compared. The retry loop compares the same already-received value three times and never reissues the request. See `gateway-plan/sources/federation-gateway-audit/src/testkit.ts` and `gateway-plan/sources/federation-gateway-audit/src/test.ts` ([test shape](https://github.com/the-guild-org/graphql-federation-gateway-audit/blob/7956ca1cabd08e02b1baee91e17457ee0847d784/src/testkit.ts), [runner oracle](https://github.com/the-guild-org/graphql-federation-gateway-audit/blob/7956ca1cabd08e02b1baee91e17457ee0847d784/src/test.ts)).

The runner therefore establishes this narrow proposition: for each checked-in operation, a gateway can produce the expected `data`, and for 12 selected operations it produces at least one error. It does **not** establish GraphQL response completion, error-path rewriting, absence of extra errors, GraphQL-over-HTTP conformance, variables/coercion, operation selection, content negotiation, GET behavior, malformed input behavior, timeout/cancellation, streaming, reload, or composition diagnostics.

The README's directive list is also not a coverage proof. For example it lists `@composeDirective`, but no file under `gateway-plan/sources/federation-gateway-audit/src/test-suites/` contains that directive. The README explicitly excludes `@authenticated`, `@policy`, `@requiresScopes`, and progressive `@override(label:)`; the operation shape cannot test their policy semantics in any case. See `gateway-plan/sources/federation-gateway-audit/README.md` ([claimed and excluded coverage](https://github.com/the-guild-org/graphql-federation-gateway-audit/blob/7956ca1cabd08e02b1baee91e17457ee0847d784/README.md)).

### Why the checked-in percentages are not an oracle

The checked-in artifacts are internally out of date. `README.md` and `REPORT.md` omit Hot Chocolate, while `gateway-plan/sources/federation-gateway-audit/gateways/hot-chocolate-fusion/results.txt` contains 199/199 across all 46 suites. Other checked-in result files contain only 189 or 192 historical cases: the report generator imputes every missing newer suite/case as failed to reach a 199-case denominator. For example the Apollo result files omit four suites and the three Hive result files omit the two new partial-union suites. The generator's imputation behavior is in `gateway-plan/sources/federation-gateway-audit/src/summary.ts`; the current results are under `gateway-plan/sources/federation-gateway-audit/gateways/` ([summary generator](https://github.com/the-guild-org/graphql-federation-gateway-audit/blob/7956ca1cabd08e02b1baee91e17457ee0847d784/src/summary.ts), [Hot Chocolate results](https://github.com/the-guild-org/graphql-federation-gateway-audit/blob/7956ca1cabd08e02b1baee91e17457ee0847d784/gateways/hot-chocolate-fusion/results.txt)).

The audit is maintained by the Hive/The Guild organization and its highest checked-in result is Hive Router. That does not invalidate individual fixtures; it means fixture provenance and adapter configuration must be visible, and ambiguous cases must be checked against specifications and all four primary gateway sources rather than resolved from the score. Versions are snapshots: at this pin Apollo Router is 2.15.0, Cosmo 0.321.2, Hive Router 0.0.71, Hive Gateway 2.8.3, Grafbase 0.53.5, and Fusion 16.5.0 preview. Exact installer/adapters live under `gateway-plan/sources/federation-gateway-audit/gateways/` ([gateway adapters](https://github.com/the-guild-org/graphql-federation-gateway-audit/tree/7956ca1cabd08e02b1baee91e17457ee0847d784/gateways)).

### Adopt, adapt, and extend

| Part | Recommendation | Reason |
|---|---|---|
| Source schemas, deterministic resolvers/data, operations, expected `data` | Adopt as imported seed fixtures, retaining upstream commit and license metadata | They cover valuable distributed shapes with existing cross-product experience. |
| Per-suite remote fixture server | Adopt initially through an external-test integration | It gives every implementation the same HTTP subgraphs. Do not make Node/Yoga a production-test dependency for all lower-level Scala suites. |
| `gateway.json` plus `install.sh`/`run.sh` convention | Adapt into a Caliban audit adapter | It is a small stable interoperability surface, but the adapter must expose JOIN-import and native-composition modes. |
| Runner and result format | Fork | The boolean error oracle, status blindness, unnamed indices, ineffective retry, and stale aggregate format are not release-grade. |
| Leaderboard percentages | Do not adopt | They mix missing-case imputation, stale outputs, changing versions, and non-normative coverage. |
| Missing semantics | Extend | Add GraphQL/HTTP, full errors/completion, composition failures, standalone graph, local/mixed sources, reload, cancellation, downstream faults, streaming placeholders, and advanced Federation milestones. |

## What the GraphQL Gateways Benchmark establishes

### Workload, data, and load model

The benchmark hosts four in-memory Rust/async-graphql subgraphs—accounts, inventory, products, and reviews—on one Axum process. The fixed data contains six users, nine products/inventory entries, and eleven reviews. Its only operation selects all users and top products, recursively traverses reviews, authors, and products, and exercises `@key`, `@requires`, and `@provides`. It returns a large response and performs multiple entity transitions; this is a useful heavy merge/join case. See `gateway-plan/sources/graphql-gateways-benchmark/subgraphs/accounts.rs`, `gateway-plan/sources/graphql-gateways-benchmark/subgraphs/inventory.rs`, `gateway-plan/sources/graphql-gateways-benchmark/subgraphs/products.rs`, `gateway-plan/sources/graphql-gateways-benchmark/subgraphs/reviews.rs`, and `gateway-plan/sources/graphql-gateways-benchmark/k6.js` ([load script](https://github.com/graphql-hive/graphql-gateways-benchmark/blob/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/k6.js), [subgraphs](https://github.com/graphql-hive/graphql-gateways-benchmark/tree/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/subgraphs)).

Constant mode is a closed model with 50 VUs for 60 seconds. Stress mode is also closed: it ramps VUs 0→50 over 10 seconds, 50→500 over 40 seconds, then 500→50 over 10 seconds. Each request is the same query bytes with no variables or operation-name diversity. In-flight request deduplication is explicitly enabled in Hive Gateway, Hive Router, and Apollo Router adapters; exact repeated concurrent requests can therefore reward coalescing in a way unlike a diverse production mix. This behavior is valid as a separately named coalescible workload, not as the general throughput gate. See `gateway-plan/sources/graphql-gateways-benchmark/k6.js`, `gateway-plan/sources/graphql-gateways-benchmark/gateways/hive-gateway/gateway.config.ts`, `gateway-plan/sources/graphql-gateways-benchmark/gateways/hive-router/router.config.yaml`, and `gateway-plan/sources/graphql-gateways-benchmark/gateways/apollo-router/config.yaml` ([k6 configuration](https://github.com/graphql-hive/graphql-gateways-benchmark/blob/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/k6.js), [Apollo adapter](https://github.com/graphql-hive/graphql-gateways-benchmark/blob/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/gateways/apollo-router/config.yaml)).

There are two warmup mechanisms. `test.sh` runs a 15-second constant-mode k6 process before measurement. The measured k6 process then runs `setup()`, which sends `2 × VUs` additional requests. Those setup requests are included in k6's HTTP metrics: the checked-in constant outputs have 100 more `http_reqs` than iterations and stress outputs have 1,000 more. Thus current RPS and latency aggregates are contaminated by setup traffic. See `gateway-plan/sources/graphql-gateways-benchmark/test.sh`, `gateway-plan/sources/graphql-gateways-benchmark/k6.js`, `gateway-plan/sources/graphql-gateways-benchmark/constant-vus-over-time_result.md`, and `gateway-plan/sources/graphql-gateways-benchmark/ramping-vus_result.md` ([orchestrator](https://github.com/graphql-hive/graphql-gateways-benchmark/blob/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/test.sh), [constant result](https://github.com/graphql-hive/graphql-gateways-benchmark/blob/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/constant-vus-over-time_result.md)).

### Controls and configurations

On Linux, `test.sh` reserves CPU 0 for k6 and assigns the router cores 1..N-1, using process groups where available. The CI scenarios set `CPU_LIMIT=3` for constant mode and 4 for stress, producing two and three router cores respectively. However the four subgraphs and Prometheus/Grafana services are not pinned away from those cores. On macOS or systems without `taskset`/`setsid`, pinning is skipped. `MEM_LIMIT` is not a dependable control: if cgroup tools exist the script references an unset `mem_bytes`; otherwise it only warns and applies no limit. Readiness is explicitly configured only for Hive Gateway variants; other adapters get a two-second survival check before warmup. See `gateway-plan/sources/graphql-gateways-benchmark/test.sh`, `gateway-plan/sources/graphql-gateways-benchmark/subgraphs/main.rs`, `gateway-plan/sources/graphql-gateways-benchmark/.github/workflows/benchmark.template.yaml`, and `gateway-plan/sources/graphql-gateways-benchmark/.github/workflows/federation-v1.workflow.yaml` ([CI scenario](https://github.com/graphql-hive/graphql-gateways-benchmark/blob/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/.github/workflows/federation-v1.workflow.yaml)).

Worker/config parity is approximate. Apollo Gateway's adapter interprets `FORK` as `workers + 1`, so CI `FORK=2` starts one worker while Rust processes use the assigned core set directly. Hive Gateway enables `--jit` and inbound request deduplication. Hive Router logs at info while most other native adapters disable telemetry and use fatal logging. Cosmo consumes a pre-generated engine JSON, whereas the others consume semantically equivalent JOIN v0.3 SDL; the Apollo Gateway SDL differs only in formatting. These are legitimate product configurations, but the run manifest must declare them and the acceptance comparison must give each router the same core budget and an explicitly justified production configuration. See `gateway-plan/sources/graphql-gateways-benchmark/gateways/apollo-gateway/index.ts`, `gateway-plan/sources/graphql-gateways-benchmark/gateways/hive-gateway/package.json`, `gateway-plan/sources/graphql-gateways-benchmark/gateways/hive-router/run.sh`, and `gateway-plan/sources/graphql-gateways-benchmark/gateways/cosmo/config.json` ([Apollo Gateway adapter](https://github.com/graphql-hive/graphql-gateways-benchmark/blob/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/gateways/apollo-gateway/index.ts), [Cosmo artifact](https://github.com/graphql-hive/graphql-gateways-benchmark/blob/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/gateways/cosmo/config.json)).

The compared binaries are older than the audit/source corpus: Apollo Router 2.10.1, Cosmo 0.292.0, Hive Router 0.0.43, Hive Gateway 2.7.2, and Grafbase 0.53.2. Hot Chocolate is absent. Therefore the checked-in ranking cannot be attributed to the four pinned implementation revisions used by the architecture research. Exact versions are in `gateway-plan/sources/graphql-gateways-benchmark/gateways/apollo-router/install.sh`, `gateway-plan/sources/graphql-gateways-benchmark/gateways/cosmo/install.sh`, `gateway-plan/sources/graphql-gateways-benchmark/gateways/hive-router/install.sh`, `gateway-plan/sources/graphql-gateways-benchmark/gateways/hive-gateway/package.json`, and `gateway-plan/sources/graphql-gateways-benchmark/gateways/grafbase/install.sh` ([gateway tree](https://github.com/graphql-hive/graphql-gateways-benchmark/tree/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/gateways)).

### Metrics and correctness oracle

k6 reports HTTP request count/rate, latency average/min/median/max/p90/p95/p99.9, and a custom success rate. The monitor samples aggregate process-group CPU and RSS every 200 ms and the toolkit retains only maximum CPU and maximum RSS. Despite comments mentioning PSS, the monitor records no PSS, allocation, GC, live-set, direct-buffer, downstream-call, connection-reuse, or plan/cache metrics. The workflow has no checked-in hardware manifest for the self-hosted `benchmark-runner-1`, and one checked-in result represents one run rather than a distribution across independent processes. See `gateway-plan/sources/graphql-gateways-benchmark/monitor.sh`, `gateway-plan/sources/graphql-gateways-benchmark/toolkit/main.rs`, and `gateway-plan/sources/graphql-gateways-benchmark/.github/workflows/decide-runner.template.yaml` ([monitor](https://github.com/graphql-hive/graphql-gateways-benchmark/blob/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/monitor.sh), [toolkit](https://github.com/graphql-hive/graphql-gateways-benchmark/blob/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/toolkit/main.rs)).

Correctness has three checks: status 200, absence of the byte substring `"errors"`, and expected response structure. The structure function recursively checks required keys/types but does not assert exact values or reject extras, and `runOnce` performs the expensive parse/structure walk only on the first applicable call per k6 isolate while subsequent calls report success. More importantly, the comparison table's “failed requests” comes from k6 `http_req_failed`, not the custom correctness rate. The checked-in stress report therefore says “0 failed” for Apollo Router while separately noting nine GraphQL errors, and likewise says “0 failed” for a Hive runtime result with an invalid structure. This is direct evidence that the headline request count is not useful-response throughput. See `gateway-plan/sources/graphql-gateways-benchmark/k6.js` and `gateway-plan/sources/graphql-gateways-benchmark/ramping-vus_result.md` ([stress result](https://github.com/graphql-hive/graphql-gateways-benchmark/blob/84b62bab2267ae7b31d4bf18a80e7691cffdf5ba/ramping-vus_result.md)).

### Adopt, adapt, and extend

| Part | Recommendation | Reason |
|---|---|---|
| Four Rust subgraphs, fixed data, supergraph, heavy operation | Adopt as one versioned scenario | It is deterministic and stresses joins, merge, entity batching, and large serialization. |
| k6 request and expected structure | Adapt | Preserve the request as a fixture, but assert an exact normalized result or stable digest on every request and count only correct responses. |
| Lifecycle, CPU affinity, raw k6 output, process-group RSS/CPU | Fork and retain | Useful foundation, but isolation/fallback/resource semantics need repair and more metrics. |
| Constant and stress scenarios | Fork | Add open-model fixed offered rates, explicit saturation search, setup exclusion, and independent repetitions. |
| Current gateway adapters | Re-pin and normalize | Add Caliban and Hot Chocolate; align source revisions, cores, worker counts, logging, telemetry, deduplication policy, readiness, and artifact semantics. |
| Checked-in ranking | Do not adopt | One query, one run, older versions, missing Hot Chocolate, setup contamination, incomplete isolation, and correctness/headline mismatch. |

## Acceptance oracle constrained by this evidence

### Compatibility

Use three layers, in this order:

1. **Normative layer:** GraphQL September 2025, GraphQL-over-HTTP, Federation, and JOIN conformance. Specifications beat any gateway vote.
2. **Imported audit layer:** run all 199 operations in both JOIN-import and native-composition modes. Expected `data` is adopted, but every case must explicitly declare expected error cardinality/class/path policy. A test can be marked product-extension or ambiguous; it cannot silently become normative because several gateways happen to pass it.
3. **Differential layer:** for behavior the specifications genuinely leave open, compare current pinned Apollo, Hive, Cosmo, and Fusion builds. Bias toward the majority only after normalizing vendor wording/codes and documenting configuration. Preserve raw responses and source-call traces so a disagreement is diagnosable.

The release report should be capability based, not a single percentage. Mandatory unary-v1 categories must all pass; later milestone cases remain visibly expected-unsupported. Add full GraphQL responses, HTTP status/media/headers, subgraph-call invariants, composition artifact/diagnostics, and cancellation/reload observations to the case model. Unexpected errors, extra/missing source calls, wrong null propagation, or wrong HTTP classification fail even when `data` happens to match.

### Performance

Keep the existing heavy operation as the **entity-join / large-response** seed, then add the agreed passthrough, single-fetch, parallel-fetch, and error-heavy workloads. Within those workloads include varied operation texts/variables and plan-hit/miss mixes; keep identical-request coalescing as a separately reported optimization scenario.

For all four primary competitors plus Caliban:

- use release artifacts at recorded revisions and a generated run manifest containing repository commit, binary checksum, config, JDK/runtime, kernel, CPU model/governor, memory, schema/artifact checksum, and driver version;
- reserve disjoint fixed CPUs for load generator, router, and sources; disable or isolate monitoring services; apply equivalent memory/core limits and verify them from the process;
- wait for every endpoint, run product-appropriate runtime warmup plus operation/plan-cache prewarming, and report cold start/reload separately;
- run fixed offered-rate steps with an arrival-rate executor, followed by a saturation search; closed-VU results may remain as a secondary concurrency view;
- execute at least five fresh processes per router/workload, randomize order, and report median plus spread rather than selecting one favorable run;
- exclude setup/warmup requests from measurement; validate every response cheaply but exactly, and compute useful RPS only from correct responses;
- disqualify runs with correctness errors, load-generator saturation, source saturation, dropped iterations outside the scenario policy, or unverified resource isolation;
- report p50/p90/p95/p99/p99.9, achieved/useful RPS, CPU-seconds/request, peak RSS and post-GC live set, allocation bytes/request, GC/direct-buffer metrics, downstream request/byte/connection counts, cache/plan metrics, and overload queueing.

The existing benchmark numbers may be reproduced as a historical sanity check, but the agreed “within roughly 15% of the leader after JVM warmup” decision is evaluated only from this forked multi-workload harness. The gate applies per representative workload to throughput and tail latency; a single aggregate ranking cannot compensate for a catastrophic join, error, or large-response path.

## Refinements to resolved research

The resolved compatibility note remains correct in putting specifications first and requiring a black-box, multi-gateway differential suite. It is now qualified to state that the public audit is only a seed execution corpus: its ordinary path bypasses native composition, it does not cover GraphQL-over-HTTP or full error semantics, and its checked-in leaderboard is stale. Audit pass percentage must not substitute for the normative unary release gate.

The resolved performance note remains correct in requiring five workloads, isolated resources, JVM warmup, repeated runs, correctness, and allocations. It is now qualified to state that the public benchmark supplies only the heavy entity-join seed and historical context. Its current rankings are not evidence for the 15% gate because Hot Chocolate is missing, revisions/configurations differ, one identical query dominates, setup traffic enters metrics, isolation is incomplete, and headline failed requests ignore GraphQL correctness failures.

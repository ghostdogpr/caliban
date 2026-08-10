# Define the compatibility and performance acceptance oracle

Type: `grilling`
Status: `resolved`
Blocked by: 02, 03, 04, 06, 07, 08, 09, 10, 12, 17

## Question

What executable conformance corpus, differential tests, workload matrix, datasets, measurement protocol, hardware controls, warmup rules, statistical thresholds, and regression budgets will determine whether the gateway matches required semantics and remains within the agreed performance envelope of leading gateways?

## Accepted decisions

### Oracle hierarchy

1. Published GraphQL, GraphQL-over-HTTP, and Federation specifications are authoritative.
2. Where a specification deliberately permits alternatives, the project records a versioned Caliban Gateway decision.
3. A maintained executable corpus captures the resulting expected behavior and source-call invariants.
4. Differential results from Apollo Router, Hive Router, Cosmo, and Hot Chocolate Fusion inform decisions only where the specifications leave genuine latitude.

The competitor majority informs a checked-in expectation; it is not recomputed dynamically. A split result requires an explicit project decision. Vendor-specific wording, plan structure, and proprietary error codes are not normative.

### Compatibility gate

The primary external compatibility target is the latest Federation Gateway Audit revision selected when its adapter is implemented and refreshed before release. A gateway-specific adapter code-first composes each suite's subgraphs, following the model already used by Hot Chocolate Fusion. Every in-scope audit case must pass. Small project-owned tests may strengthen assertions where the upstream audit checks only `data`, but they preserve recognizable upstream case identities and behavior rather than becoming a separate conformance suite.

A case may be excluded only when it exercises an explicitly deferred feature or the fixture itself is invalid or ambiguous; the reason is recorded. Assertion failures are not quarantined as flaky. A run may be retried only for a classified infrastructure failure.

### Competitor baselines

The primary external performance target is the latest GraphQL Gateways Benchmark revision selected when its adapter is implemented and refreshed before release. Each run records the resolved suite commits, exact gateway releases or image digests, and effective configuration. Floating versions are forbidden within a run, but the project is not frozen to planning-session snapshots. Updating external suites and competitors is intentional and reviewed.

### Competitive performance gate

The gate applies to the recorded current GraphQL Gateways Benchmark workload and to any workloads later added upstream:

- useful throughput must be at least 85% of the fastest competitor under the same benchmark profile;
- the workload's semantic checks must pass throughout the measured run.

Latency, CPU, and memory are reported so pathologies remain visible, but they have no initial hard acceptance threshold. The current upstream benchmark does not provide the fixed offered-load model needed for a fair cross-router p99 gate, and constructing a second benchmark harness is deliberately deferred. Fast incorrect responses do not contribute to useful throughput.

### Additional project coverage

The project does not initially create a second broad cross-gateway corpus or workload matrix. Focus remains on doing well in the two current external projects.

Small project-owned tests cover capabilities those projects cannot exercise: ordinary remote GraphQL, in-process Caliban, mixed local/remote graphs, lifecycle behavior, and the native composition entry point. Component and in-process benchmarks may diagnose regressions in these paths, but they are not an alternative public leaderboard. A local-only gateway path is compared with direct Caliban execution to prevent gross embedded-path overhead; its final numeric budget is set after the path exists and measurement noise is known.

### Execution tiers

- Pull requests run project unit/property tests, protocol/error/lifecycle checks, the complete in-scope audit through the code-first adapter, and benchmark smoke tests.
- Nightly runs on suitable dedicated hardware execute release-mode component and in-process benchmarks and report allocation/GC regressions.
- Release candidates refresh and record the external versions, run the GraphQL Gateways Benchmark comparison, and enforce the 15% gate.

Performance results from laptops or shared runners are informational. Exact warmup, repetition, and hardware settings belong to the benchmark implementation and are calibrated when that harness is added.

## Resolution

The initial acceptance oracle intentionally stays recognizable and small: maximize the in-scope score in the current Federation Gateway Audit and remain within 15% of the current performance leader in the GraphQL Gateways Benchmark. Project-owned coverage exists only for gateway capabilities those external projects cannot express. A broader custom conformance corpus, five-workload cross-router suite, fixed-load latency gate, and detailed statistical protocol are deferred until implementation evidence justifies them.

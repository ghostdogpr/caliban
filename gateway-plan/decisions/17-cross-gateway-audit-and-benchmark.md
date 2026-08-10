# What do the cross-gateway audit and benchmark establish as acceptance evidence?

Type: `research`
Status: `resolved`

## Question

What compatibility cases, expected semantics, gateway adapters, workloads, datasets, measurement controls, metrics, and known biases are embodied by the pinned Federation Gateway Audit and GraphQL Gateways Benchmark repositories? Determine which parts the Caliban Graph Router can adopt unchanged, which require an adapter or fork, which gaps need new cases, and how this evidence should constrain the later compatibility and performance acceptance oracle. Cross-check any conclusions that refine or correct the resolved compatibility and performance research notes.

## Answer

[Cross-gateway audit and benchmark as acceptance evidence](../research/17-cross-gateway-audit-and-benchmark.md) adopts the audit's 46 suites/199 operations and the benchmark's deterministic heavy entity-join graph as seed fixtures, but requires forks of both oracles. Compatibility must run both JOIN-import and Caliban-native composition modes and assert full GraphQL/HTTP semantics; performance must add the other four agreed workloads, aligned Caliban/Hot Chocolate/current gateway adapters, JVM warmup, isolated resources, repeated open-load runs, and correctness-weighted useful throughput. Neither checked-in leaderboard is a release gate.

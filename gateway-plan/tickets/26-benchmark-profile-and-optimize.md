# 26 — Benchmark, profile, and optimize the real gateway

**Outcome:** The production Quick path is measured against current gateways, and measured bottlenecks are optimized without splitting semantics.

**Blocked by:** 23 — Add the encoded response path; 25 — Improve ambiguous-route planning

**Status:** ready-for-agent

## Completion criteria

- [ ] A non-published project integrates the current GraphQL Gateways Benchmark and records its revision in the shared compatibility version file.
- [ ] The compared Apollo Router, Hive Router, Cosmo Router, and Hot Chocolate Fusion configurations are semantically equivalent where supported.
- [ ] Every measured response is checked for correctness and setup/readiness traffic is excluded.
- [ ] Profiles record throughput, latency, CPU, allocation, GC, and memory for the real Quick path.
- [ ] The dominant actionable seam is optimized behind its existing interface; specialized plans or response storage are introduced only if this evidence selects them.
- [ ] Semantic, audit, lifecycle, planner-quality, and structured-versus-encoded tests remain green after optimization.
- [ ] Useful throughput reaches the standing target or an evidence-backed expiring maintainer exception records the remaining gap.


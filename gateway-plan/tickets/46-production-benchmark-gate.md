# 46 — Build the production benchmark gate

**What to build:** Integrate the latest reviewed GraphQL Gateways Benchmark revision and define a reproducible semantically validated gate that exercises the actual Quick encoded path against current competitors.

**Blocked by:** 34 — Close compatibility and confirm publication matrix; 42 — Close operational race and interruption testing; 45 — Pass encoded gateway responses through Quick

**Status:** ready-for-agent

- [ ] The benchmark revision is pinned in the same committed external-version configuration and consumed by the workflow wiring established for the audit.
- [ ] Each run manifest records resolved upstream revisions, competitor versions or images, and effective gate configuration.
- [ ] Measured responses are semantically checked and incorrect or unexpected rejections do not count toward useful throughput.
- [ ] The gate profile removes known cross-request comparison artifacts while preserving within-operation batching, coalescing, and plan caching.
- [ ] CPU, memory, logging, readiness, workload, and measurement boundaries are normalized and unsupported competitors are recorded.
- [ ] The unmodified upstream-default run remains informational and distinct from the release gate.


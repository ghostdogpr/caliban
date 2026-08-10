# 07 — Execute and project a flat root query

**What to build:** Run the verified single-node DAG through a request coordinator, integrate flat source values into indexed request-owned storage, and project a structured GraphQL response using the first sink of a sink-parameterized prepared-selection writer.

**Blocked by:** 05 — Plan and explain one remote root call; 06 — Perform one bounded and classified remote GraphQL call

**Status:** ready-for-agent

- [ ] A request coordinator schedules the ready SourceCall and source fibers never mutate shared response state.
- [ ] Flat scalar and nullable root fields integrate into indexed storage owned only by the coordinator.
- [ ] The prepared-selection writer targets an explicit structured-response sink rather than embedding ResponseValue merging into the store.
- [ ] Client field order is deterministic regardless of source completion timing.
- [ ] Request-owned state is released only after the returned structured response can no longer reference it.


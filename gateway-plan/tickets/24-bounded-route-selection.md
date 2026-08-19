# 24 — Improve ambiguous-route planning

**Outcome:** When several valid routes can satisfy an operation, the planner selects a deterministic, reasonably efficient dependency DAG without unbounded search.

**Blocked by:** 16 — Preserve mutation ordering

**Status:** ready-for-agent

## Completion criteria

- [ ] Focused scenarios cover alternative field owners, multiple usable keys, bridge sources, requirement paths, and ordinary GraphQL lookups.
- [ ] Plan quality is compared using deterministic structural costs such as downstream call count, dependency depth, and unnecessary internal selections rather than assumed network latency.
- [ ] Ambiguous transitions use bounded candidate selection instead of committing to the first viable route when that produces a worse complete plan.
- [ ] Candidate count, expanded planning work, and planning duration have finite guardrails with safe deterministic failure when exhausted.
- [ ] Equivalent candidates use stable tie-breaking, and unambiguous operations retain a small direct planning path.
- [ ] `GatewayRuntime.explain` and execution continue to consume the same selected `OperationPlan` dependency DAG.
- [ ] Regression tests assert both client-visible correctness and route quality without coupling unrelated tests to private planner representation.
- [ ] The existing plan representation, execution scheduler, plan cache, and response store are not replaced by a second lowering pipeline.

# 27 — Improve ambiguous-route planning

**Outcome:** When several valid routes can satisfy an operation, the planner selects a deterministic, reasonably efficient dependency DAG without unbounded search.

**Blocked by:** 16 — Preserve mutation ordering

**Status:** complete

## Completion criteria

- [x] Focused scenarios cover alternative field owners, multiple usable keys, bridge sources, requirement paths, and ordinary GraphQL lookups.
- [x] Plan quality is compared using deterministic structural costs such as downstream call count, dependency depth, and unnecessary internal selections rather than assumed network latency.
- [x] Ambiguous transitions use bounded candidate selection instead of committing to the first viable route when that produces a worse complete plan.
- [x] Candidate count, expanded planning work, and planning duration have finite guardrails with safe deterministic failure when exhausted.
- [x] Equivalent candidates use stable tie-breaking, and unambiguous operations retain a small direct planning path.
- [x] `GatewayRuntime.explain` and execution continue to consume the same selected `OperationPlan` dependency DAG.
- [x] Regression tests assert both client-visible correctness and route quality without coupling unrelated tests to private planner representation.
- [x] The existing plan representation, execution scheduler, plan cache, and response store are not replaced by a second lowering pipeline.

## Implementation note

The planner carries bounded alternatives in its existing planned-field and planned-root values until a complete operation is available. It filters invalid dependency DAGs, then compares the remaining plans by logical downstream calls, dependency depth, and injected internal selections with stable input ordering as the final tie-break. Logical entity-call cost uses the executor's batching and dependency-wave rules, while mutation roots are costed independently in client order.

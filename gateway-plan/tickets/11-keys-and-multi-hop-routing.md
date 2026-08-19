# 11 — Support key and multi-hop routing breadth

**Outcome:** Entity routing supports the key shapes and dependency chains required by the corresponding audit cases.

**Blocked by:** 10 — Integrate the Federation Gateway Audit

**Status:** completed

## Completion criteria

- [x] Multiple and compound keys, resolvable flags, interface keys, and valid Federation aliases are supported.
- [x] The router selects a satisfiable key from currently available fields and may use multiple source hops when required.
- [x] Independent ready routes execute concurrently; dependent routes wait only for their declared dependencies, with deterministic response and error ordering.
- [x] Internal selections remain hidden and entity correlation remains stable across hops.
- [x] Unsatisfied obligations and dependency cycles fail with deterministic diagnostics rather than runtime guessing.
- [x] Relevant audit groups pass, plus focused tests for duplicate values, null keys, and competing keys.
- [x] The pinned audit is rerun without regressions or new deferrals.
- [x] Route selection deepens the existing plan; it does not introduce a parallel reference plan or lowered execution graph.

## Implementation notes

- Federation key metadata now retains every resolvable key, including compound and nested field sets on objects and
  interfaces. External declarations remain available for internal key acquisition without becoming field owners.
- `OperationPlan` carries explicit route dependencies. The runtime executes ready entity groups concurrently, applies
  their patches between dependency waves, and skips dependent locations whose intermediate lookup returned null.
- The pinned audit now reports 44 passing cases out of 199. `keys-mashup` remains visible under Ticket 12 because its
  final return hop depends on `@requires`; `interface-object-indirect-extension` remains under Ticket 15 because it
  requires `@interfaceObject`, rather than ordinary interface-key routing.

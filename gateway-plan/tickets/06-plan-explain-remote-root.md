# 06 — Plan and explain one remote root call

**What to build:** Turn one prepared remote root selection into a complete rich planner representation, verify it, lower it to a dense single-node execution DAG, and expose a deterministic semantic explanation.

**Blocked by:** 05 — Prepare and check client operations

**Status:** ready-for-agent

- [ ] The plan fixes the source, downstream operation, inputs, merge destination, projection coverage, and dependencies before execution.
- [ ] The verifier rejects invalid reachability, input availability, projection, merge-path, or internal-selection states.
- [ ] The lowered DAG uses compact typed identifiers and contains no arbitrary execution closures.
- [ ] PlanExplanation is deterministic and exposes semantic source/dependency/cost information without executable internals.
- [ ] A golden explanation test proves the ticket delivers observable behavior despite not executing a source.


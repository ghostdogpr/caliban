# 31 — Prove response-store correctness

**What to build:** Build a small reference planner/executor and property-based scenarios that validate the indexed response store, completion, error mapping, and source-document ownership independently of execution order.

**Blocked by:** 10 — Complete nulls and integrate GraphQL errors; 13 — Execute entity batching and correlation; 27 — Support recursive requirements and provides; 29 — Plan and execute abstract selections; 30 — Support operation conditions and selection details

**Status:** ready-for-agent

- [ ] Generated nested object/list/nullability shapes agree with the reference completion result.
- [ ] Aliases, duplicates, entity fan-out, partial failures, and abstract runtime types preserve correct paths and values.
- [ ] Varying ready-work and source completion order never changes projected data or deterministic error order.
- [ ] Source raw references never outlive their leases and caller-owned output remains valid afterward.
- [ ] Conflicting writers or unjustified internal selections are rejected by verification.


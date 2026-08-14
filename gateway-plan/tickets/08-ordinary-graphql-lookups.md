# 08 — Add explicit ordinary GraphQL lookups

**Outcome:** Ordinary GraphQL and local subgraphs can declare entity lookups without Federation transport fields.

**Blocked by:** 06 — Complete cross-source data and errors

**Status:** ready-for-agent

## Completion criteria

- [ ] Code-first metadata declares the target type, key fields, source field, and argument mapping for a lookup.
- [ ] Single and list-capable lookups reuse the existing entity-transition and correlation behavior.
- [ ] Ordered correlation and key-based correlation have explicit validation and runtime semantics.
- [ ] Compound argument objects are expressible without arbitrary user execution code in the planner.
- [ ] Invalid mappings and lookup result shapes produce source-attributed build diagnostics.
- [ ] An ordinary-only cross-source graph passes an end-to-end nested query and failure test.

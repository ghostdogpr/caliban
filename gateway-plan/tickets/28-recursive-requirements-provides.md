# 28 — Support recursive requirements and provides

**What to build:** Satisfy recursively nested key and field requirements across Federation and ordinary sources, including Federation @requires and @provides semantics.

**Blocked by:** 23 — Apply structural subgraph transformations; 27 — Support compound keys and multi-hop routing

**Status:** ready-for-agent

- [ ] Requirements are recursively expanded into planner obligations with cycle detection.
- [ ] Federation @requires compiles into the same source-neutral normalized obligation model used by keys and ordinary required-argument mappings.
- [ ] Existing structural transforms are extended to rewrite Federation @requires, @provides, and their field-set coordinates symmetrically.
- [ ] @provides affects field availability only where its path and runtime type make it valid.
- [ ] Internal requirement selections remain hidden from client validation, introspection, and projection.
- [ ] Unsatisfied or contradictory requirements produce deterministic composition or planning diagnostics.
- [ ] Reference-oracle scenarios cover nested requirements, provided fields, and varied source completion order.

# 27 — Support recursive requirements and provides

**What to build:** Satisfy recursively nested key and field requirements across Federation and ordinary sources, including Federation @requires and @provides semantics.

**Blocked by:** 26 — Support compound keys and multi-hop routing

**Status:** ready-for-agent

- [ ] Requirements are recursively expanded into planner obligations with cycle detection.
- [ ] Federation @requires and ordinary required arguments share the appropriate normalized obligation model.
- [ ] @provides affects field availability only where its path and runtime type make it valid.
- [ ] Internal requirement selections remain hidden from client validation, introspection, and projection.
- [ ] Unsatisfied or contradictory requirements produce deterministic composition or planning diagnostics.


# 18 — Compose and execute ordinary lookups

**What to build:** Extend the source-neutral capability model so ordinary remote GraphQL roots and explicit single lookups participate in cross-source entity execution without Federation directives.

**Blocked by:** 12 — Execute entity batching and correlation

**Status:** ready-for-agent

- [ ] Ordinary root fields execute through the same planner, transport, coordinator, and response store as Federation roots.
- [ ] Pinned SDL or parsed schema input is sufficient; remote schema acquisition is an independent later capability.
- [ ] Explicit entity, key, lookup field, and argument mappings are parsed and validated at build.
- [ ] Single lookups support zero or one entity, alias packing, nulls, stable deduplication, and fan-out.
- [ ] Matching type or field names never infer a join.
- [ ] Invalid coordinates, paths, arguments, result shapes, or cycles accumulate deterministic source-attributed diagnostics.

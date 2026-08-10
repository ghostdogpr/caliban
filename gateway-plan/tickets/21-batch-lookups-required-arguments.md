# 21 — Execute batch lookups and required arguments

**What to build:** Support native list-shaped lookups and declarative parent-to-argument mappings, including compound input objects and requirements removed from the client-visible signature.

**Blocked by:** 17 — Compose and execute ordinary lookups

**Status:** ready-for-agent

- [ ] Ordered correlation requires exactly one result position, possibly null, per deduplicated input.
- [ ] Key correlation accepts reordering and omission but rejects duplicate, missing, or unexpected returned keys.
- [ ] Argument mappings support parent leaf paths and nested input objects without an arbitrary expression language.
- [ ] Mappings validate input names, nullability, defaults, list lifting, path traversal, and result compatibility at build.
- [ ] Requirements become planner obligations and their internal data does not appear in client projection.


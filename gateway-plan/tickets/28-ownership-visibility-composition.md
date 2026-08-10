# 28 — Complete ownership and visibility composition

**What to build:** Implement the remaining ownership, visibility, and schema compatibility semantics needed for broad Federation composition and correct client-facing schema behavior.

**Blocked by:** 18 — Support shareable providers; 22 — Apply structural subgraph transformations; 25 — Execute the canonical mixed graph

**Status:** ready-for-agent

- [ ] @override transfers ownership according to supported Federation semantics.
- [ ] @inaccessible schema members may remain available for routing while being absent from client validation and introspection.
- [ ] Compatible shared roots compose without implying runtime failover.
- [ ] Input objects and enums intersect according to the selected composition rules.
- [ ] Visibility and ownership diagnostics preserve source attribution and stable ordering.
- [ ] Disabling client introspection rejects __schema and __type without affecting acquisition or internal __typename.


# 29 — Complete ownership and visibility composition

**What to build:** Implement the remaining ownership, visibility, and schema compatibility semantics needed for broad Federation composition and correct client-facing schema behavior.

**Blocked by:** 10 — Execute client introspection locally; 19 — Support shareable providers

**Status:** ready-for-agent

- [ ] @override transfers ownership according to supported Federation semantics.
- [ ] @inaccessible schema members may remain available for routing while being absent from client validation and introspection.
- [ ] Compatible shared roots compose without implying runtime failover.
- [ ] Input objects and enums intersect according to the selected composition rules.
- [ ] Visibility and ownership diagnostics preserve source attribution and stable ordering.
- [ ] Disabling client introspection rejects __schema and __type without affecting acquisition or internal __typename.

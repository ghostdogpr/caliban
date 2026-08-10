# 13 — Close the structured Federation MVP

**What to build:** Demonstrate the complete embedded Products-to-Reviews structured-response path and establish the request deadline, cancellation, trace-context, and ownership protocol that later operational tickets extend rather than rewrite.

**Blocked by:** 10 — Execute client introspection locally; 12 — Execute entity batching and correlation

**Status:** ready-for-agent

- [ ] The absolute deadline is created at public request entry, before the insertion point reserved for later admission and queue wait.
- [ ] Deadline victory atomically disables result delivery, interrupts the request tree, and returns a prebuilt bounded timeout response only after owned work exits.
- [ ] Caller interruption remains interruption and never fabricates a GraphQL or source-failure response.
- [ ] Interruption at transport, source-result handoff, integration, and projection releases ownership exactly once.
- [ ] Active ZIO and FiberRef trace context survives frontend, planning, source fibers, coordinator handoff, and projection.
- [ ] The canonical query can return Products data plus a safe Reviews merge-boundary error when the Reviews transport fails.


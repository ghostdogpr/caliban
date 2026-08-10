# 01 — Create the fresh gateway foundation

**What to build:** Create the unpublished embedded gateway module and the smallest compiling `Gateway`/`GatewayRuntime` scaffolding needed to establish its module and public/private boundaries, without preserving compatibility with the abandoned prototype or anticipating APIs owned by later tickets.

**Blocked by:** None — can start immediately

**Status:** ready-for-agent

- [ ] The gateway module compiles on the provisional supported JVM Scala matrix and publication remains disabled.
- [ ] The only new public scaffolding is the minimal contravariant `Gateway` description and `GatewayRuntime` shape needed by subsequent tickets; constructors and representations that must remain private are private.
- [ ] No source, lookup, batching, header-policy, transport, transformation, planner, response-store, or broad configuration type is introduced before the ticket that first gives it executable semantics.
- [ ] Gateway internals remain private and dependencies point from gateway to existing Caliban modules, never from core to gateway.
- [ ] No HTTP client, Quick, tracing, or other dependency needed only by a later ticket is added to the foundation module.

# 20 — Execute local Caliban graphs

**What to build:** Add in-process Caliban as a built-in execution source using the common capability, planner, prepared-call, coordinator, and response-store seams rather than a local-only shortcut.

**Blocked by:** 05 — Prepare and check client operations; 09 — Store and project nested objects and lists; 17 — Compose and execute ordinary lookups

**Status:** ready-for-agent

- [ ] Local-only graphs compose and execute through GatewayRuntime.
- [ ] GraphQL[R] environment requirements remain contravariant and are supplied at request execution, never captured by build.
- [ ] Local results import structurally into the indexed response store without a JSON round trip.
- [ ] Caliban wrappers, masking, tracing, and execution behavior remain intact.
- [ ] Local source limits and permits are distinct from remote transport configuration.


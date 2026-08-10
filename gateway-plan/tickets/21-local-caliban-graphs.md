# 21 — Execute local Caliban graphs

**What to build:** Add in-process Caliban as a built-in execution source using the common capability, planner, prepared-call, coordinator, and response-store seams rather than a local-only shortcut.

**Blocked by:** 12 — Execute entity batching and correlation

**Status:** ready-for-agent

- [ ] Local-only graphs compose and execute through GatewayRuntime.
- [ ] Local root execution does not require ordinary lookup or remote-acquisition support.
- [ ] Local subgraphs may expose the same explicit single-lookup capability as ordinary remote subgraphs, executed in process through the common entity-transition path.
- [ ] GraphQL[R] environment requirements remain contravariant and are supplied at request execution, never captured by build.
- [ ] Local results import structurally into the indexed response store without a JSON round trip.
- [ ] Caliban wrappers, masking, tracing, and execution behavior remain intact.
- [ ] Local source limits and permits are distinct from remote transport configuration.

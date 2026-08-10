# 26 — Execute a fixed-route mixed graph

**What to build:** Execute remote Products, local Pricing, and remote Reviews through one composition model, planner, scheduler, coordinator, and response projection using only already-supported fixed routes.

**Blocked by:** 18 — Compose and execute ordinary lookups; 21 — Execute local Caliban graphs

**Status:** ready-for-agent

- [ ] Ordinary, Federation, local-only, and mixed graphs use the same GatewayRuntime and planner.
- [ ] The representative request executes Products first, then eligible Pricing and Reviews work concurrently.
- [ ] The plan uses unique fixed providers and does not depend on shareability, route search, transformations, header policy, or public execution configuration.
- [ ] Local and remote partial errors share the same completion and projection semantics.
- [ ] Local execution performs no JSON round trip.
- [ ] The mixed path preserves deterministic projection and explanation under varied local and remote completion order.

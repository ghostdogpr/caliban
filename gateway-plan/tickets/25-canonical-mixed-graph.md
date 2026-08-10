# 25 — Execute the canonical mixed graph

**What to build:** Execute remote Products, local Pricing, and conditional remote Reviews through one composition model, planner, scheduler, coordinator, and response projection.

**Blocked by:** 19 — Add bounded route choice and call coalescing; 20 — Execute local Caliban graphs; 21 — Execute batch lookups and required arguments; 22 — Apply structural subgraph transformations; 23 — Apply runtime header policy; 24 — Configure source execution behavior

**Status:** ready-for-agent

- [ ] Ordinary, Federation, local-only, and mixed graphs use the same GatewayRuntime and planner.
- [ ] The canonical request executes Products first, then eligible Pricing and Reviews work concurrently under distinct permits.
- [ ] Variable conditions can suppress Reviews without runtime route discovery.
- [ ] Local and remote partial errors share the same completion and projection semantics.
- [ ] Environment intersections across the currently installed local and header-policy requirements compile and run.
- [ ] Local execution performs no JSON round trip.


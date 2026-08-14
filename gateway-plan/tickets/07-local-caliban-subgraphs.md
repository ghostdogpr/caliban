# 07 — Add local Caliban subgraphs

**Outcome:** An in-process Caliban graph participates in the same composed graph and entity-routing behavior as remote sources.

**Blocked by:** 06 — Complete cross-source data and errors

**Status:** ready-for-agent

## Completion criteria

- [ ] A public `Subgraph` constructor accepts a Caliban graph and derives its schema without serialization.
- [ ] Local root fields and local entity lookups use the same composition and plan semantics as remote GraphQL.
- [ ] Local values enter completion structurally with no JSON round trip.
- [ ] Required environments from local graphs accumulate in the contravariant gateway/runtime type and compile on supported Scala versions.
- [ ] ZIO context, interruption, and failures preserve ordinary Caliban behavior.
- [ ] A remote Products, local Pricing, remote Reviews test executes one mixed query through `GatewayRuntime`.
- [ ] The execution-source seam is introduced or finalized here because it now has two concrete implementations.

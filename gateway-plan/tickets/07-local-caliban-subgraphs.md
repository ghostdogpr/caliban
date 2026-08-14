# 07 — Add local Caliban subgraphs

**Outcome:** An in-process Caliban graph participates in the same composed graph and entity-routing behavior as remote sources.

**Blocked by:** 06 — Complete cross-source data and errors

**Status:** completed

## Completion criteria

- [x] A public `Subgraph` constructor accepts a Caliban graph and derives its schema without serialization.
- [x] Local root fields and local entity lookups use the same composition and plan semantics as remote GraphQL.
- [x] Local values enter completion structurally with no JSON round trip.
- [x] Required environments from local graphs accumulate in the contravariant gateway/runtime type and compile on supported Scala versions.
- [x] ZIO context, interruption, and failures preserve ordinary Caliban behavior.
- [x] A remote Products, local Pricing, remote Reviews test executes one mixed query through `GatewayRuntime`.
- [x] The execution-source seam is introduced or finalized here because it now has two concrete implementations.

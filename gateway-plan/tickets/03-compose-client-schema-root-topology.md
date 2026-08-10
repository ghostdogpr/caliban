# 03 — Compose the client schema and root topology

**What to build:** Merge normalized subgraph contributions into one client-visible schema and immutable root routing topology, yielding a complete scoped graph generation or no runtime.

**Blocked by:** 02 — Normalize pinned subgraph schemas

**Status:** ready-for-agent

- [ ] Compatible unique roots and compatible type contributions merge deterministically.
- [ ] Incompatible definitions and unsupported duplicate providers produce accumulated deterministic diagnostics.
- [ ] Source names and remote endpoints are validated according to the public contract.
- [ ] Successful build exposes the composed schema, compact rendering, topology, and warnings.
- [ ] Failed composition never returns a partial graph or runtime.

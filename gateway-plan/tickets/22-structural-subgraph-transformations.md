# 22 — Apply structural subgraph transformations

**What to build:** Apply the closed supported rename and exclusion transforms symmetrically to schema composition, capability coordinates, and downstream or local execution translations.

**Blocked by:** 17 — Compose and execute ordinary lookups; 18 — Support shareable providers

**Status:** ready-for-agent

- [ ] Transforms are scoped to one subgraph and authored coordinates consistently refer to the transformed gateway-visible schema.
- [ ] Schema, keys, lookups, shareability, requirements, field sets, and correlation coordinates transform together.
- [ ] The compiled inverse mapping produces correct downstream operations and local input translation.
- [ ] A transform cannot add sources, inject resolvers, depend on R, inspect requests, change ownership, or hide required identity metadata.
- [ ] Diagnostics retain original coordinates and reject every non-total rename or exclusion.


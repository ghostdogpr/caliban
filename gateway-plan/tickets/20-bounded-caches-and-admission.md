# 20 — Add bounded caches, admission, and concurrency

**Outcome:** A shared runtime remains bounded under repeated operations and concurrent load.

**Blocked by:** 07 — Add local Caliban subgraphs; 08 — Add explicit ordinary GraphQL lookups; 18 — Add source execution policy; 19 — Add operation resolution and policy hooks

**Status:** complete

## Completion criteria

- [x] Schema-generation-owned operation/plan caching is weighted, finite, and single-flight for stable keys.
- [x] Unstable operation resolution or policy inputs bypass caches rather than poisoning keys.
- [x] Client operation text, parser nesting, and parsed-node limits are finite and enforced before expensive planning or cache insertion.
- [x] Request admission and per-source permits are finite, interruptible, and reported through runtime status.
- [x] A source-call permit covers one logical call across its retry attempts.
- [x] Request-local entity deduplication remains distinct from cross-request caching.
- [x] Tests cover cache stampedes, eviction, interrupted waiters, permit release, and concurrent local/remote work.
- [x] Cache and admission interfaces remain private unless applications need a concrete configuration or status value.

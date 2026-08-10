# 32 — Execute ordered mutations

**What to build:** Preserve GraphQL top-level mutation order across routed sources while retaining safe concurrency inside each current mutation subtree.

**Blocked by:** 19 — Add bounded route choice and call coalescing; 25 — Execute the canonical mixed graph; 31 — Prove response-store correctness

**Status:** ready-for-agent

- [ ] The complete routed subtree of each top-level mutation field finishes before the next begins.
- [ ] Nested independent work inside the current mutation root may execute concurrently.
- [ ] Planner dependencies and verifier checks make mutation fences explicit.
- [ ] Source-call coalescing never crosses a mutation fence.
- [ ] Mutation result fields and errors remain in deterministic client order.


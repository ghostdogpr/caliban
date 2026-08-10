# 32 — Execute ordered mutations

**What to build:** Preserve GraphQL top-level mutation order across routed sources while retaining safe concurrency inside each current mutation subtree.

**Blocked by:** 14 — Prove core response-store correctness; 20 — Add bounded route choice and call coalescing

**Status:** ready-for-agent

- [ ] The complete routed subtree of each top-level mutation field finishes before the next begins.
- [ ] Nested independent work inside the current mutation root may execute concurrently.
- [ ] Planner dependencies and verifier checks make mutation fences explicit.
- [ ] Query-only coalescing remains unchanged, and any mutation-specific coalescing introduced here never crosses a mutation fence.
- [ ] Mutation result fields and errors remain in deterministic client order.

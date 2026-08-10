# 20 — Add bounded route choice and call coalescing

**What to build:** Expand the planner from fixed routes to deterministic bounded candidate search and safe source-call coalescing once shareable providers create genuine alternatives.

**Blocked by:** 12 — Execute entity batching and correlation; 19 — Support shareable providers

**Status:** ready-for-agent

- [ ] The versioned cost heuristic prefers fewer sequential stages, fewer calls, better batching, less duplicate work, less unnecessary data, then stable topology identity.
- [ ] Search stays within configured state and plan-node limits.
- [ ] A truncated search with a feasible plan returns the best complete plan and records searchTruncated.
- [ ] A limit reached before any feasible plan produces a distinct diagnostic and never caches or executes a partial plan.
- [ ] Coalescing requires the same source and query operation type, completed dependency frontier, compatible conditions and batching, and recoverable mappings; mutations are conservatively excluded until Ticket 32 owns explicit fences.
- [ ] Redundant transitions and internal selections are removed only when verifier invariants remain satisfied.

# 27 — Support compound keys and multi-hop routing

**What to build:** Plan and execute compound or multiple entity keys and stable multi-hop transitions across more than two sources while rejecting unsatisfiable or cyclic routes.

**Blocked by:** 12 — Execute entity batching and correlation; 14 — Prove core response-store correctness; 20 — Add bounded route choice and call coalescing

**Status:** ready-for-agent

- [ ] Compound and multiple key alternatives normalize into explicit topology capabilities.
- [ ] The planner may choose complete multi-hop routes using the bounded cost model.
- [ ] Intermediate entity batches retain stable deduplication and fan-out.
- [ ] Recursive route exploration rejects cycles and missing key material deterministically.
- [ ] Plan explanations expose every transition and dependency without executable internals.
- [ ] Reference-oracle scenarios cover multi-hop fan-out, partial failure, and varied source completion order.

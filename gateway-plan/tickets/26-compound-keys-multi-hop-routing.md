# 26 — Support compound keys and multi-hop routing

**What to build:** Plan and execute compound or multiple entity keys and stable multi-hop transitions across more than two sources while rejecting unsatisfiable or cyclic routes.

**Blocked by:** 25 — Execute the canonical mixed graph

**Status:** ready-for-agent

- [ ] Compound and multiple key alternatives normalize into explicit topology capabilities.
- [ ] The planner may choose complete multi-hop routes using the bounded cost model.
- [ ] Intermediate entity batches retain stable deduplication and fan-out.
- [ ] Recursive route exploration rejects cycles and missing key material deterministically.
- [ ] Plan explanations expose every transition and dependency without executable internals.


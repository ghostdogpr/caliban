# 52 — Write examples and migration documentation

**What to build:** Document the embedded gateway as a Caliban library and provide compiling examples for every supported graph shape plus the reviewed changes to existing Caliban and Quick behavior.

**Blocked by:** 17 — Acquire ordinary and Federation schemas remotely; 22 — Execute batch lookups and required arguments; 23 — Apply structural subgraph transformations; 26 — Execute a fixed-route mixed graph; 34 — Close compatibility and confirm publication matrix; 42 — Close operational race and interruption testing; 46 — Pass encoded gateway responses through Quick; 50 — Verify the useful-throughput gate

**Status:** ready-for-agent

- [ ] Ordinary remote, Federation, local-only, and mixed graph examples compile on the supported example/test matrix.
- [ ] Documentation explains code-first composition, remote acquisition, scoped build, execution environments, single/batch lookup metadata, structural transforms, headers, policy, lifecycle, and plan explanation.
- [ ] Quick documentation covers finite body limits, 413, mutation-over-GET 405, encoded capability retention, and structured fallback.
- [ ] Core documentation calls out the ResponseError sealed-hierarchy compatibility impact and encoded-interpreter capability.
- [ ] Lifecycle documentation honestly describes uninterruptible user effects, overdue status, drain, and scope-close behavior.
- [ ] Audit, benchmark, version refresh, performance exception, and deferred-scope processes are documented.

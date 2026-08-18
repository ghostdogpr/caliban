# 11 — Support key and multi-hop routing breadth

**Outcome:** Entity routing supports the key shapes and dependency chains required by the corresponding audit cases.

**Blocked by:** 10 — Integrate the Federation Gateway Audit

**Status:** ready-for-agent

## Completion criteria

- [ ] Multiple and compound keys, resolvable flags, interface keys, and valid Federation aliases are supported.
- [ ] The router selects a satisfiable key from currently available fields and may use multiple source hops when required.
- [ ] Independent ready routes execute concurrently; dependent routes wait only for their declared dependencies, with deterministic response and error ordering.
- [ ] Internal selections remain hidden and entity correlation remains stable across hops.
- [ ] Unsatisfied obligations and dependency cycles fail with deterministic diagnostics rather than runtime guessing.
- [ ] Relevant audit groups pass, plus focused tests for duplicate values, null keys, and competing keys.
- [ ] The pinned audit is rerun: every newly passing case is added to `supported-cases.txt`, ownership remains accurate for failures, and a suite is marked supported once none of its cases fail. No new deferrals are introduced.
- [ ] Route selection deepens the existing plan; it does not introduce a parallel reference plan or lowered execution graph.

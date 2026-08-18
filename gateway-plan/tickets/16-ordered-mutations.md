# 16 — Preserve mutation ordering

**Outcome:** Top-level mutation fields execute serially in client order even when routed to different sources.

**Blocked by:** 10 — Integrate the Federation Gateway Audit

**Status:** ready-for-agent

## Completion criteria

- [ ] Each top-level mutation field and all of its dependent routes complete before the next top-level mutation begins.
- [ ] Independent nested work within the active mutation field may run concurrently.
- [ ] Query concurrency remains unchanged.
- [ ] Failure and null completion in one mutation field follow GraphQL semantics without reordering later fields.
- [ ] Coalescing or batching never crosses a top-level mutation fence.
- [ ] Relevant audit cases and an explicit cross-source ordering test pass.
- [ ] The pinned audit is rerun: every newly passing case is removed from `expectations.tsv`, ownership remains accurate for failures, and no new deferrals are introduced.

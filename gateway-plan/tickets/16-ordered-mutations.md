# 16 — Preserve mutation ordering

**Outcome:** Top-level mutation fields execute serially in client order even when routed to different sources.

**Blocked by:** 10 — Integrate the Federation Gateway Audit

**Status:** complete

## Completion criteria

- [x] Each top-level mutation field and all of its dependent routes complete before the next top-level mutation begins.
- [x] Independent nested work within the active mutation field may run concurrently.
- [x] Query concurrency remains unchanged.
- [x] Failure and null completion in one mutation field follow GraphQL semantics without reordering later fields.
- [x] Coalescing or batching never crosses a top-level mutation fence.
- [x] Relevant audit cases and an explicit cross-source ordering test pass.
- [x] The pinned audit is rerun with all 199 cases passing and no exceptions.

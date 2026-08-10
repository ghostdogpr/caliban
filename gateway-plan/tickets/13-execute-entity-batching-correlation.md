# 13 — Execute entity batching and correlation

**What to build:** Execute the planned Federation entity transition with stable deduplication, one prepared entity batch, correlation back to every client location, and integration through the common response store.

**Blocked by:** 07 — Perform one bounded and classified remote GraphQL call; 10 — Complete nulls and integrate GraphQL errors; 12 — Plan one Federation entity transition

**Status:** ready-for-agent

- [ ] Duplicate representations are deduplicated before the source call and fan out afterward.
- [ ] One prepared _entities call carries the stable batch under one logical source-call ownership boundary.
- [ ] Null entities, partial data, GraphQL errors, and classified SourceFailure values integrate at the correct client locations.
- [ ] Internal keys and representations never appear in final projection.
- [ ] Correlation and error order remain deterministic under varied source completion timing.


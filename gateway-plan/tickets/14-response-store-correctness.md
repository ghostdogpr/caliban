# 14 — Prove core response-store correctness

**What to build:** Build a small reference completion/execution oracle and property-based scenarios for the nested and entity semantics already supported, establishing a reusable correctness harness that later breadth tickets extend.

**Blocked by:** 09 — Complete nulls and integrate GraphQL errors; 12 — Execute entity batching and correlation

**Status:** ready-for-agent

- [ ] Generated nested object/list/nullability shapes agree with the reference completion result.
- [ ] Repeated selections, duplicates, entity fan-out, and partial failures preserve correct paths and values.
- [ ] Varying ready-work and source completion order never changes projected data or deterministic error order.
- [ ] Any source-document references used by the current implementation never outlive their ownership, and returned structured output remains valid afterward.
- [ ] The oracle and generators expose extension points used by later requirements, abstract-selection, condition, and mutation tickets rather than predicting those semantics here.
- [ ] Conflicting writers or unjustified internal selections are rejected by verification.

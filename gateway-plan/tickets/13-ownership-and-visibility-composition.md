# 13 — Complete ownership and visibility composition

**Outcome:** The client schema and routing metadata agree on which compatible fields and types are visible and executable.

**Blocked by:** 10 — Integrate the Federation Gateway Audit

**Status:** completed

## Completion criteria

- [x] `@shareable`, `@external`, `@inaccessible`, and supported `@override` semantics are applied consistently to schema and routing.
- [x] Every Federation provider sharing a field declares compatible shareability as required by the supported Federation semantics.
- [x] Multiple valid providers use deterministic route selection and may coalesce identical work within one operation without promising failover.
- [x] Compatible and incompatible object, interface, union, input-object, enum, scalar, and directive contributions follow the audit-driven composition rules.
- [x] Federation directive imports, aliases, and namespaced forms resolve without leaking transport metadata.
- [x] Invalid ownership or visibility produces accumulated source-attributed diagnostics.
- [x] Relevant audit groups pass without introducing a second general-purpose schema model solely for validation.
- [x] The pinned audit is rerun: every newly passing case is removed from `expectations.tsv`, ownership remains accurate for failures, and no new deferrals are introduced.

The pinned audit now passes 107 of 199 cases. Remaining abstract runtime-routing failures are owned by Ticket 15.

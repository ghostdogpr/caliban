# 13 — Complete ownership and visibility composition

**Outcome:** The client schema and routing metadata agree on which compatible fields and types are visible and executable.

**Blocked by:** 10 — Integrate the Federation Gateway Audit

**Status:** ready-for-agent

## Completion criteria

- [ ] `@shareable`, `@external`, `@inaccessible`, and supported `@override` semantics are applied consistently to schema and routing.
- [ ] Every Federation provider sharing a field declares compatible shareability as required by the supported Federation semantics.
- [ ] Multiple valid providers use deterministic route selection and may coalesce identical work within one operation without promising failover.
- [ ] Compatible and incompatible object, interface, union, input-object, enum, scalar, and directive contributions follow the audit-driven composition rules.
- [ ] Federation directive imports, aliases, and namespaced forms resolve without leaking transport metadata.
- [ ] Invalid ownership or visibility produces accumulated source-attributed diagnostics.
- [ ] Relevant audit groups pass without introducing a second general-purpose schema model solely for validation.
- [ ] The pinned audit is rerun: every newly passing case is removed from `expectations.tsv`, ownership remains accurate for failures, and no new deferrals are introduced.

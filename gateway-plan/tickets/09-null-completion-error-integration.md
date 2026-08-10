# 09 — Complete nulls and integrate GraphQL errors

**What to build:** Compile and execute GraphQL null completion and error integration over the indexed store, including a defined response destination for classified SourceFailure values.

**Blocked by:** 08 — Store and project nested objects and lists

**Status:** ready-for-agent

- [ ] Non-null violations propagate idempotently to the nearest nullable boundary and mark newly unreachable descendants.
- [ ] Source GraphQL errors preserve safe messages, allowed extensions, and rewritten client paths in deterministic order.
- [ ] A classified SourceFailure attaches at its planned merge boundary with a safe gateway-authored message while independent work continues.
- [ ] Missing or unusable source error paths attach at the source call merge boundary.
- [ ] Null and error behavior is independent of source completion order.
- [ ] Ticket 38 may refine masking and extension policy but does not replace the SourceFailure integration semantics established here.


# 21 — Close deadlines, drain, and lifecycle races

**Outcome:** Runtime shutdown, request deadlines, and interruption preserve structured ownership and exactly-once accounting.

**Blocked by:** 20 — Add bounded caches, admission, and concurrency

**Status:** complete

## Completion criteria

- [x] One absolute request deadline includes admission wait, routing, source work, completion, and response handoff.
- [x] Deadline expiry disables late delivery, interrupts cooperative work, and returns a bounded safe timeout response only for deadline expiry.
- [x] Caller interruption and scope close remain interruption and fabricate no response.
- [x] Runtime state moves consistently through running, draining, and closed while racing admissions have exactly one outcome.
- [x] Active and overdue work remains accounted for until its structured request tree exits.
- [x] Uninterruptible user code is never detached and its inability to be forcibly terminated is visible in status or metrics.
- [x] Deterministic race tests inject interruption at admission, permit, source, retry, result handoff, completion, and scope close.

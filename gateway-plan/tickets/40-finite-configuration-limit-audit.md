# 40 — Audit finite configuration and limits

**What to build:** Verify that every gateway subsystem introduced its own finite resource protections and close the immutable typed configuration model without retrofitting limits in one place.

**Blocked by:** 34 — Close compatibility and confirm publication matrix; 35 — Add bounded caches and single-flight; 36 — Implement admission, drain, and runtime status; 37 — Track overdue work and narrow deadlines; 38 — Implement retries, source outcomes, and masking; 39 — Add operation resolution and policy; 43 — Add the gateway encoded response sink; 44 — Harden Quick ingress and install header context

**Status:** ready-for-agent

- [ ] Gateway-owned query, parser, variable, schema, source, planner, cache, queue, concurrency, response-memory, final encoded-byte, Quick request-body, and lifecycle bounds are always finite.
- [ ] Optional structural policies remain distinct from mandatory safety bounds.
- [ ] Per-request options only narrow deadlines and resource limits.
- [ ] Explicit source values, inherited/disabled/configured facilities, and strictest-cap resolution behave consistently.
- [ ] Contradictory settings fail build and ineffective settings produce deterministic warnings.
- [ ] No untyped option map or environment-variable reader enters the embedded library.

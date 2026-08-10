# 42 — Close operational race and interruption testing

**What to build:** Exercise the completed structured gateway under systematic suspension, race, interruption, deadline, drain, and ownership-transfer faults to prove exactly-once behavior.

**Blocked by:** 26 — Execute a fixed-route mixed graph; 34 — Close compatibility and confirm publication matrix; 35 — Add bounded caches and single-flight; 36 — Implement admission, drain, and runtime status; 37 — Track overdue work and narrow deadlines; 38 — Implement retries, source outcomes, and masking; 39 — Add operation resolution and policy; 40 — Audit finite configuration and limits; 41 — Add bounded metrics and safe logging

**Status:** ready-for-agent

- [ ] Admission versus drain produces exactly registered active execution or typed draining rejection.
- [ ] Single-flight waiters, source permits, retries, and transport attempts release exactly once under cancellation.
- [ ] SourceResult transfer defines the source-timeout versus request-timeout classification boundary.
- [ ] Uninterruptible local, resolver, policy, and header effects remain owned and overdue until exit, with late delivery disabled.
- [ ] Caller interruption and scope close fabricate no ordinary response.
- [ ] Fault suites detect leaked fibers, buffers, source documents, bodies, permits, cache work, transport resources, or accounting.
- [ ] The canonical remote Products/local Pricing/conditional remote Reviews scenario runs with the completed header, policy, deadline, admission, and projection semantics.

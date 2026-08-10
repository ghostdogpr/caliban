# 41 — Add bounded metrics and safe logging

**What to build:** Expose useful low-cardinality ZIO metrics and narrowly scoped automatic logging without retaining or labeling sensitive request or source data.

**Blocked by:** 40 — Audit finite configuration and limits

**Status:** ready-for-agent

- [ ] Metrics cover request, admission, overdue work, planning, caches, source permits/attempts/bytes/outcomes, integration, projection, output, cancellation, and runtime state.
- [ ] Labels are restricted to bounded operation type, source, outcome/code, and cache-kind vocabularies.
- [ ] Raw query, variables, headers, bodies, responses, upstream messages, operation names, and hashes never become labels.
- [ ] Expected request, GraphQL, source, retry, denial, limit, and diagnostic outcomes are not logged automatically.
- [ ] Only unexpected gateway defects and finalizer/resource-release failures are logged with bounded safe annotations.
- [ ] Dropped telemetry is itself observable without unbounded buffering.


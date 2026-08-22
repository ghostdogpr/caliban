# 30 — Finish tracing, documentation, and release review

**Outcome:** The gateway is observable, documented, cross-built, and ready for a public release decision.

**Blocked by:** 07 — Add local Caliban subgraphs; 08 — Add explicit ordinary GraphQL lookups; 09 — Acquire remote schemas safely;
14 — Add structural schema transformations; 28 — Benchmark, profile, and optimize the real gateway; 29 — Deduplicate identical remote
query calls

**Status:** ready-for-agent

## Completion criteria

- [ ] OpenTelemetry integration adds spans for request, routing, source calls, retries, and completion with W3C propagation and no raw GraphQL data capture by default.
- [ ] Metrics cover requests, source calls, retries, caches, admission, in-flight deduplication, and overdue work with bounded label cardinality; expected errors are not logged automatically.
- [ ] Examples compile for ordinary remote, Federation remote, local Caliban, mixed, pinned-schema, acquired-schema, and Quick deployment paths.
- [ ] Public documentation explains lifecycle, limits, remote-error disclosure, introspection control, security-policy requirements, composed directives, headers, environment intersections, deferred features, and migration expectations.
- [ ] The supported Scala matrix runs the real gateway and examples; Scala 2 remains only if the implementation uses no valuable Scala 3-only feature that justifies narrowing.
- [ ] Public types are minimal, privately constructible where intended, and reviewed for MiMa and naming consistency with Caliban.
- [ ] The selected Federation Gateway Audit revision is refreshed through an explicit reviewed pin change, and every case passes through native composition and execution.
- [ ] Audit evidence records the upstream revision, case results, Scala version, and execution environment.
- [ ] Clean-checkout project, audit, benchmark, lifecycle, tracing, formatting, and documentation checks pass.
- [ ] Publication is enabled only after the maintainer accepts the compatibility and performance evidence.

# 30 — Finish tracing, documentation, and release review

**Outcome:** The gateway is observable, documented, cross-built, and ready for a public release decision.

**Blocked by:** 07 — Add local Caliban subgraphs; 08 — Add explicit ordinary GraphQL lookups; 09 — Acquire remote schemas safely;
14 — Add structural schema transformations; 28 — Benchmark, profile, and optimize the real gateway; 29 — Deduplicate identical remote
query calls

**Status:** complete

## Completion criteria

- [x] OpenTelemetry integration adds spans for request, routing, source calls, retries, and completion with W3C propagation and no raw GraphQL data capture by default.
- [x] Metrics cover requests, source calls, retries, caches, admission, in-flight deduplication, and overdue work with bounded label cardinality; expected errors are not logged automatically.
- [x] Runnable applications demonstrate ordinary remote, Federation remote, local Caliban, mixed, pinned-schema, acquired-schema, and Quick deployment paths.
- [x] Public documentation explains lifecycle, limits, remote-error disclosure, introspection control, security-policy requirements, composed directives, headers, environment intersections, deferred features, and migration expectations.
- [x] Normal project checks cover the supported Scala versions; the compatibility audit uses one Scala version and only checks upstream compatibility.
- [x] Public types are minimal, privately constructible where intended, and reviewed for MiMa and naming consistency with Caliban.
- [x] The selected Federation Gateway Audit revision and canonical repository location are explicitly reviewed, and every case passes through native composition and execution.
- [x] CI runs the pinned upstream audit and fails unless every reported compatibility case passes.
- [x] Clean-checkout project, audit, benchmark, lifecycle, tracing, formatting, and documentation checks pass.
- [x] Publication state follows the maintainer's explicit release decision.

## Completion notes

The existing audit pin was reviewed unchanged after the upstream project moved to its canonical repository and passes 199/199
cases. Gateway and gateway-tracing are enabled for publication by maintainer direction; the broader performance target remains
tracked by ticket 28. Runnable examples remain a non-published project. Metrics and tracing are installed explicitly as composable
`GatewayWrapper` integrations, leaving the empty-wrapper runtime path free of telemetry collection costs.

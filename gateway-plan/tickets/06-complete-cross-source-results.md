# 06 — Complete cross-source data and errors

**Outcome:** Cross-source execution preserves partial data, GraphQL errors, source failures, and GraphQL null propagation.

**Blocked by:** 05 — Batch and correlate Federation entities

**Status:** completed

## Completion criteria

- [x] Source GraphQL error paths are rewritten to client paths while independent data remains available.
- [x] A source failure attaches at the affected client location without discarding unrelated source data.
- [x] Non-null violations propagate to the nearest nullable boundary across nested objects and lists.
- [x] Completion and deterministic error ordering do not depend on source completion order.
- [x] Missing or unusable source error paths attach at the route's merge location with a safe message.
- [x] Partial-error, transport-failure, and nested-null Products-to-Reviews scenarios pass through `GatewayRuntime`.
- [x] Completion uses Caliban values and semantics; a custom indexed or raw response store requires a profile from this working path.

Ticket 6 completes merged `ResponseValue` data against the prepared Caliban fields after all source work finishes. Source errors are retained only when their paths describe client selections; gateway-authored failures use safe messages at root or entity merge locations. Completion adds missing non-null errors, propagates nulls through objects and lists, and retains deterministic plan order without introducing a second result representation.

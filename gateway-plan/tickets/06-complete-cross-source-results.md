# 06 — Complete cross-source data and errors

**Outcome:** Cross-source execution preserves partial data, GraphQL errors, source failures, and GraphQL null propagation.

**Blocked by:** 05 — Batch and correlate Federation entities

**Status:** ready-for-agent

## Completion criteria

- [ ] Source GraphQL error paths are rewritten to client paths while independent data remains available.
- [ ] A source failure attaches at the affected client location without discarding unrelated source data.
- [ ] Non-null violations propagate to the nearest nullable boundary across nested objects and lists.
- [ ] Completion and deterministic error ordering do not depend on source completion order.
- [ ] Missing or unusable source error paths attach at the route's merge location with a safe message.
- [ ] Partial-error, transport-failure, and nested-null Products-to-Reviews scenarios pass through `GatewayRuntime`.
- [ ] Completion uses Caliban values and semantics; a custom indexed or raw response store requires a profile from this working path.

# 17 — Complete the GraphQL-over-HTTP source boundary

**Outcome:** Remote execution classifies every supported GraphQL-over-HTTP response and releases transport resources exactly once.

**Blocked by:** 09 — Acquire remote schemas safely

**Status:** ready-for-agent

## Completion criteria

- [ ] Valid GraphQL envelopes win over retryable HTTP status; empty, malformed, oversized, redirected, and unsupported-media responses become typed source failures.
- [ ] Encoded requests and decoded responses have finite byte limits enforced before unbounded materialization.
- [ ] Response structure and nesting limits reject pathological JSON with bounded diagnostics.
- [ ] Outbound GraphQL request extensions are dropped unless explicitly supported.
- [ ] Safe source failures avoid raw headers, variables, bodies, and internal throwable messages.
- [ ] Request, response, and body ownership is released exactly once on success, protocol failure, size failure, timeout, and interruption.
- [ ] The exhaustive protocol matrix is tested against a real pooled backend without exposing that backend in the public interface.

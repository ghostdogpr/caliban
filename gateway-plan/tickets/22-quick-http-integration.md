# 22 — Integrate the structured gateway with Quick

**Outcome:** Users can expose the structured embedded gateway through Quick with correct request context and HTTP behavior.

**Blocked by:** 16 — Preserve mutation ordering; 21 — Close deadlines, drain, and lifecycle races

**Status:** ready-for-agent

## Completion criteria

- [ ] The ordinary structured `GatewayRuntime` is exposed through Quick without a gateway-specific server stack.
- [ ] Quick installs incoming header context, honors explicit response headers, enforces finite body limits, returns 413 for oversized bodies, and returns 405 with `Allow: POST` where required.
- [ ] GraphQL media types and status codes follow the reviewed Caliban/GraphQL-over-HTTP parity matrix.
- [ ] Existing Quick users receive an explicit compatibility decision for the finite default body limit and status changes.
- [ ] End-to-end HTTP tests cover queries, mutations, parse/validation errors, execution errors, timeouts, unsupported methods, and header forwarding.
- [ ] The adapter remains thin and delegates GraphQL semantics to `GatewayRuntime`.

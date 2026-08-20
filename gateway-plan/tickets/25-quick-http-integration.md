# 25 — Integrate the structured gateway with Quick

**Outcome:** Users can expose the structured embedded gateway through Quick with correct request context and HTTP behavior.

**Blocked by:** 16 — Preserve mutation ordering; 21 — Close deadlines, drain, and lifecycle races; 23 — Compose selected directive metadata; 24 — Secure remote errors and verify introspection control

**Status:** complete

## Completion criteria

- [x] The ordinary structured `GatewayRuntime` is exposed through Quick without a gateway-specific server stack.
- [x] Quick installs incoming header context, preserves Caliban `Configurator` and FiberRef behavior, honors explicit response headers, enforces finite body limits, returns 413 for oversized bodies, and returns 405 with `Allow: POST` where required.
- [x] GraphQL media types and status codes follow the reviewed Caliban/GraphQL-over-HTTP parity matrix.
- [x] Existing Quick users receive an explicit compatibility decision for the finite default body limit and status changes.
- [x] End-to-end HTTP tests cover queries, mutations, disabled introspection, parse/validation errors, execution errors, timeouts, unsupported methods, and header forwarding.
- [x] The adapter remains thin and delegates GraphQL semantics to `GatewayRuntime`.

## Compatibility decision

Quick now applies a configurable 1,048,576-byte request-body limit by default. Oversized bodies return `413`; mutations sent with
`GET` return `405` with `Allow: POST`; and unsupported methods return `405` with `Allow: GET, POST`. These intentional status changes
replace the previous `400` mutation response and unbounded body materialization. Requests that offer no supported response media
type return `406`, and `POST` requests with an unsupported request media type return `415`.

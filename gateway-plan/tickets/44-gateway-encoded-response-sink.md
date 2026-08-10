# 44 — Project gateway responses directly to encoded bytes

**What to build:** Adopt the core encoded capability in GatewayRuntime and add a second sink to the existing prepared-selection writer that produces caller-owned wire-ready bytes directly from the indexed response store.

**Blocked by:** 14 — Prove core response-store correctness; 43 — Add the core encoded response capability

**Status:** ready-for-agent

- [ ] GatewayRuntime adopts the encoded interpreter capability without changing its structured execution semantics.
- [ ] The encoded sink walks the same prepared client selection as the structured sink and does not build a second response tree.
- [ ] Structured and encoded results are semantically equivalent for every execution semantic available when this ticket starts, including nested/entity success, partial errors, null completion, and SourceFailure integration.
- [ ] Sink conformance scenarios are registered once and run against every installed sink, so later requirements, abstract selections, conditions, and mutations extend both paths regardless of ticket completion order.
- [ ] Returned bytes are caller-owned, bounded, and valid after request-owned source documents and buffers are released.
- [ ] Json and GraphQLResponseJson output uses the format and outcome supplied by the core capability without inspecting encoded bytes.
- [ ] Gateway v1 emits no cache-control directive and drops source top-level extensions.

# 43 — Add the gateway encoded response sink

**What to build:** Project caller-owned wire-ready bytes from the existing prepared-selection writer and indexed response store using the gateway-neutral encoded response types established in core.

**Blocked by:** 02 — Establish the shared Caliban core seams; 31 — Prove response-store correctness

**Status:** ready-for-agent

- [ ] The encoded sink walks the same prepared client selection as the structured sink and does not build a second response tree.
- [ ] Json and GraphQLResponseJson formats use their canonical UTF-8 media types.
- [ ] Encoded outcomes classify request errors, executed results, and server failures without inspecting bytes.
- [ ] Structured and encoded results are semantically equivalent for success, partial errors, SourceFailure integration, abstract types, and mutations.
- [ ] Returned bytes are caller-owned, bounded, and valid after request-owned source documents and buffers are released.
- [ ] Gateway v1 emits no cache-control directive and drops source top-level extensions.


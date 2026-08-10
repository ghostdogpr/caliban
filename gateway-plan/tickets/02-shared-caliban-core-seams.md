# 02 — Establish the shared Caliban core seams

**What to build:** Add the gateway-neutral Caliban capabilities needed by the embedded engine and later HTTP integration: bounded parsing, incoming-header context, response outcome classification, structured server/request failures, and encoded-interpreter types.

**Blocked by:** 01 — Create the fresh gateway foundation

**Status:** ready-for-agent

- [ ] Bounded parsing enforces token, nesting, and AST-node budgets without breaking existing parser APIs.
- [ ] IncomingHeaders is a validated case-insensitive multi-map with a narrowly scoped FiberRef fallback and no server-library dependency.
- [ ] GraphQL response outcomes distinguish request errors, executed results, and server failures without allowing ordinary execution errors to claim server failure.
- [ ] CalibanError.ResponseError is covered across the Scala matrix, including Caliban-owned exhaustive matches and the documented sealed-hierarchy compatibility impact.
- [ ] Encoded format, response, and interpreter types are gateway-neutral and do not introduce a Quick or gateway dependency into core.


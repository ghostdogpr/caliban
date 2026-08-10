# 15 — Harden schema and operation parsing

**What to build:** After the structured Federation MVP works end to end, add gateway-neutral finite parser limits in Caliban core and apply them to gateway schema and client-operation parsing without changing successful GraphQL semantics.

**Blocked by:** 13 — Close the structured Federation MVP

**Status:** ready-for-agent

- [ ] A source-compatible core parser overload enforces finite token, nesting, and AST-node budgets while parsing rather than after allocating an oversized document.
- [ ] Pinned SDL normalization, `check`, and request preparation use the bounded parser path with finite internal defaults.
- [ ] Ordinary valid schemas and operations produce the same documents, validation results, and prepared operations as before hardening.
- [ ] Limit failures are stable typed request or source-attributed build failures at the appropriate boundary.
- [ ] Parser limits remain gateway-neutral, introduce no dependency from core to gateway, and pass the supported Scala matrix.
- [ ] Ticket 40 remains responsible for closing the public typed configuration and auditing these defaults with every other finite bound.

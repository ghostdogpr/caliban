# 02 — Normalize pinned subgraph schemas

**What to build:** Allow code-first subgraphs backed by pinned SDL or parsed documents to be validated and normalized into one source-neutral schema and capability representation before composition, using Caliban's existing parser for the structured MVP.

**Blocked by:** 01 — Create the fresh gateway foundation

**Status:** ready-for-agent

- [ ] Pinned SDL and parsed documents normalize through the same semantic path.
- [ ] Normalization preserves original source coordinates for later diagnostics.
- [ ] Ordinary and Federation schemas enter one capability model rather than separate graph-wide modes.
- [ ] Invalid schema input produces stable source-attributed diagnostics without constructing a runtime.
- [ ] Normalization performs no schema merging, routing, or source execution.
- [ ] Parser resource hardening is left to Ticket 15 rather than being introduced as a prerequisite for composition.

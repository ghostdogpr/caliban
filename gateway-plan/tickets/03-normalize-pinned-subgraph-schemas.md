# 03 — Normalize pinned subgraph schemas

**What to build:** Allow code-first subgraphs backed by pinned SDL or parsed documents to be bounded, validated, and normalized into one source-neutral schema and capability representation before composition.

**Blocked by:** 02 — Establish the shared Caliban core seams

**Status:** ready-for-agent

- [ ] Pinned SDL and parsed documents normalize through the same bounded path.
- [ ] Normalization preserves original source coordinates for later diagnostics.
- [ ] Ordinary and Federation schemas enter one capability model rather than separate graph-wide modes.
- [ ] Invalid schema input produces stable source-attributed diagnostics without constructing a runtime.
- [ ] Normalization performs no schema merging, routing, or source execution.


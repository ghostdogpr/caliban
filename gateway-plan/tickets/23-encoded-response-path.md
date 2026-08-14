# 23 — Add the encoded response path

**Outcome:** Quick can send a caller-owned encoded gateway response without paying an avoidable structured-to-wire conversion cost.

**Blocked by:** 22 — Integrate the structured gateway with Quick

**Status:** ready-for-agent

## Completion criteria

- [ ] One generic core capability allows an interpreter to return a caller-owned encoded response without core depending on gateway.
- [ ] Gateway structured and encoded paths use the same composition, routing, execution, completion, and error semantics.
- [ ] Quick passes encoded gateway responses through without inspecting or re-encoding their bodies.
- [ ] Final encoded bytes have a finite configurable bound.
- [ ] Semantic parity tests compare structured and encoded results for nested data, entity joins, partial errors, null propagation, and mutations.
- [ ] A focused measurement demonstrates the removed conversion cost.
- [ ] The encoded implementation does not create a second gateway engine or force a custom response store without profile evidence.

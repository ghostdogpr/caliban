# 26 — Add the encoded response path

**Outcome:** Quick can send a caller-owned encoded gateway response without paying an avoidable structured-to-wire conversion cost.

**Blocked by:** 25 — Integrate the structured gateway with Quick

**Status:** complete

## Completion criteria

- [x] One generic core capability allows an interpreter to return a caller-owned encoded response without core depending on gateway.
- [x] Gateway structured and encoded paths use the same composition, routing, execution, completion, and error semantics.
- [x] Quick passes encoded gateway responses through without inspecting or re-encoding their bodies.
- [x] Final encoded bytes have a finite configurable bound.
- [x] Semantic parity tests compare structured and encoded results for nested data, entity joins, partial errors, null propagation, and mutations.
- [x] A focused measurement demonstrates the removed conversion cost.
- [x] The encoded implementation does not create a second gateway engine or force a custom response store without profile evidence.

## Implementation note

`GraphQLInterpreter.executeRequestWith` is the single internal response boundary used by ordinary structured execution and Quick's caller-owned encoded response. Quick supplies a bounded direct jsoniter encoder and receives a caller-owned, right-sized byte array; streaming responses continue through the existing structured path.

The finite 16 MB default changes oversized unary JSON responses from a successful response to an empty `500` response. Ticket 29 should include this compatibility change in the release notes.

`JsonEncodingBenchmark` compares Quick's actual bounded `GraphQLResponse` encoder with the previous materialized response envelope. A local JDK 25 run (`-prof gc -wi 2 -i 3 -w 1s -r 1s -f 1`) measured 13.79 million versus 12.41 million operations per second and 96 versus 128 allocated bytes per operation for the focused response, demonstrating the removed envelope construction cost. This is a component measurement, not an end-to-end throughput claim.

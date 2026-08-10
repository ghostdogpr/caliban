# Choose the embedded composition contract and source normalization

Type: `grilling`
Status: `resolved`
Blocked by: 01, 02

## Question

How should the embedded, code-first gateway acquire and normalize ordinary remote GraphQL, Federation, and local Caliban sources; combine their schemas; declare cross-source joins; report composition failures; and produce an executable graph without prematurely defining a serialized artifact?

## Answer

Develop the embedded gateway first. `Gateway.compose(...)` constructs an immutable Scala description, and its effectful `build` acquires schemas, validates and composes them, and returns an executable Caliban API. It must run directly in memory; no file, standalone router, or CLI is involved in this path. Remote sources support both endpoint schema acquisition and pinned SDL, while local sources obtain their schema and executor directly from a `GraphQL[R]` value.

Use one graph-level composition entry point and a heterogeneous per-source model. Federation, ordinary remote GraphQL, and local Caliban sources may coexist in one graph. Normalize every source into a shared internal source-schema model rather than selecting one graph-wide Federation-or-stitching mode. Unique root fields coexist automatically. Matching type or field names never infer a cross-source join.

Every cross-source transition requires explicit identity and recall metadata owned by the source that can resolve the target value. Federation sources translate `@key` plus `_entities` into that model. Ordinary and local sources declare a stable key and a normal GraphQL/local lookup field in Scala configuration. Keep the public lookup API ergonomic with plain names and field-selection strings; `build` parses them and validates all referenced types, fields, arguments, and result shapes against the acquired schemas.

Same-named types merge. Compatible, differently owned fields combine. Incompatible definitions fail composition. Multiple sources resolving the same field fail unless the provider is explicitly shareable; declared key fields may repeat as identity. Do not namespace types automatically. Preserve the existing final gateway-level Caliban `Transformer` capability. Keep enough original-coordinate metadata to add per-source transforms when a real use case requires them, without committing that larger surface initially.

Composition validates each source, normalizes source capabilities, merges schemas and ownership, and validates satisfiability before producing the executable graph. Failures accumulate stable, source-attributed diagnostics rather than failing at the first error. The first implementation is accurately described as **Composite-inspired heterogeneous composition**, not Composite Schemas support: it borrows key, lookup, argument mapping, explicit shareability, and satisfiability concepts while the specification remains preliminary. Comparative evidence and the minimal Composite slice are in [Hybrid composition: Federation sources plus ordinary GraphQL sources](../research/06-hybrid-composition.md).

Serialized execution packages, standalone-router loading and reload, and CLI composition are deliberately deferred until the main composition, planning, execution, and response machinery prove the in-memory model. Their format must be derived from that model rather than constrain it.

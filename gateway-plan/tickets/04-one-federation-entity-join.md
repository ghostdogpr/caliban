# 04 — Execute one Federation entity join

**Outcome:** A Products field can be extended by Reviews through one single-field Federation key and one `_entities` lookup.

**Blocked by:** 03 — Compose and execute multiple remote roots

**Status:** completed

## Completion criteria

- [x] Federation `@link`, `@key`, `_service`, and `_entities` metadata required by the scenario is recognized without exposing transport artifacts in the client schema.
- [x] The route selects the key internally, builds one representation, calls the target `_entities` field, and merges the returned fields at the original client object.
- [x] Internal key and `__typename` selections are absent from the client response unless explicitly requested.
- [x] Missing keys, unsatisfied lookups, and routing cycles fail deterministically before a source call.
- [x] `GatewayRuntime.explain` derives a deterministic semantic description from the same executable plan used for the join.
- [x] A Products-to-Reviews end-to-end test executes through `GatewayRuntime` with nested data.
- [x] The existing plan representation is deepened for dependencies rather than accompanied by a second lowered representation.

## Implementation note

`SchemaComposition` consumes each source document during gateway construction to derive Federation directive scope, field ownership, single-field keys, and entity lookup capability. The resulting `ComposedGraph` retains only the client `RootType` and routing metadata.

The private `OperationPlanner` constructs the single `OperationPlan` shared by execution and `explain`: root routes carry collision-free private key and typename selections, and entity routes retain those response names with their dependency and merge path. `RemoteGatewayRuntime` interprets that plan directly without a second lowered representation.

Federation transport metadata is removed from the client schema using resolved `@link` imports and namespaces. An authored Federation service SDL may establish lookup capability through a resolvable `@key` without declaring runtime `_entities` types and fields; an explicitly declared `_entities` field is still validated.

An entity-only Federation service document is normalized solely while constructing its internal `RootType`: the gateway supplies a private transport query root for Caliban validation, while composition continues to consume the original document, so no dummy application root is exposed. `explain(GraphQLRequest)` prepares variables and directives through the same path as execution; the string overload remains a convenience for operations without variables or extensions.

At Ticket 4 completion, the gateway deliberately handled one nullable object representation and one entity transition. A null parent skipped the lookup, while list-valued joins failed during planning. Ticket 5 subsequently added list batching and correlation, and Ticket 6 added cross-source result completion. Multi-hop joins remain Ticket 11.

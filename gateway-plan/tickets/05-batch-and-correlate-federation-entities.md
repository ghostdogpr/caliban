# 05 — Batch and correlate Federation entities

**Outcome:** The Federation path works for product lists and correlates one stable `_entities` batch back to every original object.

**Blocked by:** 04 — Execute one Federation entity join

**Status:** completed

## Completion criteria

- [x] Representations are deduplicated in stable order across all compatible entity routes, with one logical `_entities` call per compatible source/entity/key/selection group.
- [x] Results correlate back to every original object, including duplicate representations.
- [x] Null, reordered, missing, extra, or duplicate entity results have defined deterministic behavior.
- [x] Internal representations and correlation keys never appear in the client response.
- [x] The canonical Products-to-Reviews list scenario passes through `GatewayRuntime`.
- [x] Batching deepens the existing route/execution implementation rather than introducing an independent scheduler or plan form.

## Implementation note

Compatible entity routes are grouped in plan order by target, entity type, key, and normalized downstream selection. Each group walks its dependency responses in client order and builds one request-local batch. A linked hash map preserves the first occurrence of every typename/key representation while retaining every route and client response path for fan-out. `EntityExecutor` sends that stable unique list through one `_entities` request, and response assembly applies the correlated patch at each retained path.

The gateway requests collision-free private key and typename aliases from the entity source. Non-null results are correlated by that identity, so result order does not affect the client response. Null results resolve the representation at the same batch position without adding a patch. For malformed responses, the first result wins; duplicate and extra results are ignored with deterministic gateway errors, while every missing result produces an error at each affected client location. Subgraph entity errors are likewise fanned out to duplicate locations. Final projection is still driven only by client fields, so source keys, representations, and correlation aliases remain internal.

The private `EntityExecutor` owns grouping, batch preparation, concrete Federation lookup execution, correlation, and entity error relocation behind one execution interface. `RemoteGatewayRuntime` retains orchestration, root execution, final merging and projection, introspection, and explanation. Ticket 5 adds neither a scheduler, another plan representation, nor a lookup protocol abstraction.

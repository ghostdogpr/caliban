# 09 — Acquire remote schemas safely

**Outcome:** Applications may build ordinary and Federation subgraphs from their remote endpoints instead of pinning schema text.

**Blocked by:** 02 — Execute one pinned remote graph end to end

**Status:** completed

## Completion criteria

- [x] Ordinary sources acquire schemas through introspection and Federation sources through `_service { sdl }`.
- [x] Pinned and acquired schemas enter the same composition path and produce the same graph semantics.
- [x] Each acquisition has finite independent time, response-byte, and parsing limits.
- [x] Redirects are disabled by default and static acquisition headers cannot override protocol-owned headers.
- [x] Acquisition resources are gateway-owned and released on success, failure, timeout, and interruption.
- [x] Failures accumulate as source-attributed composition diagnostics without constructing a partial runtime.
- [x] Tests cover concurrent acquisition, one failure among valid siblings, and cleanup.

## Implementation note

`RemoteSchemaAcquisition` is the single private acquisition seam. Ordinary introspection and Federation `_service` acquisition both produce a parsed `Document`, which immediately enters the same normalization and composition path as pinned schema input. Acquisitions share the gateway-owned pooled backend, run concurrently, and apply independent timeout, response-byte, and pre-parse nesting bounds.

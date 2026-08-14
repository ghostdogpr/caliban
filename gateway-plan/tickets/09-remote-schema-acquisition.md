# 09 — Acquire remote schemas safely

**Outcome:** Applications may build ordinary and Federation subgraphs from their remote endpoints instead of pinning schema text.

**Blocked by:** 02 — Execute one pinned remote graph end to end

**Status:** ready-for-agent

## Completion criteria

- [ ] Ordinary sources acquire schemas through introspection and Federation sources through `_service { sdl }`.
- [ ] Pinned and acquired schemas enter the same composition path and produce the same graph semantics.
- [ ] Each acquisition has finite independent time, response-byte, and parsing limits.
- [ ] Redirects are disabled by default and static acquisition headers cannot override protocol-owned headers.
- [ ] Acquisition resources are gateway-owned and released on success, failure, timeout, and interruption.
- [ ] Failures accumulate as source-attributed composition diagnostics without constructing a partial runtime.
- [ ] Tests cover concurrent acquisition, one failure among valid siblings, and cleanup.

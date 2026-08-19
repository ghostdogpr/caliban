# 12 — Support requirements and provided fields

**Outcome:** Federation `@requires` and `@provides` influence routing and internal selection without changing the client schema incorrectly.

**Blocked by:** 11 — Support key and multi-hop routing breadth

**Status:** completed

## Completion criteria

- [x] Field-set parsing covers nested fields and fragments used by supported Federation versions.
- [x] Requirements are gathered recursively before dependent source calls and are never projected unless requested.
- [x] Provided fields satisfy downstream work only within the scope where they are promised.
- [x] Requirement chains, conflicts, and cycles have deterministic planning behavior.
- [x] Argument-bearing requirements supported by the selected Federation baseline preserve argument semantics.
- [x] Relevant audit groups and focused partial-failure tests pass through `GatewayRuntime`.
- [x] The pinned audit is rerun without regressions or new deferrals.

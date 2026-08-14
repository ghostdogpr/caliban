# 12 — Support requirements and provided fields

**Outcome:** Federation `@requires` and `@provides` influence routing and internal selection without changing the client schema incorrectly.

**Blocked by:** 11 — Support key and multi-hop routing breadth

**Status:** ready-for-agent

## Completion criteria

- [ ] Field-set parsing covers nested fields and fragments used by supported Federation versions.
- [ ] Requirements are gathered recursively before dependent source calls and are never projected unless requested.
- [ ] Provided fields satisfy downstream work only within the scope where they are promised.
- [ ] Requirement chains, conflicts, and cycles have deterministic planning behavior.
- [ ] Argument-bearing requirements supported by the selected Federation baseline preserve argument semantics.
- [ ] Relevant audit groups and focused partial-failure tests pass through `GatewayRuntime`.

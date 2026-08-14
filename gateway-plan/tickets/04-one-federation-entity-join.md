# 04 — Execute one Federation entity join

**Outcome:** A Products field can be extended by Reviews through one single-field Federation key and one `_entities` lookup.

**Blocked by:** 03 — Compose and execute multiple remote roots

**Status:** ready-for-agent

## Completion criteria

- [ ] Federation `@link`, `@key`, `_service`, and `_entities` metadata required by the scenario is recognized without exposing transport artifacts in the client schema.
- [ ] The route selects the key internally, builds one representation, calls the target `_entities` field, and merges the returned fields at the original client object.
- [ ] Internal key and `__typename` selections are absent from the client response unless explicitly requested.
- [ ] Missing keys, unsatisfied lookups, and routing cycles fail deterministically before a source call.
- [ ] `GatewayRuntime.explain` derives a deterministic semantic description from the same executable plan used for the join.
- [ ] A Products-to-Reviews end-to-end test executes through `GatewayRuntime` with nested data.
- [ ] The existing plan representation is deepened for dependencies rather than accompanied by a second lowered representation.

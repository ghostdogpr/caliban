# 05 — Batch and correlate Federation entities

**Outcome:** The Federation path works for product lists and correlates one stable `_entities` batch back to every original object.

**Blocked by:** 04 — Execute one Federation entity join

**Status:** ready-for-agent

## Completion criteria

- [ ] Entity representations are deduplicated per operation and sent in stable order through one logical `_entities` call.
- [ ] Results correlate back to every original object, including duplicate representations.
- [ ] Null, reordered, missing, extra, or duplicate entity results have defined deterministic behavior.
- [ ] Internal representations and correlation keys never appear in the client response.
- [ ] The canonical Products-to-Reviews list scenario passes through `GatewayRuntime`.
- [ ] Batching deepens the existing route/execution implementation rather than introducing an independent scheduler or plan form.

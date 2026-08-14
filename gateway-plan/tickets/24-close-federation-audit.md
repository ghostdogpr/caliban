# 24 — Close the Federation Gateway Audit

**Outcome:** Every in-scope case in the selected current Federation Gateway Audit passes through the native code-first gateway.

**Blocked by:** 12 — Support requirements and provided fields; 15 — Execute abstract and conditional selections; 16 — Preserve mutation ordering

**Status:** ready-for-agent

## Completion criteria

- [ ] The selected upstream revision is refreshed through an explicit reviewed version-file change.
- [ ] All in-scope cases pass against the real gateway composition and execution path.
- [ ] Exclusions are limited to explicitly deferred features or invalid/ambiguous fixtures and record their rationale.
- [ ] Failures discovered here are fixed in the owning deep module rather than patched in the audit adapter.
- [ ] Project tests retain focused coverage for important error details the upstream audit does not assert.
- [ ] The run manifest records revision, supported cases, exclusions, Scala version, and environment.

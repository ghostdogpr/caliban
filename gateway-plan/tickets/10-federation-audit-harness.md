# 10 — Integrate the Federation Gateway Audit

**Outcome:** The current Federation Gateway Audit can execute against the code-first embedded gateway and report supported, failing, and explicitly deferred cases.

**Blocked by:** 06 — Complete cross-source data and errors

**Status:** ready-for-agent

## Completion criteria

- [ ] A non-published compatibility project adapts upstream subgraphs to `Gateway.compose` without a serialized supergraph artifact.
- [ ] The selected current upstream revision is recorded in one checked-in version file read by path-filtered CI.
- [ ] Upstream suite and case identities are preserved in reports.
- [ ] The already supported basic entity cases pass through the real gateway runtime.
- [ ] Remaining failures are grouped into the breadth tickets that own their semantics; no assertion failure is hidden as flaky.
- [ ] Only genuinely deferred features or invalid fixtures have documented exclusions.

# 10 — Integrate the Federation Gateway Audit

**Outcome:** The current Federation Gateway Audit can execute against the code-first embedded gateway and report supported, failing, and explicitly deferred cases.

**Blocked by:** 06 — Complete cross-source data and errors

**Status:** completed

## Completion criteria

- [x] A non-published compatibility project adapts upstream subgraphs to `Gateway.compose` without a serialized supergraph artifact.
- [x] The selected current upstream revision is recorded in one checked-in version file read by path-filtered CI.
- [x] Upstream suite and case identities are preserved in reports.
- [x] The already supported basic entity cases pass through the real gateway runtime.
- [x] Remaining failures are grouped into the breadth tickets that own their semantics; no assertion failure is hidden as flaky.
- [x] Only genuinely deferred features or invalid fixtures have documented exclusions.

## Implementation notes

- The non-published `gatewayAudit` project serves each upstream suite through native `Gateway.compose` inputs fetched
  from the audit fixture server.
- `gateway-audit/upstream.env` pins the upstream repository and revision. Path-filtered CI runs the upstream audit,
  preserves its raw result and logs, and generates a case-level disposition report.
- The pinned revision currently reports 199 cases: 17 pass and 182 fail. Every passing case identity is protected as
  a regression baseline; the remaining results stay visible under their owning breadth ticket or the documented
  Ticket 24 deferral.

# 34 — Close compatibility and confirm publication matrix

**What to build:** Close every in-scope compatibility case and re-confirm the real planner/executor against the publishable Scala matrix before enabling gateway publication.

**Blocked by:** 31 — Prove response-store correctness; 32 — Execute ordered mutations; 33 — Integrate the Federation Gateway Audit

**Status:** ready-for-agent

- [ ] Every in-scope case in the pinned audit revision passes through code-first composition.
- [ ] Only explicitly staged features remain excluded and every exclusion has a reviewed reason.
- [ ] Structured execution parity covers success, partial error, source failure, abstract types, and mutation ordering.
- [ ] The full gateway compile/test matrix runs against the real planner, dense DAG, response store, and source-call generation.
- [ ] Any matrix narrowing has concrete language or dependency evidence recorded before publication.
- [ ] Publication is enabled only for the confirmed supported versions.


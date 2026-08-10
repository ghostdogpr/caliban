# 34 — Close compatibility and confirm publication matrix

**What to build:** Run the complete in-scope compatibility gate, require separately scoped blockers for any newly exposed product defect, and confirm the candidate Scala publication matrix while keeping publication disabled until final release closure.

**Blocked by:** 14 — Prove core response-store correctness; 32 — Execute ordered mutations; 33 — Integrate the Federation Gateway Audit

**Status:** ready-for-agent

- [ ] Every in-scope case in the pinned audit revision passes through code-first composition; a newly exposed implementation gap blocks this gate through a context-sized follow-up ticket rather than expanding Ticket 34.
- [ ] Only explicitly staged features remain excluded and every exclusion has a reviewed reason.
- [ ] Structured execution parity covers success, partial error, source failure, abstract types, and mutation ordering.
- [ ] The full gateway compile/test matrix runs against the real planner, dense DAG, response store, and source-call generation.
- [ ] Any matrix narrowing has concrete language or dependency evidence recorded before publication.
- [ ] The confirmed supported versions are recorded, but publication remains disabled for Ticket 53 to enable only after every release gate passes.

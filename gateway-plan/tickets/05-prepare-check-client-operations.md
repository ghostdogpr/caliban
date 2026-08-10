# 05 — Prepare and check client operations

**What to build:** Implement the bounded, variable-independent operation frontend against the composed client schema, including the inherited check behavior and the prepared-operation boundary used by planning.

**Blocked by:** 02 — Establish the shared Caliban core seams; 04 — Compose the client schema and root topology

**Status:** ready-for-agent

- [ ] check performs bounded parsing and configured whole-document static validation only.
- [ ] Execution preparation selects an operation, validates it, normalizes fragments/directives/selections, and compiles variable and argument expressions.
- [ ] Prepared operations contain no concrete request variables and remain owned by one graph generation.
- [ ] Mandatory gateway structural and type checks cannot be bypassed by skip-validation settings.
- [ ] Operation selection, validation, and structural failures use the intended request-error classifications.


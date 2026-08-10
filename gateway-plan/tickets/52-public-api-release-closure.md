# 52 — Close the public API and release

**What to build:** Perform the final public compatibility review and clean-checkout release verification across gateway, core, Quick, external suites, tracing, and documentation.

**Blocked by:** 34 — Close compatibility and confirm publication matrix; 42 — Close operational race and interruption testing; 49 — Verify the useful-throughput gate; 50 — Add optional OpenTelemetry integration; 51 — Write examples and migration documentation

**Status:** ready-for-agent

- [ ] The final public API preserves the approved module direction, contravariant environments, private representations, immutable configuration, and no low-level execution internals.
- [ ] MiMa establishes the first gateway baseline appropriately and cross-matrix core compatibility checks include ResponseError.
- [ ] Final limit auditing explicitly verifies that Quick request-body bytes and gateway encoded-output bytes are always finite and cannot be disabled.
- [ ] External audit and benchmark revisions are explicitly refreshed through the committed version mechanism and run manifests remain reproducible.
- [ ] Clean-checkout compatibility, operational, benchmark, tracing, documentation/example, API, and release suites pass.
- [ ] No standalone router, CLI, serialized artifact, streaming execution, alternate protocol, or other deferred feature has leaked into the initial release.


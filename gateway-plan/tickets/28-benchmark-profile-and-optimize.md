# 28 — Benchmark, profile, and optimize the real gateway

**Outcome:** The production Quick path is measured against current gateways, and measured bottlenecks are optimized without splitting semantics.

**Blocked by:** 26 — Add the encoded response path; 27 — Improve ambiguous-route planning

**Status:** in progress

## Completion criteria

- [x] A non-published project integrates the current GraphQL Gateways Benchmark and records its revision in the shared compatibility version file.
- [x] The published Hive > Grafbase > Cosmo > Apollo ordering is reproduced locally before Caliban is compared on the same pinned
      operation and both upstream load scenarios; unavoidable artifact substitutions are recorded explicitly.
- [x] Apollo Router, Hive Router, Grafbase, Cosmo Router, and Caliban use the published revision's configuration contract; the
      missing Hot Chocolate Fusion adapter is recorded explicitly.
- [x] The pinned upstream driver and configurations run unmodified; reported throughput uses measured scenario iterations so setup
      traffic is excluded.
- [x] Profiles record throughput, latency, CPU, allocation, GC, and memory for the real Quick path.
- [x] The dominant actionable seam is optimized behind its existing interface; specialized plans or response storage are introduced only if this evidence selects them.
- [x] Semantic, audit, lifecycle, planner-quality, and structured-versus-encoded tests remain green after optimization.
- [ ] Useful throughput reaches the standing target or an evidence-backed expiring maintainer exception records the remaining gap.

## Optimization evidence

A JFR profile of the production Quick path places the dominant controllable cost in response assembly rather than remote-call
coordination. Immutable map construction and hashing, object completion, value merging, byte-array copying, and young-generation
collection dominate the sampled CPU and allocation stacks. The in-flight query table is not a sampled hot spot.

Gateway assembly performed a recursive projection and then recursively completed the projected value. Completion already selects the
client fields and preserves runtime-type evidence, so the projection pass was removed. The gateway semantic suites pass with this
single-pass assembly, and an adjacent warmed A/B improved constant-load throughput from 335.6 to 347.5 iterations/s (3.5%). A second
ordered-field lookup experiment was removed because reverse-order A/B measurements did not confirm a gain.

The remaining gap to the standing target is not closed and no maintainer exception has been recorded, so this ticket remains in
progress. Benchmark integration, the measurement contract, configuration provenance, and reproduction commands remain in
`gateway-benchmark`; machine-specific profile artifacts are deliberately not checked in.

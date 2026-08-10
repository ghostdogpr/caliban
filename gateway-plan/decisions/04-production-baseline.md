# Which operational and extensibility capabilities form the production baseline?

Type: `research`
Status: `resolved`

## Question

Across the pinned gateways, what is the majority production baseline for configuration, artifact delivery, health and readiness, graceful shutdown, observability, header propagation, timeouts, cancellation, retries, persisted operations, limits, extension hooks, and deployment packaging? Separate capabilities that belong in the core engine, the standalone router host, optional modules, or later milestones.

## Answer

Make cancellation/deadlines, immutable generation lifecycle, layered safety limits, low-overhead instrumentation events, typed coarse extension hooks, and the operation-resolution seam engine responsibilities. Make strict file/env/CLI configuration, artifact acquisition and last-known-good reload, probes, signal/drain handling, exporters, HTTP admission, and JVM/OCI packaging standalone-host responsibilities. Ship persisted operations/APQ, artifact providers, and conservative remote-GraphQL resilience as optional v1 modules; defer dynamic/external plugins, control-plane product work, response caching, rate/authorization policy, full traffic shaping, and Helm/operator polish. All four gateways support explicit deadlines, layered limits, observability, header policy, persisted operations, and extensions, but only Cosmo exposes broad execution retries, so request retries are opt-in, replay-safe, deadline-bound, and disabled by default; mutations are never retried implicitly. Full comparison, cited evidence, placement, and acceptance invariants: [Production operational baseline and subsystem placement](../research/04-production-baseline.md).

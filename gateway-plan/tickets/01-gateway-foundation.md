# 01 — Keep the gateway foundation

**Outcome:** Retain the reviewed unpublished gateway module and the minimal public `Gateway`/`GatewayRuntime` foundation as the base of the new implementation.

**Blocked by:** None

**Status:** completed

## Completion criteria

- [x] The gateway module compiles on the provisional JVM Scala matrix and publication remains disabled.
- [x] `Gateway` and `GatewayRuntime` are contravariant in their environment and keep their representations private.
- [x] Gateway internals depend on existing Caliban modules; core does not depend on gateway.
- [x] No source, planner, response-store, transport, Quick, or tracing abstraction exists in the foundation without executable behavior.
